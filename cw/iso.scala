object Isomorphism {
    /**
    * The type [[Nothing]] has no value.
    * So it is impossible to construct an instance of it.
    * In this solution, wherever a situation arises where
    * for types to check, you need a function that takes a [[Nothing]],
    * you can use [[absurd]].
    */
    def absurd[R](n: Nothing): R = n // [[Nothing]] is subtype of everything, so I can return a n as type [[R]]

    // so, when are two type, `A` and `B`, considered equal?
    // a definition might be, it is possible to go from `A` to `B`,
    // and from `B` to `A`.
    // Going a roundway trip should leave you the same value.
    // Unfortunately it is virtually impossible to test this in Scala.
    // This is called Isomorphism.

    type ISO[A, B] = (A => B, B => A)

    // given ISO a b, we can go from a to b
    def substL[A, B]: ISO[A, B] => (A => B) = _._1

    // and vice versa
    def substR[A, B]: ISO[A, B] => (B => A) = _._2

    // There can be more than one ISO a b
    def isoBool: ISO[Boolean, Boolean] = (identity, identity)
    def isoBoolNot: ISO[Boolean, Boolean] = (! _, ! _)

    // isomorphism is reflexive
    def refl[A]: ISO[A, A] = (identity, identity)

    // isomorphism is symmetric
    def symm[A, B]: ISO[A, B] => ISO[B, A] = {
        case (ab, ba) => (ba, ab)
    }

    // isomorphism is transitive
    def trans[A, B, C]: (ISO[A, B], ISO[B, C]) => ISO[A, C] = {
        case ((ab, ba), (bc, cb)) => ((ab andThen bc), (cb andThen ba))
    }

    // We can combine isomorphism:
    def isoTuple[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A, C), (B, D)] = {
        case ((ab, ba), (cd, dc)) => (
            {case (a, c) => (ab(a), cd(c))},
            {case (b, d) => (ba(b), dc(d))})
    }
    // isn't this monad ? seems familiar to me
    def isoList[A, B]: ISO[A, B] => ISO[List[A], List[B]] = {
        case (ab, ba) => {
            (la => la map ab, lb => lb map ba)
        }
    }
    def isoOption[A, B]: ISO[A, B] => ISO[Option[A], Option[B]] = {
        case (ab, ba) => {
            (oa => oa map ab, ob => ob map ba)
        }
    }
    def isoEither[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[Either[A, C], Either[B, D]] = {
        case ((ab, ba), (cd, dc)) =>
            (
                eac => { eac match {
                    case Left(a) => Left(ab (a))
                    case Right(c) => Right(cd (c))
                }},
                ebd => { ebd match {
                    case Left(b) => Left(ba (b))
                    case Right(d) => Right(dc (d))
                }}
            )

    }
    def isoFunc[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A => C), (B => D)] = {
        case ((ab, ba), (cd, dc)) => {
            ((ac => ba andThen ac andThen cd), (bd => ab andThen bd andThen dc))
        }
    }

    // Going another way is hard (and is generally impossible)
    // 一一對應！！
    // 好！
    def isoUnOption[A, B]: ISO[Option[A], Option[B]] => ISO[A, B] = {
        case (oab, oba) => {
            // def getoa (b : B) : Option[A] = oba(Some (b))
            // def getob (a : A) : Option[B] = oab(Some (a))
            // (absurd[B], absurd)
            ((a => oab(Some(a)).getOrElse(oab (None).get)), (b => oba(Some(b)).getOrElse(oba (None).get)))
        }
    }
    // Remember, for all valid ISO, converting and converting back
    // Is the same as the original value.
    // You need this to prove some case are impossible.

    // We cannot have
    // isoUnEither[A, B, C, D]: (ISO[Either[A, B], Either[C, D]], ISO[A, C]) => ISO[B, D]
    // Note that we have
    // 我直接哭出來
    def isoEU: ISO[Either[List[Unit], Unit], Either[List[Unit], Nothing]] = {
        ({
            case Left(units) => Left(()::units)
            case Right(u) => Left(List())
        },
        {
            case Left(()::units) => Left(units)
            case Left(Nil) => Right(())
            case Right(u) => Right(absurd (u))
        })

        // ({case eluu => {
        //     eluu match {
        //         case Right(u) => Left (List(u))
        //         case _ => eluu
        //     }
        // }}, {case elun => {
        //     elun match {
        //         case Left(units) => Left(units)
        //         case n => Right(absurd[Unit] (n))
        //     }
        // }}
        // )
    }
    // where Unit, has 1 value, (the value is also called Unit), and Void (also called Nothing) has 0 values.
    // If we have isoUnEither,
    // We have ISO[Unit, Nothing] by calling isoUnEither isoEU
    // That is impossible, since we can get a Nothing by substL on ISO[Unit, Nothing]
    // So it is impossible to have isoUnEither

    // And we have isomorphism on isomorphism!
    def isoSymm[A, B]: ISO[ISO[A, B], ISO[B, A]] = {
        ({case (ab, ba) => (ba, ab)}, {case (ba, ab) => (ab, ba)})
    }
}


/*
and these are tests

import org.scalatest._

class IsomorphismSpec extends FlatSpec with Matchers {
  import Isomorphism._
  val bISO: ISO[Boolean, Boolean] = (!_, !_)
  def lrl[A, B](iso: ISO[A, B]): A => A =
    a => substR(iso)(substL(iso)(a))

  "substL" should "get A => B" in {
    substL(bISO)(true) shouldBe false
    substL(bISO)(false) shouldBe true
    substL(isoBool)(false) shouldBe false
    substL(isoBool)(true) shouldBe true
  }

  "substR" should "get B => A" in {
    substR(bISO)(true) shouldBe false
    substR(bISO)(false) shouldBe true
    substR(isoBool)(false) shouldBe false
    substR(isoBool)(true) shouldBe true
  }
    (ISO[Either[List[Unit], Unit], Either[List[Unit], Nothing]])

  "combining isomorphisms" should "work with isoEU" in {
    substL(isoEU)(Right(())).isLeft shouldBe true
    for (i <- 1 to 10) {
      val lst = List.fill(i)(())
      lrl(isoEU)(Left(lst)) shouldBe Left(lst)
    }
  }
}
*/