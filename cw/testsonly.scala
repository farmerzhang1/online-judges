object test{
    def absurd[R](n: Nothing): R = n // [[Nothing]] is subtype of everything, so I can return a n as type [[R]]
}




object Isomorphism {
  //好難啊！！
/**
 * The type [[Nothing]] has no value.
 * So it is impossible to construct an instance of it.
 * In this solution, wherever a situation arises where
 * for types to check, you need a function that takes a [[Nothing]],
 * you can use [[absurd]].
 */
def absurd[R](n: Nothing): R = ???

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
def substR[A, B]: ISO[A, B] => (B => A) = ???

// There can be more than one ISO a b
def isoBool: ISO[Boolean, Boolean] = (identity, identity)
def isoBoolNot: ISO[Boolean, Boolean] = (! _, ! _)

// isomorphism is reflexive
def refl[A]: ISO[A, A] = ???

// isomorphism is symmetric
def symm[A, B]: ISO[A, B] => ISO[B, A] = ???

// isomorphism is transitive
def trans[A, B, C]: (ISO[A, B], ISO[B, C]) => ISO[A, C] = ???

// We can combine isomorphism:
def isoTuple[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A, C), (B, D)] = {
  case ((ab, ba), (cd, dc)) => {
    (a, c) => (ab(a), cd(c)), ???
  }
}

def isoList[A, B]: ISO[A, B] => ISO[List[A], List[B]] = ???
def isoOption[A, B]: ISO[A, B] => ISO[Option[A], Option[B]] = ???
def isoEither[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[Either[A, C], Either[B, D]] = ???
def isoFunc[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A => C), (B => D)] = ???

// Going another way is hard (and is generally impossible)
def isoUnOption[A, B]: ISO[Option[A], Option[B]] => ISO[A, B] = ???
// Remember, for all valid ISO, converting and converting back
// Is the same as the original value.
// You need this to prove some case are impossible.

// We cannot have
// isoUnEither[A, B, C, D]: (ISO[Either[A, B], Either[C, D]], ISO[A, C]) => ISO[B, D]
// Note that we have
def isoEU: ISO[Either[List[Unit], Unit], Either[List[Unit], Nothing]] = ???
// where Unit, has 1 value, (the value is also called Unit), and Void has 0 values.
// If we have isoUnEither,
// We have ISO[Unit, Nothing] by calling isoUnEither isoEU
// That is impossible, since we can get a Nothing by substL on ISO[Unit, Nothing]
// So it is impossible to have isoUnEither

// And we have isomorphism on isomorphism!
def isoSymm[A, B]: ISO[ISO[A, B], ISO[B, A]] = ???
}