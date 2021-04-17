import scala.math._
object StepInPrimes {
    def step(g: Int, m: Long, n: Long): String = {
        // your code
        if (m >= n) ""
        else if (is_prime (m) && is_prime (m + g)) "what"
        else step (g, next_prime (m), n)
    }

    def is_prime (p : Long) : Boolean = {
        (2 until sqrt(p).toInt) forall (p % _ != 0)
    }

    def next_prime (p : Long) : Long = {
        if (p % 2 == 0) next_prime (p - 1)
        else {
        if (is_prime (p+2)) p+2
        else next_prime (p+2)}
    }
}