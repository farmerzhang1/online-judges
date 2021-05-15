object StepInPrimes {

  def step(g: Int, m: Long, n: Long): String =
    (m+g to n)
      .collectFirst { case p if BigInt(p).isProbablePrime(4) && BigInt(p - g).isProbablePrime(4) => s"${(p-g,p)}" }
      .getOrElse("")
}

// there is the miller-rabin prime test, gonna write it
