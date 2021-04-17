object Kata {
    def cleanString(s: String): String = {
        //感覺我會把他很醜陋地寫出來（（（
        // well, maybe not
        (s.toList).foldLeft ("") {(str, c) => if (c == '#') str take (str.length-1) else str + c}
    }
}