object Solution {
    def isValidWalk(walk: Seq[Char]): Boolean = {
        // your code here
        var leftright : Int = 0
        var updown : Int = 0
        for (c <- walk) {
        c match {
            case 'w' => (leftright = leftright - 1)
            case 'e' => (leftright = leftright + 1)
            case 'n' => (updown = updown + 1)
            case 's' => (updown = updown - 1)
        }}
        walk.length == 10 && leftright == 0 && updown == 0
    }
}