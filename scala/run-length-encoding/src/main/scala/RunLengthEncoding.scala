object RunLengthEncoding {

  def encode(str: String): String = {
    val listStr = str.toList

    def groupEquals(vals: List[Char]): List[List[Char]] = vals match {
      case Nil => Nil
      case x :: xs1 =>
        val (first, rest) = vals span (y => y == x )
        first :: groupEquals(rest)
    }

    groupEquals(listStr).map( c =>
      if (c.length == 1) { s"${c.head}" } else {
        s"${c.length}${c.head}"
      }).mkString

  }

  def decode(str: String): String = {
    val listStr = str.toList

    def explode(vals: List[Char]): List[Char] = {
      vals match {
        case Nil => Nil
        case (a :: b) => {
          val (mult, x :: xs1) = vals span (_.isDigit)
          if (mult.isEmpty) x::explode(xs1)
          else List.fill(mult.mkString.toInt)(x) ::: explode(xs1)
        }
      }
    }

    explode(listStr).mkString
  }
}
