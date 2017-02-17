class Bob {
  def hey(question: String): String = {
    val Question = """(.*[a-z]\?$|.*[0-9]\?$|[A-Z]\?$)""".r
    val Shouting = """(.*[A-Z]\!$|.*[A-Z]\?$|.*[A-Z])""".r
    val Politics = """(.*\s+|.*\A\z)""".r

    question match {
      case Question(s) => "Sure."
      case Shouting(s) => "Whoa, chill out!"
      case Politics(s) => "Fine. Be that way!"
      case _ => "Whatever."
    }
  }
}
