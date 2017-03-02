class DNA(dna: String) {

  val Nucleotid = "[ACGT]+".r
  val NotANucleotid = "[^ACGT]+".r

  private def validateDna(dna: String): Boolean = {
    dna.matches(Nucleotid.toString) | dna.isEmpty
  }

  private def validateNucleotid(char: Char): Option[String] = {
    Nucleotid.findFirstIn(char.toString)
  }

  private def findImposterNucleotid: String = {
    val imposter = NotANucleotid.findAllIn(dna).mkString
    s"invalid nucleotide '$imposter'"
  }

  def count(char: Char): Either[String, Int] = {
    validateDna(dna) match {
      case true => {
        validateNucleotid(char) match {
          case None => Left(s"invalid nucleotide '$char'")
          case Some(v) => Right(dna.count(_ == char))
        }
      }
      case _ => Left(findImposterNucleotid)
    }
  }

  private def nucleotidesMap: Map[Char, Int] = {
    Map(
      'A' -> count('A').right.get,
      'C' -> count('C').right.get,
      'G' -> count('G').right.get,
      'T' -> count('T').right.get
    )
  }

  def nucleotideCounts: Either[String, Map[Char, Int]] = {
    validateDna(dna) match {
      case true => Right(nucleotidesMap)
      case _ => Left(findImposterNucleotid)
    }
  }
}
