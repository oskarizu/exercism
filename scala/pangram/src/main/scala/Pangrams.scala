object Pangrams {
  def isPangram(value: String): Boolean = {

    def prepareInput(input: String): Set[Char] = {
      val listChars = input.toLowerCase.toList
      listChars.filter(_.isLetterOrDigit).toSet
    }

    val alphabet = ('a' to 'z').toSet
    val sentence = prepareInput(value)

    alphabet.diff(sentence).isEmpty
  }
}
