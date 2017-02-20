class Anagram(value: String) {
  def matches(candidates: Seq[String]): Seq[String] = {
    val sortedValue = value.toLowerCase.sorted

    def cleanValues(value: String): String = value.toLowerCase.sorted

    val candidatesMap = candidates.map(c => c -> cleanValues(c))

    candidatesMap
      .filter(_._1.toLowerCase != value.toLowerCase)
      .filter(_._2 == sortedValue).map(_._1)
  }
}
