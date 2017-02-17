class Phrase(phrase: String) {
  def wordCount: Map[String, Int] = {

    def cleanValues(item: String): String = {
      item.replaceAll("[^a-zA-Z0-9\\']", "").toLowerCase()
    }

    val arrayItems = phrase
      .split(",| ")
      .map { f =>
        cleanValues(f)
      }
      .filterNot(_.isEmpty)

    arrayItems.groupBy(identity).collect {
      case (a, b) => (a.toString, b.size)
    }
  }
}
