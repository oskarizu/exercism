object Hamming {
  def compute(first: String, second: String): Option[Int]= {
    val isComparable: Boolean = first.length == second.length

    def getHammingDistance: Int = {
      first.toList.zip(second.toList).count( x => x._1 != x._2)
    }

    isComparable match {
      case false => None
      case true => Some(getHammingDistance)
    }
  }
}
