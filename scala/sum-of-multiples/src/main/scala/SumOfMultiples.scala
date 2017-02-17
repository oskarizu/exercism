object SumOfMultiples {
  def sumOfMultiples(factors: Set[Int], limit: Int): Int = {
    val startGroup = 1 until limit

    def checkMultiple(value: Int, check: Int): Int = {
      if (value % check == 0) { value } else { 0 }
    }

    def checkFactor(factor: Int): Set[Int] = {
      val reducedList =
        startGroup.map(checkMultiple(_, factor)).filter(_ != 0)
      reducedList.toSet
    }
    factors.flatMap(checkFactor(_)).sum
  }
}
