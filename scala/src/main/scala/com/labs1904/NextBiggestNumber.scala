package com.labs1904

object NextBiggestNumber {

  def main(args: Array[String]): Unit = {

    val input = 67809
    val nextBiggestNumber = getNextBiggestNumber(input)
    println(s"Input: $input")
    println(s"Next biggest number: $nextBiggestNumber")
  }

  def permutationsOfInt(i: Integer): List[Int] = {
    val s = i.toString
    val a = s.split("")
    a.permutations.toList.map(_.foldLeft("")((agg, next) => {
      agg + next
    })).map(_.toInt)
  }

  def getNextBiggestNumber(i: Integer): Int = {
    val perms = permutationsOfInt(i)

    perms.foldLeft(-1)((agg, next) => {
      if((next < agg || agg == -1) && next > i){
        next
      } else {
        agg
      }
    })
  }
}
