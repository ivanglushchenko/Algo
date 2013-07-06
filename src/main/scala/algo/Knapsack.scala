package main.scala.algo

import scala.collection.immutable.Set

class Knapsack(header: String, body: List[String]) {
  val (n, k) = Utils.toTwoInts(header)
  val items = Utils.mapi(body filter(!_.isEmpty), (i, el: String) => (i + 1, Utils.toTwoInts(el)))

  def solve() {
    def loop(prevSln: Array[(Int, List[Int])], items: List[(Int, (Int, Int))]): Array[(Int, List[Int])] = items match {
      case (n, (v, w)) :: tl =>
        val nextSln = Array.ofDim[(Int, List[Int])](k + 1)
        for (i <- 0 to k) {
          if (i < w || prevSln(i)._1 >= (v + prevSln(i - w)._1)) nextSln(i) = prevSln(i)
          else nextSln(i) = (v + prevSln(i - w)._1, n :: prevSln(i - w)._2)
        }
        loop(nextSln, tl)
      case List() => prevSln
    }

    val initialSolution = Array.fill(k + 1)(0, List[Int]())
    val (_, selectedItems) = loop(initialSolution, items)(k)
    val set = Set() ++ selectedItems
    val values = items map (i => (i._1, i._2._1)) toMap
    val selectedValue = selectedItems map (values(_)) sum
    val selectedIndicators = for (i <- 0 until n) yield if (set.contains(i)) "1" else "0"

    println("" + selectedValue + " 0")
    println(selectedIndicators mkString " ")
  }

  def solveNoBacktrack() {
    def loop(prevSln: Array[Int], items: List[(Int, (Int, Int))], bp: Map[Int, Int]): (Array[Int], Map[Int, Int]) = items match {
      case (n, (v, w)) :: tl =>
        //println("processing " + n)
        val nextSln = Array.ofDim[Int](k + 1)
        var nextBp = bp
        for (i <- 0 to k) {
          if (i < w || prevSln(i) >= (v + prevSln(i - w))) nextSln(i) = prevSln(i)
          else {
            nextSln(i) = v + prevSln(i - w)
            nextBp = nextBp + (nextSln(i) -> n)
          }
        }
        loop(nextSln, tl, nextBp)
      case _ => (prevSln, bp)
    }

    val initialSolution = Array.fill(k + 1)(0)
    val (sln, bp) = loop(initialSolution, items, Map())
    val values = items map (i => (i._1, i._2._1)) toMap
    val weights = items map (i => (i._1, i._2._2)) toMap
    val bestSln = sln(k)
    def backtrack(sln: Int): List[Int] =
      if (sln == 0) List()
      else {
        val selectedItem = bp(sln)
        selectedItem :: backtrack(sln - weights(selectedItem))
      }
    val selectedItems = backtrack(bestSln)
    var set = Set() ++ selectedItems
    val selectedValue = selectedItems map (values(_)) sum
    val selectedIndicators = for (i <- 0 until n) yield if (set.contains(i)) "1" else "0"

    println("" + selectedValue + " 0")
    println(selectedIndicators mkString " ")
  }
}