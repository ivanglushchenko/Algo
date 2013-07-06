package main.scala.algo

import scala.collection.immutable.Set
import scala.collection.mutable.BitSet

class Knapsack(header: String, body: List[String]) {
  val (n, k) = Utils.toTwoInts(header)
  val items = Utils.mapi(body filter(!_.isEmpty), (i, el: String) => (i, Utils.toTwoInts(el)))

  def solveWithImplicitBacktracking() {
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
    val weights = items map (i => (i._1, i._2._2)) toMap
    val selectedValue = selectedItems map (values(_)) sum
    val selectedIndicators = for (i <- 0 until n) yield if (set.contains(i)) "1" else "0"

    println("" + selectedValue + " 0")
    println(selectedIndicators mkString " ")

    Log.writeLine(set.toList.sorted map (i => "" + i + "=" ) mkString (" "))
  }

  def solveWithExplicitBacktracking() {
    Log.writeLine("Solving " + n + ":" + k)

    val bp = Array.fill(n)(BitSet.empty)

    def loop(prevSln: Array[Int], nextSln: Array[Int], items: List[(Int, (Int, Int))]): (Array[Int]) = items match {
      case (n, (v, w)) :: tl =>
        Log.writeLine("-> starting iteration " + (n + 1))
        //val nextSln = Array.ofDim[Int](k + 1)
        for (i <- 0 to k) {
          if (i < w || prevSln(i) >= (v + prevSln(i - w))) nextSln(i) = prevSln(i)
          else {
            nextSln(i) = v + prevSln(i - w)
            bp(n).add(i)
          }
        }

        Log.writeLine("<- done")

        loop(nextSln, prevSln, tl)

      case _ => prevSln
    }

    loop(Array.fill(k + 1)(0), Array.fill(k + 1)(0), items)

    val values = items map (i => (i._1, i._2._1)) toMap
    val weights = items map (i => (i._1, i._2._2)) toMap

    def backtrack(n: Int, k: Int): Set[Int] =
      if (n < 0 || k == 0) Set()
      else {
        if (bp(n).contains(k)) backtrack(n - 1, k - weights(n)) + n
        else backtrack(n - 1, k)
      }

    val selectedItems = backtrack(n - 1, k)
    val selectedValue = selectedItems map (values(_)) sum
    val selectedIndicators = for (i <- 0 until n) yield if (selectedItems.contains(i)) "1" else "0"

    println("" + selectedValue + " 1")
    println(selectedIndicators mkString " ")
  }
}