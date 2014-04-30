package main.scala.algo.mindgames.puzzle4

/**
 * Created with IntelliJ IDEA.
 * User: Ivan
 * Date: 9/29/13
 * Time: 10:27 AM
 * To change this template use File | Settings | File Templates.
 */
abstract class Direction {
  def jumpFrom(i: Int, j: Int): Option[(Int, Int)]
}

case object N extends Direction {
  override def jumpFrom(i: Int, j: Int): Option[(Int, Int)] = {
    if (i > 0) Some(i - 1, j) else None
  }
}

case object S extends Direction {
  override def jumpFrom(i: Int, j: Int): Option[(Int, Int)] = {
    if (i < 3) Some(i + 1, j) else None
  }
}

case object W extends Direction {
  override def jumpFrom(i: Int, j: Int): Option[(Int, Int)] = {
    if (j > 0) Some(i, j - 1) else None
  }
}

case object E extends Direction {
  override def jumpFrom(i: Int, j: Int): Option[(Int, Int)] = {
    if (j < 3) Some(i, j + 1) else None
  }
}

object Direction {
  val all = List(N, S, W, E)
}

case class Turn(val grid: Array[Array[Int]], val i: Int, val j: Int) {
  val signature = (grid map (line => line mkString (","))) mkString (":")
  val isFinal =
    grid(0)(0) == 1 && grid(0)(1) == 2 && grid(0)(2) == 3 && grid(0)(3) == 4 &&
    grid(1)(0) == 5 && grid(1)(1) == 6 && grid(1)(2) == 7 && grid(1)(3) == 8 &&
    grid(2)(0) == 9 && grid(2)(1) == 10 && grid(2)(2) == 11 && grid(2)(3) == 12 &&
    grid(3)(0) == 13 && grid(3)(1) == 14 && grid(3)(2) == 15 && grid(3)(3) == 0
  val matches =
    grid(0)(0) == 1 + grid(0)(1) == 2 + grid(0)(2) == 3 + grid(0)(3) == 4 +
    grid(1)(0) == 5 + grid(1)(1) == 6 + grid(1)(2) == 7 + grid(1)(3) == 8 +
    grid(2)(0) == 9 + grid(2)(1) == 10 + grid(2)(2) == 11 + grid(2)(3) == 12 +
    grid(3)(0) == 13 + grid(3)(1) == 14 + grid(3)(2) == 15 + grid(3)(3) == 0
  def nextJumps: List[(Int, Int)] = Direction.all map (d => d.jumpFrom(i, j)) filter (_.isDefined) map (_.get)

  def next: List[Turn] = nextJumps map (extend(_))

  def extend(n: (Int, Int)) = {
    val newGrid = grid.clone()

    val (ni, nj) = n
    val t1 = grid(i)(j)
    val t2 = grid(ni)(nj)

    newGrid(i)(j) = t2
    newGrid(ni)(nj) = t1

    new Turn(newGrid, ni, nj)
  }
}

object Turn {
  def apply(v: Array[Int], i: Int, j: Int) = {
    val a = v.toArray
    new Turn(Array(Array(a(0), a(1), a(2), a(3)), Array(a(4), a(5), a(6), a(7)), Array(a(8), a(9), a(10), a(11)), Array(a(12), a(13), a(14), a(15))), i, j)
  }
}

class Puzzle4 {
  val startingPos = Turn(Array(14, 3, 12, 2, 10, 13, 0, 4, 11, 15, 8, 5, 9, 6, 7, 1), 1, 2)

  def nextTurns(allSignatures: Set[String], unexploredTurns: List[Turn]) : Stream[Turn] = unexploredTurns match {
    case Nil => Stream.empty
    case hd :: tl => {
      val nextTurns = hd.next filter (t => !allSignatures.contains(t.signature))

      val allNext = Set() ++ unexploredTurns flatMap (_.next) map (t => (t.signature, t)) toMap
      val unique = allNext filter (t => !allSignatures.contains(t._1))
      if (unique.size == 0) Stream.Empty
      else {
        val uniqueSignatures = unique map (_._1)
        val uniqueTurns = unique map (_._2)
        Stream.concat(uniqueTurns toStream, nextTurns(allSignatures ++ uniqueSignatures, uniqueTurns toList))
      }
    }
  }

  lazy val allTurns = nextTurns(Set(startingPos.signature), List(startingPos))
  lazy val allFinals = allTurns filter (_.isFinal)

  def solve() {
    val oneSolution = allFinals.take(1).toList.head
    println(oneSolution)
  }
}
