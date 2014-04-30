package main.scala.algo.discreteoptimizations

import scala.util.Random
import scala.collection.mutable.{Set => MSet, Map => MMap}
import main.scala.algo.{Log, Utils}

class TravelingSalesman(header: String, body: List[String]) {
  val n = header.toInt
  val coordinates = Utils.mapi(body filter(!_.isEmpty))((i, el) => (i, Utils.toTwoDoubles(el))).toMap
  def calcDist(i: Int, j: Int) = math.sqrt(math.pow(coordinates(i)._1 - coordinates(j)._1, 2) + math.pow(coordinates(i)._2 - coordinates(j)._2, 2))

  val rnd = new Random(2)

  def length(path: List[Int]): Double = {
    def loop(vs: List[Int], acc: Double): Double = vs match {
      case hd :: nk :: tl => loop(nk :: tl, calcDist(hd, nk) + acc)
      case hd :: tl => acc + calcDist(hd, path.head)
      case _ => throw new IllegalStateException()
    }
    loop(path, 0)
  }

  def length(path: Array[Int]): Double = {
    var res = 0.0
    for (i <- 1 until path.length) {
      res = res + calcDist(path(i - 1), path(i))
    }
    res + calcDist(path(0), path(path.length - 1))
  }

  def intersect(row1: (Int, Int), row2: (Int, Int)): Boolean = intersect(coordinates(row1._1), coordinates(row1._2), coordinates(row2._1), coordinates(row2._2))

  def intersect(row1_from: Int, row1_to: Int, row2_from: Int, row2_to: Int): Boolean = intersect(coordinates(row1_from), coordinates(row1_to), coordinates(row2_from), coordinates(row2_to))

  def intersect(row1_from: (Double, Double), row1_to: (Double, Double), row2_from: (Double, Double), row2_to: (Double, Double)): Boolean =
    if ((row1_from == row2_from && row1_to == row2_to) || (row1_from == row2_to && row1_to == row2_from)) true
    else if ((row1_from == row2_from && row1_to != row2_to) || (row1_from != row2_from && row1_to == row2_to) || (row1_from == row2_to && row1_to != row2_from) || (row1_from != row2_to && row1_to == row2_from)) false
    else {
      if (row1_to._1 != row1_from._1) {
        val t2 = ((row2_from._2 - row1_from._2) * (row1_to._1 - row1_from._1) - (row2_from._1 - row1_from._1) * (row1_to._2 - row1_from._2)) / ((row2_to._1 - row2_from._1) * (row1_to._2 - row1_from._2) - (row2_to._2 - row2_from._2) * (row1_to._1 - row1_from._1))
        val t1 = ((row2_to._1 - row2_from._1) * t2 + row2_from._1 - row1_from._1) / (row1_to._1 - row1_from._1)
        t2 >= 0 && t2 <= 1 && t1 >= 0 && t1 <= 1
      } else {
        val t2 = ((row2_from._1 - row1_from._1) * (row1_to._2 - row1_from._2) - (row2_from._2 - row1_from._2) * (row1_to._1 - row1_from._1)) / ((row2_to._2 - row2_from._2) * (row1_to._1 - row1_from._1) - (row2_to._1 - row2_from._1) * (row1_to._2 - row1_from._2))
        val t1 = ((row2_to._2 - row2_from._2) * t2 + row2_from._2 - row1_from._2) / (row1_to._2 - row1_from._2)
        t2 >= 0 && t2 <= 1 && t1 >= 0 && t1 <= 1
      }
    }

  def validateSln(sln: Array[Int]) = (Set() ++ sln).size == n

  def getRandomSln = rnd.shuffle(List.range(0, n))

  val closestNeighbours = MMap.empty[Int, List[Int]]

  def getClosestNeighbour(i: Int, restrictedVertices: MSet[Int]): Int = {
    def getAllNeighbours = (for (j <- 0 until n; if j != i) yield (j, calcDist(i, j))) sortBy (_._2) map (_._1)

    if (closestNeighbours contains i) {
      val allowedNeighbours = closestNeighbours(i) filter (!restrictedVertices.contains(_))
      if (allowedNeighbours.isEmpty) (getAllNeighbours filter (!restrictedVertices.contains(_))).head
      else allowedNeighbours.head
    } else {
      val neighbours = getAllNeighbours
      closestNeighbours += (i -> (neighbours take 2).toList)
      (neighbours filter (!restrictedVertices.contains(_))).head
    }
  }

  def getInitialClosestNeighbour(): Array[Int] = (List.range(0, n) map (i => getInitialClosestNeighbour(i)) sortBy(_._2)).head._1

  def getInitialClosestNeighbour(startingPoint: Int) = {
    Log.write("in getInitialClosestNeighbour " + startingPoint + "...")

    val sln = Array.fill(n)(0)
    sln(0) = startingPoint
    val visited = MSet(startingPoint)
    for (prevItem <- 0 until n - 1) {
      val closest = getClosestNeighbour(sln(prevItem), visited)
      sln(prevItem + 1) = closest
      visited add closest
    }
    val l = length(sln)
    Log.writeLine("" + l)
    (sln, l)
  }

  def greedyClosestNeighbour(sln: Array[Int]) = {
    Log.writeLine("in greedyClosestNeighbour")
    var iteration = 0
    while (iteration < n * 4) {
      val l = length(sln)
      val n1 = rnd.nextInt(n)
      val n2 = if (n1 == n - 1) 0 else n1 + 1

      val neighbours = for (i <- 0 until n; if i != sln(n1)) yield (calcDist(sln(n1), i), i)
      val closestNeighbour = (neighbours.toList sortBy (_._1)).head
      val cnIndex = sln indexOf closestNeighbour._2

      sln(cnIndex) = sln(n2)
      sln(n2) = closestNeighbour._2

      val newLength = length(sln)
      if (newLength > l) {
        sln(n2) = sln(cnIndex)
        sln(cnIndex) = closestNeighbour._2
      }
      iteration = iteration + 1
    }

    sln
  }

  def straigthtenPath(sln: Array[Int]) = {
    Log.writeLine("in straigthtenPath")
    var iteration = 0
    while (iteration < n) {
      val t1 = iteration
      val t2 = (t1 + 1) % n
      val t3 = (t2 + 1) % n
      val d1 = calcDist(sln(t1), sln(t2))
      val d2 = calcDist(sln(t2), sln(t3))
      val d3 = calcDist(sln(t1), sln(t3))
      if (d1 + d2 > d2 + d3){
        val t = sln(t2)
        sln(t2) = sln(t3)
        sln(t3) = t
      }

      iteration = iteration + 1
    }

    sln
  }

  def apply2opt(sln: Array[Int]) = {
    Log.writeLine("in apply2opt")

    var currentLength = length(sln)
    var iteration = 0

    while (iteration < 10) {
      Log.writeLine("2opt: i = " + iteration + ", currentLength = " + currentLength)

      val edges = ((for (from <- 0 until n; to = (from + 1) % n; d = calcDist(sln(from), sln(to))) yield (d, (from, to))) sortBy (-_._1) map (_._2)).toArray
      val crosses = (for (i <- 0 until (n - 1); j <- (i + 1) until n ; row1 = edges(i); row2 = edges(j); if intersect(sln(row1._1), sln(row1._2), sln(row2._1), sln(row2._2))) yield (row1, row2)).toList
      if (crosses.isEmpty)
        iteration = 30
      else {
        val affectedEdges = MSet[Int]()
        for (fc <- crosses;
             if !affectedEdges.contains(fc._1._1) && !affectedEdges.contains(fc._2._1)) {
          //affectedEdges add fc._1._1
          //affectedEdges add fc._2._1
          val newLength = currentLength - calcDist(sln(fc._1._1), sln(fc._1._2)) - calcDist(sln(fc._2._1), sln(fc._2._2)) + calcDist(sln(fc._1._1), sln(fc._2._1)) + calcDist(sln(fc._1._2), sln(fc._2._2));
          if (newLength < currentLength) {
            currentLength = newLength
            swap2Opt(sln, math.min(fc._1._1, fc._2._1), math.max(fc._1._1, fc._2._1))
            //val eee = length(sln)
            //if (math.abs(eee - newLength) > 1) {
            //  println ("ddd")
            //}
          }
        }
      }

      iteration = iteration + 1
    }

    val edges = ((for (from <- 0 until n; to = (from + 1) % n; d = calcDist(sln(from), sln(to))) yield (d, (from, to))) sortBy (-_._1) map (_._2)).toArray
    val crosses = (for (i <- 0 until (n - 1); j <- (i + 1) until n ; row1 = edges(i); row2 = edges(j); if intersect(sln(row1._1), sln(row1._2), sln(row2._1), sln(row2._2))) yield (row1, row2)).toList
    Log.writeLine("unresolved crosses:")
    for (fc <- crosses) {
      Log.writeLine("   " + sln(fc._1._1) + "->" + sln(fc._1._2) + " crosses " + sln(fc._2._1) + "->" + sln(fc._2._2))
    }
    sln
  }

  def swap2Opt(sln: Array[Int], i: Int, k: Int) {
    Log.writeLine("in swap2Opt " + i + ", " + k, false)
    val numOfSwaps = (k - i) / 2
    for (l <- 0 until numOfSwaps) {
      val t = sln(i + l + 1)
      sln(i + l + 1) = sln(k - l)
      sln(k - l) = t
    }
    sln
  }

  def readFromFile() = {
    val lines = scala.io.Source.fromFile("s:\\Sources\\Algo\\tmp\\tsp_opt\\p6").getLines().toList
    lines.tail.map(Utils.toTwoInts(_)).map(_._1).toArray
  }

  def solve() {
    val sln = readFromFile() //apply2opt(getInitialClosestNeighbour()) // apply2opt(straigthtenPath(greedyClosestNeighbour(Array.range(0, n))))
    if (!validateSln(sln)) println("sln is broken")
    val slnValue = length(sln.toList)
    Log.writeLine("solution: " + slnValue)
    println("" + slnValue + " 0")
    println("" + sln.mkString(" "))
  }
}
