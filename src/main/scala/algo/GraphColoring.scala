package main.scala.algo

import scala.collection.immutable.BitSet
import scala.util.Random

class GraphColoring(header: String, body: List[String]) {
  val (n, k) = Utils.toTwoInts(header)
  val M = 78
  val edges = body filter(!_.isEmpty) map (Utils.toTwoInts(_)) flatMap (t => List((t._1, t._2), (t._2, t._1)))
  val edgeMap = edges groupBy (t => t._1) map (t => (t._1, (t._2 map (_._2)).toSet))

  val seed = new Random().nextInt()
  val rnd = new Random(seed)

  Log.writeLine("Seed set to: " + seed)
  Log.writeLine("Solving: " + header)
  Log.writeLine("Edges stats: " + (edgeMap.toList sortBy (t => -t._2.size) map (t => "" + t._1 + ":" + t._2.size) mkString "; "), isEnabled = false)

  def readFromFile() = {
    scala.io.Source.fromFile("s:\\Sources\\Algo\\tmp\\gc_opt\\p2").getLines().toList
  }

  def useConstraintPropagation() = {
    val allPossibleColors =
      (for (i <- 0 until n) yield (i, BitSet() ++ (for(i <- 0 until M) yield i))).toMap

    def applyConstraints(availableColors: Map[Int, BitSet], vertex: Int, color: Int) = {
      val reducedColors = availableColors - vertex
      reducedColors map (t => (t._1, if (edgeMap(vertex).contains(t._1)) t._2 - color else t._2))
    }

    def extendInitialColorAssignments(ca: Map[Int, Int], ac: Map[Int, BitSet], vertices: Set[Int], nextColor: Int): (Map[Int, Int], Map[Int, BitSet]) =
      if (vertices.isEmpty) (ca, ac)
      else {
        val intersections = for (v <- vertices) yield (v, vertices intersect edgeMap(v))
        val (v, nextV) = (intersections.toList sortBy(-_._2.size)).head
        //val v =  (vertices.toList sortBy (t => -edgeMap(t).size)).head
        val nextCA = ca + (v -> nextColor)
        val nextAC = applyConstraints(ac, v, nextColor)
        //val nextV = vertices intersect edgeMap(v)

        Log.writeLine("init assigmnent step: " + v + " -> " + nextColor + ", next vertices: " + nextV.size + ", assigned colors: " + nextCA.size)

        extendInitialColorAssignments(nextCA, nextAC, nextV, nextColor + 1)
      }

    val bestStartingVertices = {
      val allIntersections = for (i <- 0 until n - 1; j <- i + 1 until n) yield (i, j, (edgeMap(i) intersect edgeMap(j)).size)
      val biggestIntersection = allIntersections.toList.sortBy(-_._3).head
      (biggestIntersection._1, biggestIntersection._2)
    }

    val (initialAssignedColors, initialAvailableColors) = extendInitialColorAssignments(
      Map(bestStartingVertices._1 -> 0, bestStartingVertices._2 -> 1),
      applyConstraints(applyConstraints(allPossibleColors, bestStartingVertices._1, 0), bestStartingVertices._2, 1),
      edgeMap(bestStartingVertices._1) intersect edgeMap(bestStartingVertices._2),
      2)

    /*
        val mostConnectedEdge = (edges sortBy (t => -edgeMap(t._1).size - edgeMap(t._2).size)).head

        def extendInitialColorAssignments(ca: Map[Int, Int], ac: Map[Int, BitSet], vertices: Set[Int], nextColor: Int): (Map[Int, Int], Map[Int, BitSet]) =
          if (vertices.isEmpty) (ca, ac)
          else {
            val v = (vertices.toList sortBy (t => -edgeMap(t).size)).head
            val nextCA = ca + (v -> nextColor)
            val nextAC = applyConstraints(ac, v, nextColor)
            val nextV = vertices intersect edgeMap(v)

            Log.writeLine("init assigmnent step: " + v + " -> " + nextColor + ", next vertices: " + nextV.size + ", assigned colors: " + nextCA.size)

            extendInitialColorAssignments(nextCA, nextAC, nextV, nextColor + 1)
          }

        val (initialAssignedColors, initialAvailableColors) = extendInitialColorAssignments(
          Map(mostConnectedEdge._1 -> 0, mostConnectedEdge._2 -> 1),
          applyConstraints(applyConstraints(allPossibleColors, mostConnectedEdge._1, 0), mostConnectedEdge._2, 1),
          edgeMap(mostConnectedEdge._1) intersect edgeMap(mostConnectedEdge._2),
          2)
    */
    val initialColorGroups = (initialAssignedColors.toList map (_._2) groupBy(t => t)) map (t => (t._1, t._2.size)) withDefaultValue 0
    val decisions = scala.collection.mutable.Stack.fill(1)((initialAssignedColors, initialAvailableColors, initialColorGroups))

    var iteration = 1
    while (decisions.head._1.size < n) {
      val (assignedColors, availableColors, colorGroups) = decisions.head

      val availableColorGroups = (availableColors.toList groupBy (t => t._2.size)).toList
      val mostRestrictedVertices = (availableColorGroups sortBy (t => t._1)).head._2
      val mostRestrictedVerticeWithMostEdges = (mostRestrictedVertices sortBy (t => -edgeMap(t._1).size)).head._1

      if (availableColors(mostRestrictedVerticeWithMostEdges).isEmpty){
        decisions.pop()
        //} else if (availableColors(mostRestrictedVerticeWithMostEdges).size == 1) {

      } else {
        //val i = rnd.nextInt(availableColors(mostRestrictedVerticeWithMostEdges).size)
        //val assignedColor = availableColors(mostRestrictedVerticeWithMostEdges).drop(i - 1).head
        //val assignedColor = availableColors(mostRestrictedVerticeWithMostEdges).head
        val assignedColor = (availableColors(mostRestrictedVerticeWithMostEdges) map (t => (t, colorGroups(t)))).toList.sortBy(_._2).head._1

        Log.writeLine("v " + mostRestrictedVerticeWithMostEdges + " -> " + assignedColor + " (" + availableColors(mostRestrictedVerticeWithMostEdges).size + " options)")

        val updAvailableColors = availableColors.updated(mostRestrictedVerticeWithMostEdges, availableColors(mostRestrictedVerticeWithMostEdges) - assignedColor)
        val newAssignedColors = assignedColors + (mostRestrictedVerticeWithMostEdges -> assignedColor)
        val newColorGroups = colorGroups updated (assignedColor, colorGroups(assignedColor) + 1)

        decisions.pop()
        decisions.push((assignedColors, updAvailableColors, colorGroups))

        val newAvailableColors = applyConstraints(availableColors, mostRestrictedVerticeWithMostEdges, assignedColor)

        decisions.push((newAssignedColors, newAvailableColors, newColorGroups))
      }

      iteration = iteration + 1
      if (iteration % 1000 == 0){
        //Log.writeLine("iteration: " + iteration + ", stack size: " + decisions.size)
      }
    }

    decisions.head._1
  }

  def solve() {
    val finalColors = useConstraintPropagation()

    Log.writeLine("Final colors:" + (finalColors.toList sortBy (t => t._1)))
    Log.writeLine("Color groups:" + ((finalColors.toList groupBy (t => t._2)).toList map (t => (t._1, t._2.size)) sortBy (t => t._2)))

    //readFromFile() map (println(_))
  }
}
