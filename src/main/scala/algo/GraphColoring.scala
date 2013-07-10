package main.scala.algo

import scala.collection.immutable.BitSet

class GraphColoring(header: String, body: List[String]) {
  val (n, k) = Utils.toTwoInts(header)
  val M = n / 2
  val edges = body filter(!_.isEmpty) map (Utils.toTwoInts(_)) flatMap (t => List((t._1, t._2), (t._2, t._1)))
  val edgeMap = edges groupBy (t => t._1) map (t => (t._1, (t._2 map (_._2)).toSet))

  Log.writeLine("Solving: " + header)
  Log.writeLine("Edges stats: " + (edgeMap.toList sortBy (t => -t._2.size) map (t => "" + t._1 + ":" + t._2.size) mkString "; "), isEnabled = false)

  def solve() {
    val mostConnectedEdge = (edges sortBy (t => -edgeMap(t._1).size - edgeMap(t._2).size)).head
    val allPossibleColors =
      (for (i <- 0 until n) yield (i, BitSet() ++ (for(i <- 0 until M) yield i))).toMap

    def applyConstraints(availableColors: Map[Int, BitSet], vertex: Int, color: Int) = {
      val reducedColors = availableColors - vertex
      reducedColors map (t => (t._1, if (edgeMap(vertex).contains(t._1)) t._2 - color else t._2))
    }

    def extendInitialColorAssignments(ca: Map[Int, Int], ac: Map[Int, BitSet], vertices: Set[Int], nextColor: Int): (Map[Int, Int], Map[Int, BitSet]) =
      if (vertices.isEmpty) (ca, ac)
      else {
        val v = (vertices.toList sortBy (t => -edgeMap(t).size)).head
        val nextCA = ca + (v -> nextColor)
        val nextAC = applyConstraints(ac, v, nextColor)
        val nextV = vertices intersect edgeMap(v)
        extendInitialColorAssignments(nextCA, nextAC, nextV, nextColor + 1)
      }

    val (initialAssignedColors, initialAvailableColors) = extendInitialColorAssignments(
      Map(mostConnectedEdge._1 -> 0, mostConnectedEdge._2 -> 1),
      applyConstraints(applyConstraints(allPossibleColors, mostConnectedEdge._1, 0), mostConnectedEdge._2, 1),
      edgeMap(mostConnectedEdge._1) intersect edgeMap(mostConnectedEdge._2),
      2)

    val decisions = scala.collection.mutable.Stack.fill(1)((initialAssignedColors, initialAvailableColors))

    var iteration = 1
    while (decisions.head._1.size < n) {
      val (assignedColors, availableColors) = decisions.head

      val colorGroups = (availableColors.toList groupBy (t => t._2.size)).toList
      val mostRestrictedVertices = (colorGroups sortBy (t => t._1)).head._2
      val mostRestrictedVerticeWithMostEdges = (mostRestrictedVertices sortBy (t => -edgeMap(t._1).size)).head._1

      if (availableColors(mostRestrictedVerticeWithMostEdges).isEmpty){
        decisions.pop()
      } else {
        val assignedColor = availableColors(mostRestrictedVerticeWithMostEdges).head
        val updAvailableColors = availableColors.updated(mostRestrictedVerticeWithMostEdges, availableColors(mostRestrictedVerticeWithMostEdges) - assignedColor)
        val newAssignedColors = assignedColors + (mostRestrictedVerticeWithMostEdges -> assignedColor)

        decisions.pop()
        decisions.push((assignedColors, updAvailableColors))

        val newAvailableColors = applyConstraints(availableColors, mostRestrictedVerticeWithMostEdges, assignedColor)

        decisions.push((newAssignedColors, newAvailableColors))
      }

      iteration = iteration + 1
      if (iteration % 1000 == 0){
        Log.writeLine("iteration: " + iteration + ", stack size: " + decisions.size)
      }
    }

    val (finalColors, _) = decisions.head

    Log.writeLine("Final colors:" + (finalColors.toList sortBy (t => t._1)))
    Log.writeLine("Color groups:" + ((finalColors.toList groupBy (t => t._2)).toList map (t => (t._1, t._2.size)) sortBy (t => t._2)))

    for ((v, c) <- finalColors) {
      val neighbourColors = (edgeMap(v) map (t => finalColors(t))).toList.sorted
      Log.writeLine("for v" + v + ", " + c + ": " + neighbourColors.size + "(" + neighbourColors + ")")
    }

    val colors = for (i <- 0 until n) yield finalColors(i).toString
    val uniqueColors = (finalColors map (_._2)).toSet
    println("" + uniqueColors.size + " 0")
    println(colors mkString " ")
  }
}
