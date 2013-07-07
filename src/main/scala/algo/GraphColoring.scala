package main.scala.algo

import scala.collection.immutable.BitSet

class GraphColoring(header: String, body: List[String]) {
  val (n, k) = Utils.toTwoInts(header)
  val edges = body filter(!_.isEmpty) map (Utils.toTwoInts(_)) flatMap (t => List((t._1, t._2), (t._2, t._1)))
  val edgeMap = edges groupBy (t => t._1) map (t => (t._1, (t._2 map (_._2)).toSet))

  Log.writeLine("Solving: " + header)
  Log.writeLine("Edges stats: " + (edgeMap.toList sortBy (t => -t._2.size) map (t => "" + t._1 + ":" + t._2.size) mkString "; "))

  def solve() {
    val initialAssignedColors = Map.empty[Int, Int]
    val initialAvailableColors = (for (i <- 0 until n) yield (i, BitSet() ++ (for(i <- 0 until n) yield i))).toMap

    val decisions = scala.collection.mutable.Stack.fill(1)((initialAssignedColors, initialAvailableColors))

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

        def applyConstraints(vertex: Int, availableColors: BitSet): BitSet =
          if (edgeMap(mostRestrictedVerticeWithMostEdges).contains(vertex)) availableColors - assignedColor
          else availableColors

        val newAvailableColors =
          (availableColors - mostRestrictedVerticeWithMostEdges) map(t => (t._1, applyConstraints (t._1, t._2)))

        decisions.push((newAssignedColors, newAvailableColors))
      }
    }

    val (finalColors, _) = decisions.head

    val colors = for (i <- 0 until n) yield finalColors(i).toString
    val uniqueColors = (finalColors map (_._2)).toSet
    println("" + uniqueColors.size + " 0")
    println(colors mkString " ")
  }
}
