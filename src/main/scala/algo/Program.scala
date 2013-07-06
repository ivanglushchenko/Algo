package main.scala.algo

object Program extends App {
  val filename = args.head
  val source = scala.io.Source.fromFile(filename).getLines().toList
  val knapsack = new Knapsack(source.head, source.tail)
  knapsack.solve()
  //knapsack.solveNoBacktrack()

/*
  val filename = args.head
  val source = scala.io.Source.fromFile(filename)
  val data = source.mkString.split("\n").toList.
    map(_.split(" ").toList.filter(_.size>0).
    map(_.toInt)).filter(_.size>0).
    map(x => (x.head, x.tail.head))

  def solve( data: List[(Int,Int)]) = {
    val capacity = data.head._2
  }

  solve(data)
*/
  //val a = args.mkString(",")
  //println("first line: " + source.head)
}
