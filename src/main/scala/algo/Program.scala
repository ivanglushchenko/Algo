package main.scala.algo

import main.scala.algo.mindgames.puzzle4.Puzzle4

object Program extends App {
  val isProd = false

  //val filename = args.head
  //val source = scala.io.Source.fromFile(filename).getLines().toList
  //val solver = new WarehouseLocation(source.head, source.tail)
  Stopwatch.measure("solve") {
    //new Puzzle2()
    //solver.solve()
  }

  new Puzzle4().solve()
}
