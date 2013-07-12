package main.scala.algo

object Program extends App {
  val isProd = false

  val filename = args.head
  val source = scala.io.Source.fromFile(filename).getLines().toList
  val solver = new GraphColoring(source.head, source.tail)
  Stopwatch.measure("solve") {
    solver.solve()
  }
}
