package main.scala.algo

object Program extends App {
  val filename = args.head
  val source = scala.io.Source.fromFile(filename).getLines().toList
  val coloring = new GraphColoring(source.head, source.tail)
  coloring.solve()
}
