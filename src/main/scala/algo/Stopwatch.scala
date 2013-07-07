package main.scala.algo

object Stopwatch {
  var counter = 0

  def measure(caption: String)(action: => Unit) {
    counter = counter + 1

    val startTime = System.currentTimeMillis
    Log.writeLine("-> #" + counter + " " + caption)
    action

    val dt = (System.currentTimeMillis - startTime).toDouble / 1000.0
    Log.writeLine("<- #" + counter + " done in " + dt + "s")
  }
}