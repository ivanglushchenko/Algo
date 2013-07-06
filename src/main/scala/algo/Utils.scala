package main.scala.algo

import java.io.File

object Utils {
  def toTwoInts(s: String): (Int, Int) = {
    val parts = (s split ' ' filterNot  (_.isEmpty))
    (parts.head.toInt, parts.tail.head.toInt)
  }

  def mapi[T, U](list: List[T], f: (Int, T) => U): List[U] = {
    var i = 0
    (for (el <- list) yield {
      i = i + 1
      f (i - 1, el)
    }) toList
  }

  def writeToFile(fileName: String)(op: java.io.PrintWriter => Unit) {
    val file = new File(fileName)
    val p = new java.io.PrintWriter(file)
    try {
      op(p)
    } finally {
      p.close()
    }
  }
}
