package main.scala.algo

import java.io.File
import scala.collection.LinearSeq

object Utils {
  def toTwoInts(s: String): (Int, Int) = {
    val parts = (s split ' ' filterNot  (_.isEmpty))
    (parts.head.toInt, parts.tail.head.toInt)
  }

  def toTwoDoubles(s: String): (Double, Double) = {
    val parts = (s split ' ' filterNot  (_.isEmpty))
    (parts.head.toDouble, parts.tail.head.toDouble)
  }

  def mapi[B, T <: List[B], U](list: T, f: (Int, B) => U): List[U] = {
    var i = 0
    (for (el <- list) yield {
      i = i + 1
      f (i - 1, el)
    }).toList
  }

  def mapi2[B, T <: scala.collection.immutable.Seq[B], U](list: T)(f: (Int, B) => U): List[U] = {
    var i = 0
    (for (el <- list) yield {
      i = i + 1
      f (i - 1, el)
    }).toList
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
