package main.scala.algo

import java.io.{FileWriter, File}
import java.util.Calendar
import java.text.SimpleDateFormat

object Log {
  val file = new File("s:/Sources/Algo/tmp/log.txt")
  //val writer = new java.io.PrintWriter(file)
  val writer = new FileWriter("s:/Sources/Algo/tmp/log.txt", true)

  def write(message: String) {
    val today = Calendar.getInstance().getTime()
    val prompt = new SimpleDateFormat("HH").format(today) + ":" + new SimpleDateFormat("mm").format(today) + ":" + new SimpleDateFormat("ss").format(today)

    writer.write(prompt + "  " + message)
    writer.flush()
  }

  def writeLine(message: String) {
    val today = Calendar.getInstance().getTime()
    val prompt = new SimpleDateFormat("HH").format(today) + ":" + new SimpleDateFormat("mm").format(today) + ":" + new SimpleDateFormat("ss").format(today)

    writer.write(prompt + "  " + message + "\n\r")
    writer.flush()
  }
}
