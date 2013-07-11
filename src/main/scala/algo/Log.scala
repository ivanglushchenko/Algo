package main.scala.algo

import java.io.{FileWriter, File}
import java.util.Calendar
import java.text.SimpleDateFormat

object Log {
  val writer =
    if (Program.isProd) new FileWriter("s:/Sources/Algo/tmp/prod.txt", false)
    else new FileWriter("s:/Sources/Algo/tmp/dev.txt", true)

  def writeLine(message: String) {
    writeLine(message, true)
  }

  def writeLine(message: String, isEnabled: Boolean) {
    write(message + "\n\r", isEnabled)
  }

  def write(message: String) {
    write(message, true)
  }

  def write(message: String, isEnabled: Boolean) {
    if (isEnabled){
      val today = Calendar.getInstance().getTime()
      val prompt = new SimpleDateFormat("HH").format(today) + ":" + new SimpleDateFormat("mm").format(today) + ":" + new SimpleDateFormat("ss").format(today)

      writer.write(prompt + "  " + message)
      writer.flush()
    }
  }
}
