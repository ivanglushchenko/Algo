package main.scala.algo

import scala.collection.mutable.{ Map => MMap }

class WarehouseLocation(header: String, body: List[String]) {
  val (n, m) = Utils.toTwoInts(header)
  //val w  = Utils.mapi(body take n, (i, el: String) => (i, Utils.toTwoDoubles(el)))
  val w  = Utils.mapi(body take n)((i, el) => (i, Utils.toTwoDoubles(el)))
  def getCustomer(i: Int, xs: List[String]): List[(Int, Int, Map[Int, Double])] = xs match {
    case hd :: nk :: tl =>
      var distances = (nk split " " map (_.toDouble)).toList
      val dMap = Utils.mapi(distances)((i, el) => (i, el)).toMap
      (i, hd.toInt, dMap) :: getCustomer(i + 1, tl)
    case _ => List()
  }
  val c = getCustomer(0, body drop n)

  def cost(sln: Array[Int]) = {
    val wc = (sln.toSet.toList map ((t: Int) => w(t)._2._2)).sum
    val cc = Utils.mapi(sln.toList)((i, t) => c(i)._3(t)).sum

    wc + cc
  }

  def getGreedySln(): Array[Int] = {
    val wc = MMap.empty[Int, Double] ++ (for (t <- w) yield (t._1, t._2._1))

    val sln = Array.fill(m)(0)

    def getClosestWarehouse(i: Int): Int = {
      val allWarehouses = c(i)._3.toList.sortBy(t => t._2)
      val w = allWarehouses.filter(t => wc(t._1) >= c(i)._2)
      if (w.size > 1) w.tail.head._1
      else w.head._1
    }


    for (i <- 0 until m) {
      var closestW = getClosestWarehouse(i)
      wc += (closestW -> (wc(closestW) - c(i)._2))
      sln(i) = closestW
    }

    sln
  }

  def solve() {
    val sln = getGreedySln()
    val slnValue = cost(sln)
    println("" + slnValue + " 0")
    println("" + sln.mkString(" "))
  }
}
