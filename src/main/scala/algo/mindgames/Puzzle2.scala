package main.scala.algo.mindgames.puzzle2

/**
 * Created with IntelliJ IDEA.
 * User: Ivan
 * Date: 9/28/13
 * Time: 8:58 PM
 * To change this template use File | Settings | File Templates.
 */
abstract sealed class Cell {
  val canJumpTo: Set[Cell]
  def isGood(op1: Cell, op2: Cell): Boolean = !op1.canJumpTo.contains(this) && !op2.canJumpTo.contains(this)
}

case object A extends Cell {
  override val canJumpTo = Set[Cell](C, D, F)
}

case object B extends Cell {
  override val canJumpTo = Set[Cell](D, E, G)
}

case object C extends Cell {
  override val canJumpTo = Set[Cell](A, E, H, J)
}

case object D extends Cell {
  override val canJumpTo = Set[Cell](A, B, E, G, F)
}

case object E extends Cell {
  override val canJumpTo = Set[Cell](B, D, G, C, H, J)
}

case object F extends Cell {
  override val canJumpTo = Set[Cell](A, D, H, I)
}

case object G extends Cell {
  override val canJumpTo = Set[Cell](I, B, D, E)
}

case object H extends Cell {
  override val canJumpTo = Set[Cell](F, I, C, E, J)
}

case object I extends Cell {
  override val canJumpTo = Set[Cell](G, F, H)
}

case object J extends Cell {
  override val canJumpTo = Set[Cell](C, E, H)
}

abstract sealed class Player {
  def teammate: Player
  def opponents: List[Player]
}

case object X1 extends Player {
  override def teammate = X2
  override def opponents = List(Y1, Y2)
}

case object X2 extends Player {
  override def teammate = X1
  override def opponents = List(Y1, Y2)
}

case object Y1 extends Player {
  override def teammate = Y2
  override def opponents = List(X1, X2)
}

case object Y2 extends Player {
  override def teammate = Y1
  override def opponents = List(X1, X2)
}

object Player
{
  val all = List(X1, X2, Y1, Y2)
}

case class Turn(val map: Map[Player, Cell]) {
  var steps = List[(Player, Cell)]()

  val x1 = map(X1)
  val x2 = map(X2)
  val y1 = map(Y1)
  val y2 = map(Y2)
  val isFinal = ((x1 == I && x2 == J) || (x1 == J && x2 == I)) && ((y1 == A && y2 == B) || (y1 == B && y2 == A))

  def next: List[Turn] = {
    for {
      player <- Player.all
      playerCanJumpTo <- map(player).canJumpTo
      if playerCanJumpTo != map(player.teammate)
      if playerCanJumpTo != map(player.opponents(0))
      if playerCanJumpTo != map(player.opponents(1))
      if playerCanJumpTo.isGood(map(player.opponents(0)), map(player.opponents(1)))
    } yield extend(player, playerCanJumpTo)
  }

  def extend(p: Player, c: Cell) = {
    val turn = Turn(map updated (p, c))
    turn.steps = (p, c) :: steps
    turn
  }
}

object Turn {
  def apply(pairs : scala.Tuple2[Player, Cell]*) = {
    val map = Map() ++ pairs
    new Turn(map)
  }
}

class Puzzle2 {
  val startingPos = Turn(X1 -> A, X2 -> B, Y1 -> I, Y2 -> J)

  def nextTurns(allTurns: Set[Turn], currentTurns: List[Turn]) : Stream[Turn] = currentTurns match {
    case Nil => Stream.empty
    case _ => {
      val allNext = Set() ++ currentTurns flatMap (_.next)
      val unique = allNext filter (!allTurns.contains(_))
      if (unique.size == 0) Stream.Empty
      else Stream.concat(unique toStream, nextTurns(allTurns ++ unique, unique.toList))
    }
  }

  lazy val allTurns = nextTurns(Set(startingPos), List(startingPos))
  lazy val allFinals = allTurns filter (_.isFinal)

  def solve() {
    val oneSolution = allFinals.take(1).toList.head
    println(oneSolution.steps)
  }
}
