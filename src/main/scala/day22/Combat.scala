package day22

import utils.InputUtils.splitInput

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Combat {

  type Deck = Queue[Int]

  case class Player(deck: Deck, history: Set[String] = Set.empty)

  case class Game(id: String,
                  p1: Player,
                  p2: Player,
                  winner: Option[Boolean] = None
                 ) {
    def checkInfinite(): Boolean = {
      Seq(p1, p2).exists(checkInfiniteLoop)
    }

    def p1WinsRound(): Game = {
      val ((c1, q1), (c2, q2)) = (p1.deck.dequeue, p2.deck.dequeue)
      val newP1 = p1.copy(
        deck = q1.enqueueAll(Seq(c1, c2)),
        history = p1.history + key(p1.deck)
      )
      val newP2 = p2.copy(deck = q2, history = p2.history + key(p2.deck))
      this.copy(p1 = newP1, p2 = newP2)
    }

    def p2WinsRound(): Game = {
      val ((c1, q1), (c2, q2)) = (p1.deck.dequeue, p2.deck.dequeue)
      val newP2 = p2.copy(
        deck = q2.enqueueAll(Seq(c2, c1)),
        history = p2.history + key(p2.deck)
      )
      val newP1 = p1.copy(deck = q1, history = p1.history + key(p1.deck))
      this.copy(p1 = newP1, p2 = newP2)
    }

    def playRound(display: Boolean = false): Game = {
      if (display) {
        println(s"Game :$id")
        println(s"P1 :" + p1.deck.mkString(","))
        println(s"P2 :" + p2.deck.mkString(","))
        println
      }
      if (p1.deck.isEmpty) this.copy(winner = Some(false))
      else if (p2.deck.isEmpty) this.copy(winner = Some(true))
      else if (checkInfinite()) this.copy(winner = Some(true))
      else {
        val ((c1, q1), (c2, q2)) = (p1.deck.dequeue, p2.deck.dequeue)
        if (c1 <= q1.size && c2 <= q2.size) {
          if (display) {
            println("Sub game")
          }
          // Recursive Game
          val subGame = Game(
            id = this.id + "_1",
            p1 = Player(deck = q1.take(c1)),
            p2 = Player(deck = q2.take(c2)),
          )
          val (p1Won, _) = rPlay(subGame, display)
          // Play Game
          if (p1Won) p1WinsRound() else p2WinsRound()
        } else {
          if (display) {
            println("Normal round" + (if (c1 > c2) "P1" else "P2") + " wins")
          }
          if (c1 > c2) p1WinsRound() else p2WinsRound()
        }
      }
    }
  }


  def parse(input: List[String]): (Deck, Deck) = {
    val Seq(p1, p2) = splitInput(input)
      .map(_.drop(1)
        .map(_.toInt)
      ).map(Queue.from[Int](_))
    (p1, p2)
  }

  def play(players: (Deck, Deck)): Deck = {
    val (p1, p2) = players

    @tailrec
    def playRec(one: Deck, two: Deck): Deck = {
      if (one.isEmpty) two
      else if (two.isEmpty) one
      else {
        val ((c1, newOne), (c2, newTwo)) = (one.dequeue, two.dequeue)
        if (c1 > c2)
          playRec(newOne.enqueueAll(Seq(c1, c2)), newTwo)
        else
          playRec(newOne, newTwo.enqueueAll(Seq(c2, c1)))
      }
    }

    playRec(p1, p2)
  }

  def score(deck: Deck): Int = {
    deck.reverse
      .zipWithIndex
      .map { case (v, i) => v * (i + 1) }
      .sum
  }

  def key(deck: Deck): String = {
    deck.mkString(",")
  }

  def checkInfiniteLoop(p: Player): Boolean = {
    p.history(key(p.deck))
  }


  @tailrec
  def rPlay(game: Game, display: Boolean = false): (Boolean, Deck) = {
    game.winner match {
      case Some(p1Won) => (p1Won, if (p1Won) game.p1.deck else game.p2.deck)
      case None =>
        val newState = game.playRound(display)
        rPlay(newState, display)
    }
  }

  def playRecursiveCombat(players: (Deck, Deck), display: Boolean = false): Deck = {
    val (p1, p2) = players
    val game = Game(
      id = "root",
      p1 = Player(p1),
      p2 = Player(p2),
    )
    val res = rPlay(game, display)
    res._2
  }
}
