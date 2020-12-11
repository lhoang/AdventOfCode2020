package day11

import math._
import scala.annotation.tailrec
import scala.util._

object SeatingSystem {

  type Room = List[String]

  def countAdjacentOccupiedSeats(x: Int, y: Int, room: Room): Int = {
    val width = room.head.length - 1
    val height = room.length - 1
    val occupied = for {
      j <- max(y-1, 0) to min(y+1, height)
      i <- max(x-1, 0) to min(x+1, width)
      if i!=x || j!=y
      if room(j)(i) == '#'
    } yield (i,j)
    //println(occupied.mkString(", "))
    occupied.size
  }

  def countVisibleOccupiedSeats(x: Int, y: Int, room: Room): Int = {
    @tailrec
    def visible(i: Int, j: Int, dirX: Int, dirY: Int): Char =
      Try(room(j)(i)) match {
        case Success('.') => visible(i+dirX, j+dirY, dirX, dirY)
        case Success(c) => c
        case Failure(_) => '.'
      }

    val occupied = for {
      dirX <- -1 to 1
      dirY <- -1 to 1
      if dirX!=0 || dirY!=0
    } yield visible(x+dirX, y+dirY, dirX, dirY)
    //println(occupied.mkString(", "))
    occupied.count(_ == '#')
  }


  def playRoundAdjacent(room: Room, display: Boolean = false): Room = {
    playRound(room, method = countAdjacentOccupiedSeats, limit = 4, display)
  }

  def playRoundVisible(room: Room, display: Boolean = false): Room = {
    playRound(room, method = countVisibleOccupiedSeats, limit = 5, display)
  }

  private def playRound(room: Room, method:(Int, Int, Room) => Int, limit:Int  ,display: Boolean) = {
    val width = room.head.length - 1
    val height = room.length - 1
    val newRoom = for {
      j <- 0 to height
      i <- 0 to width
    } yield {
      val occupied = method(i, j, room)
      room(j)(i) match {
        case 'L' if occupied == 0 => '#'
        case '#' if occupied >= limit => 'L'
        case c => c
      }
    }
    val res = newRoom.grouped(width + 1)
      .map(_.mkString)
      .toList
    if (display) displayRoom(res)
    res
  }

  def displayRoom(room: Room): Unit = {
    println
    room.foreach(println)
    println
    println
  }

  def stabilizeAndCountAdjacent(room: Room, display: Boolean = false): Int =
    stabilizeAndCount(room,method=playRoundAdjacent(_, display))

  def stabilizeAndCountVisible(room: Room, display: Boolean = false): Int =
    stabilizeAndCount(room,method=playRoundVisible(_, display))

  def stabilizeAndCount(room: Room,method: Room => Room): Int = {
    @tailrec
    def rec(current: Room):Int = {
      val newRoom = method(current)
      if (newRoom == current)
        current.mkString.count(_ == '#')
      else rec(newRoom)
    }
    rec(room)
  }

}
