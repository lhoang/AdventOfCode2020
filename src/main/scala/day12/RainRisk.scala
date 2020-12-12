package day12

import math._

object RainRisk {

  case class Instruction(op: String, value: Int)

  abstract class Ship {
    def moveX(value: Int): Ship

    def moveY(value: Int): Ship

    def moveForward(value: Int): Ship

    def rotate(value: Int): Ship

    def move(current: Instruction): Ship =
      current match {
        case Instruction("N", v) => this.moveY(v)
        case Instruction("S", v) => this.moveY(-v)
        case Instruction("E", v) => this.moveX(v)
        case Instruction("W", v) => this.moveX(-v)
        case Instruction("L", v) => this.rotate(v)
        case Instruction("R", v) => this.rotate(-v)
        case Instruction("F", v) => this.moveForward(v)
      }
  }

  /**
   * Position of the ship
   *
   * @param x     coord West -> East
   * @param y     coord South ->  North
   * @param angle angle in degrees where 0° is full east
   */
  case class Pos(x: Int, y: Int, angle: Int) extends Ship {

    def moveX(value: Int): Pos = this.copy(x = this.x + value)

    def moveY(value: Int): Pos = this.copy(y = this.y + value)

    def moveForward(value: Int): Pos = {
      val dx = cos(angle.toRadians).round.toInt
      val dy = sin(angle.toRadians).round.toInt
      this.moveX(dx * value).moveY(dy * value)
    }

    def rotate(value: Int): Pos = this.copy(angle = angle + value)

    override def move(current: Instruction): Pos = super.move(current).asInstanceOf[Pos]
  }

  /**
   * Position of the ship
   *
   * @param x  coord West -> East
   * @param y  coord South ->  North
   * @param dx waypoint West -> East
   * @param dy waypoint South ->  North
   */
  case class WayPoint(x: Int, y: Int, dx: Int, dy: Int) extends Ship {
    def moveX(value: Int): WayPoint = this.copy(dx = this.dx + value)

    def moveY(value: Int): WayPoint = this.copy(dy = this.dy + value)

    def rotate(value: Int): WayPoint =
      if (Set(90, -270)(value))
        this.copy(dx = -dy, dy = dx)
      else if (Set(180, -180)(value))
        this.copy(dx = -dx, dy = -dy)
      else if (Set(270, -90)(value))
        this.copy(dx = dy, dy = -dx)
      else throw new IllegalArgumentException(s"Rotate: $value")

    def moveForward(value: Int): WayPoint = {
      this.copy(x = this.x + dx * value, y = this.y + dy * value)
    }

    override def move(current: Instruction): WayPoint = super.move(current).asInstanceOf[WayPoint]

  }

  def parseInstructions(lines: List[String]): List[Instruction] = {
    val regex = """^([NSEWLRF])(\d+)$""".r
    lines.map {
      case regex(op, v) => Instruction(op, v.toInt)
      case _ => throw new IllegalArgumentException
    }
  }

  def moveShip(instructions: List[Instruction]): Pos = {
    instructions.foldLeft(Pos(0, 0, 0))((pos, current) => pos.move(current))
  }

  def moveShipWayPoint(instructions: List[Instruction]): WayPoint = {
    instructions.foldLeft(WayPoint(0, 0, 10, 1))((wp, current) => wp.move(current))
  }

  def computeFinaleManhattanDistance(input: List[String]): Int = {
    val pos = moveShip(parseInstructions(input))
    pos.x.abs + pos.y.abs
  }

  def computeFinaleManhattanDistanceWayPoint(input: List[String]): Int = {
    val wp = moveShipWayPoint(parseInstructions(input))
    wp.x.abs + wp.y.abs
  }

}