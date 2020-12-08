package day08

import scala.annotation.tailrec

object GameConsole {

  case class Instruction(op: String, value: Int)

  def parseInstructions(input: List[String]): List[Instruction] = {
    input.map(s => {
      val Array(op, value) = s.split(" ")
      Instruction(op, value.toInt)
    })
  }

  def compute(instructions: List[Instruction]): (Int, Boolean) = {

    @tailrec
    def rec(index: Int, acc: Int, former: Set[Int]): (Int, Boolean) = {
      if (former(index)) (acc, false)
      else if (index == instructions.size) (acc, true)
      else instructions(index) match {
        case Instruction("nop", _) => rec(index + 1, acc, former + index)
        case Instruction("acc", v) => rec(index + 1, acc + v, former + index)
        case Instruction("jmp", v) => rec(index + v, acc, former + index)
        case _ => println(s"Error at $index, path: ${former.mkString(", ")}"); (0, false)
      }
    }

    rec(0, 0, Set.empty[Int])
  }

  def fixAndCompute(instructions: List[Instruction]): Int = {

    val indicesToFix = instructions.zipWithIndex
      .filter { case (inst, _) => Set("nop", "jmp")(inst.op) }
      .map(_._2)

    indicesToFix.map(i => {
      val newInstruction = instructions(i) match {
        case Instruction("nop", v) => Instruction("jmp", v)
        case Instruction("jmp", v) => Instruction("nop", v)
        case a: Instruction => a
      }
      instructions.patch(i, Seq(newInstruction), 1)
    })
      .map(compute)
      .find(_._2)
      .map(_._1)
      .getOrElse(0)
  }
}
