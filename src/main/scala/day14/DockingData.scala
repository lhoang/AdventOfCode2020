package day14

object DockingData {

  case class State(mask:String, registry: Map[BigInt, BigInt])

  val State0: State = State(mask="X"*36, registry = Map.empty[BigInt, BigInt])

  def writeV1(state: State, address: Int, value: Int): State = {
    val registry = state.registry.updated(BigInt(address), applyMask(value.toInt, state.mask))
    state.copy(registry = registry)
  }

  def writeV2(state: State,address: Int, value: Int): State = {
    val registry = applyMaskAddress(address.toInt, state.mask)
      .foldLeft(state.registry)((reg, newAddress) => {
        reg.updated(newAddress, value)
      })
    state.copy(registry = registry)
  }

  def readLinesV1(input: List[String]): State = {
    readLine(input, writeV1)
  }

  def readLinesV2(input: List[String]): State = {
    readLine(input, writeV2)
  }

  private def readLine(input: List[String], method:(State,Int, Int) => State): State = {
    val mask = """mask = ([10X]{36})""".r
    val mem = """mem\[(\d+)\] = (\d+)""".r

    input.foldLeft(State0)((state, line) =>
      line match {
        case mask(m) => state.copy(mask = m)
        case mem(index, value) => method(state, index.toInt, value.toInt)
      }
    )
  }

  def to36bin(n: Int): String =
    ("0"*36+n.toBinaryString).takeRight(36)

  def applyMask(n:Int, mask: String) : BigInt = {
    val res = mask.zip(to36bin(n))
      .map{case (m, bit) => if (m.isDigit) m else bit}
      .mkString
    BigInt(res, 2)
  }

  def applyMaskAddress(n:Int, mask: String): List[BigInt] = {
    val address = mask.zip(to36bin(n))
      .map{
        case ('0', bit) => bit
        case (c, _) => c
      }
      .mkString
    generateAllAddresses(address)
  }

  def generateAllAddresses(address: String): List[BigInt] = {
    val res = address.foldRight(List(""))((c, acc) =>
      c match {
        case 'X' => acc.flatMap(s => Seq(0,1).map(_+s))
        case d => acc.map(d + _)
      }
    )
    res.map(c => BigInt(c, 2))
  }

  def computeSumValues(input:List[String], version: Int): BigInt = {
    val state = if (version == 1) readLinesV1(input)
                 else readLinesV2(input)
    state.registry.values.sum
  }

}
