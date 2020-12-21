package day21

object Allergens {

  type Line = (Seq[String], Seq[String])

  case class Culprit(name: String, allergen: String)

  def identifyCulprit(alg: String, lines: List[Line]): Culprit = {
    val found = lines.filter(_._2.contains(alg))
      .flatMap(_._1)

    val res = found.groupMapReduce(identity)(_ => 1)(_ + _)
      .maxBy(_._2)
    Culprit(res._1, alg)
  }

  def removeKnownCulprit(culprit: Culprit, lines: List[Line]): List[Line] = {
    lines.map(line => {
      val (ings, algs) = line
      (ings.filterNot(_ == culprit.name), algs.filterNot(_ == culprit.allergen))
    })
  }

  def removeKnownSafe(safe: String, lines: List[Line]): List[Line] = {
    lines.map(line => {
      val (ings, algs) = line
      (ings.filterNot(_ == safe), algs)
    })
  }


  def parse(input: List[String]): List[Line] = {
    val line = """([\w ]+) \(contains ([\w, ]+)\)""".r
    input.map {
      case line(ingredients, allergens) =>
        ingredients.split(" ").toList -> allergens.split(", ").toList
    }
  }

  def findCulprits(lines:List[Line]): List[Culprit] = {
    val nbCulprits = lines.flatMap(_._2).toSet.size

    def rec(remaining: List[Line],
            culprits: List[Culprit]): List[Culprit] = {
      if (culprits.size == nbCulprits)
        culprits
      else {
        val (_, alg) = remaining.head
        if (alg.isEmpty)
          rec(remaining.tail, culprits)
        else {
          val currentAlg = alg.head
          val culprit = identifyCulprit(currentAlg, remaining)
          val cleaned = removeKnownCulprit(culprit, remaining)
          rec(cleaned, culprit :: culprits)
        }
      }
    }

    rec(lines, List.empty[Culprit])
  }

  def countSafeIngredients(lines: List[Line]):Int = {
    val allIngredients = lines.flatMap(_._1).toSet
    val culprits = findCulprits(lines).map(_.name).toSet
    val safe = allIngredients.filterNot(s => culprits(s))

    lines.flatMap(_._1).count(s => safe(s))
  }

  def canonicalCulprit(culprits: List[Culprit]): String =
    culprits.sortBy(_.allergen)
      .map(_.name)
      .mkString(",")
}
