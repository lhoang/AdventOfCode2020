package day21

import scala.annotation.tailrec

object Allergens {

  case class Line(ingr: Seq[String],alg: Seq[String])

  case class Culprit(name: Seq[String], allergen: String)

  def consolidateCulprits(culprits: Set[Culprit]): Set[Culprit]= {

    @tailrec
    def recConsolidate(current: Set[Culprit]): Set[Culprit] =
      if (current.forall(_.name.size == 1))
        current
      else {
        val (knownList, remaining) = current.partition(_.name.size == 1)
        val knownIngr = knownList.flatMap(_.name)
        recConsolidate(
          knownList ++ remaining
            .map(c => c.copy(name = c.name.filterNot(s => knownIngr(s))))
        )
      }

    recConsolidate(culprits)
  }

  def parse(input: List[String]): List[Line] = {
    val line = """([\w ]+) \(contains ([\w, ]+)\)""".r
    input.map { case line(ingredients, allergens) =>
      Line(ingredients.split(" ").toSeq, allergens.split(", ").toSeq)
    }
  }

  def findCulprits(lines:List[Line]): Set[Culprit]= {
    val allergens = lines.flatMap(_.alg).toSet

    val suspects = allergens.map(alg => {
      Culprit(name = lines.filter(_.alg.contains(alg))
         .map(_.ingr)
        .reduce((a, b) => a.intersect(b)),
        allergen = alg)
    })
    consolidateCulprits(suspects)
  }

  def countSafeIngredients(lines: List[Line]):Int = {
    val allIngredients = lines.flatMap(_.ingr).toSet
    val culprits = findCulprits(lines).map(_.name.head)
    val safe = allIngredients.filterNot(s => culprits(s))
    lines.flatMap(_.ingr).count(s => safe(s))
  }

  def canonicalCulprit(culprits: Set[Culprit]): String =
    culprits.toSeq.sortBy(_.allergen)
      .map(_.name.head)
      .mkString(",")
}
