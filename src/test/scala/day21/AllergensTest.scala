package day21

import utils.BaseTestSuite
import day21.Allergens._
import utils.InputUtils.read

class AllergensTest extends BaseTestSuite {

  val example =
    """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
      |trh fvjkl sbzzf mxmxvkd (contains dairy)
      |sqjhc fvjkl (contains soy)
      |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin.split("\n").toList

  "Allergens" should "consolidate culprits" in {
    val culprits = Set(
      Culprit(Seq("mxmxvkd", "sqjhc"), "fish"),
      Culprit(Seq("mxmxvkd"), "dairy"),
      Culprit(Seq("sqjhc", "fvjkl"), "soy")
    )
    consolidateCulprits(culprits) should contain theSameElementsAs List(
      Culprit(Seq("sqjhc"), "fish"),
      Culprit(Seq("mxmxvkd"), "dairy"),
      Culprit(Seq("fvjkl"), "soy")
    )
  }

  it should "find the culprits (example)" in {
    val lines = parse(example)
    val culprits = findCulprits(lines)
    culprits should contain theSameElementsAs List(
      Culprit(Seq("mxmxvkd"), "dairy"),
      Culprit(Seq("sqjhc"), "fish"),
      Culprit(Seq("fvjkl"), "soy"),
    )
  }

  it should "count safe ingredients (example)" in {
    val lines = parse(example)
    countSafeIngredients(lines) shouldBe 5
  }

  it should "count safe ingredients - part 1" in {
    val lines = parse(read("day21.txt"))
    countSafeIngredients(lines) shouldBe 2317
  }

  it should "get the canonical culprits (example)" in {
    val lines = parse(example)
    val culprits = findCulprits(lines)
    culprits.map(c => c.name + " -> " + c.allergen)
      .foreach(println)
    canonicalCulprit(culprits) shouldBe "mxmxvkd,sqjhc,fvjkl"
  }

  it should "get the canonical culprits - part 2" in {
    val lines = parse(read("day21.txt"))
    val culprits = findCulprits(lines)
    culprits.map(c => c.name + " -> " + c.allergen)
      .foreach(println)
    canonicalCulprit(culprits) shouldBe "kbdgs,sqvv,slkfgq,vgnj,brdd,tpd,csfmb,lrnz"
  }
}
