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

  "Allergens" should "identify culprit ingredient" in {
    val lines = List(
      (Seq("mxmxvkd","kfcds","sqjhc","nhms"), Seq("dairy","fish")),
      (Seq("trh","fvjkl","sbzzf","mxmxvkd"), Seq("dairy")),
      (Seq("sqjhc","mxmxvkd","sbzzf"), Seq("dairy","fish")),
    )
    identifyCulprit("dairy", lines ) shouldBe Culprit("mxmxvkd", "dairy")
  }


  it should "remove culprit" in {
    val lines = List(
      (Seq("mxmxvkd","kfcds","sqjhc","nhms"), Seq("dairy","fish")),
      (Seq("trh","fvjkl","sbzzf","mxmxvkd"), Seq("dairy")),
      (Seq("sqjhc","mxmxvkd","sbzzf"), Seq("dairy","fish")),
    )
    removeKnownCulprit(Culprit("mxmxvkd", "dairy"), lines) shouldBe List(
      (Seq("kfcds","sqjhc","nhms"), Seq("fish")),
      (Seq("trh","fvjkl","sbzzf"), Seq()),
      (Seq("sqjhc","sbzzf"), Seq("fish")),
    )
  }

  it should "remove known safe ingredients" in {
    val lines = List(
      (Seq("mxmxvkd","kfcds","sqjhc","nhms"), Seq("dairy","fish")),
      (Seq("trh","fvjkl","sbzzf","mxmxvkd"), Seq("dairy")),
    )
    removeKnownSafe("sbzzf", lines) shouldBe List(
      (Seq("mxmxvkd","kfcds","sqjhc","nhms"), Seq("dairy","fish")),
      (Seq("trh","fvjkl","mxmxvkd"), Seq("dairy"))
    )
  }

  it should "find the culprits (example)" in {
    val lines = parse(example)
    val culprits = findCulprits(lines)
    culprits should contain theSameElementsAs List(
      Culprit("mxmxvkd", "dairy"),
      Culprit("sqjhc", "fish"),
      Culprit("fvjkl", "soy"),
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
    canonicalCulprit(culprits) shouldBe "mxmxvkd,sqjhc,fvjkl"
  }

  it should "get the canonical culprits - part 2" in {
    val lines = parse(read("day21.txt"))
    val culprits = findCulprits(lines)
    canonicalCulprit(culprits) shouldBe "lrnz,sqvv,csfmb,vgnj,kbdgs,tpd,brdd,slkfgq"
  }
}
