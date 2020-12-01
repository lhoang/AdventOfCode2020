package utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Inside, OptionValues}

abstract class BaseTestSuite extends AnyFlatSpec with Matchers
  with OptionValues with Inside