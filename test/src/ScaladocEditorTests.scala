package explicitlyinferred

import utest.*

object ScaladocEditorTests extends TestSuite {
  private val documentation = EffectDocumentation(List("Alpha", "Zebra"), List("Result"))
  private val markers = Markers("types", "/types")

  val tests = Tests {
    test("creates a managed Scaladoc") {
      val expected =
        """  /**
          |   * <!-- types -->
          |   * Errors:
          |   *   - Alpha
          |   *   - Zebra
          |   *
          |   * Returns:
          |   *   - Result
          |   * <!-- /types -->
          |   */
          |""".stripMargin

      assert(ScaladocEditor.create("  ", documentation, markers, "\n") == expected)
    }

    test("updates a region while preserving prose and tags") {
      val input =
        """/** Existing documentation.
          | * <!-- types -->
          | * Errors:
          | *   - Stale
          | * <!-- /types -->
          | * @param value existing tag
          | */""".stripMargin

      val output = ScaladocEditor.update(input, "", documentation, markers, "\n")

      assert(output.contains("Existing documentation."))
      assert(output.contains("@param value existing tag"))
      assert(output.contains("*   - Alpha"))
      assert(!output.contains("Stale"))
      assert(output.indexOf("<!-- /types -->") < output.indexOf("@param"))
      assert(ScaladocEditor.update(output, "", documentation, markers, "\n") == output)
    }

    test("updates inline custom markers") {
      val custom = Markers("effect-types", "/effect-types")
      val input =
        """/** <!-- effect-types -->
          |  * Errors:
          |  *   - Stale
          |  * <!-- /effect-types -->
          |  */""".stripMargin

      val output = ScaladocEditor.update(input, "", documentation, custom, "\n")

      assert(output.split("<!-- effect-types -->", -1).length == 2)
      assert(output.split("<!-- /effect-types -->", -1).length == 2)
      assert(!output.contains("Stale"))
    }

    test("converts an attached block comment to Scaladoc") {
      val output = ScaladocEditor.update("/* keep me */", "  ", documentation, markers, "\n")

      assert(output.startsWith("/**"))
      assert(output.contains("keep me"))
      assert(output.contains("<!-- types -->"))
    }

    test("preserves CRLF") {
      val output = ScaladocEditor.create("", documentation, markers, "\r\n")
      assert(output.contains("\r\n"))
      assert(!output.replace("\r\n", "").contains('\n'))
    }

    test("does not consume an incomplete managed region") {
      val input =
        """/**
          | * <!-- types -->
          | * manual content
          | */""".stripMargin
      val output = ScaladocEditor.update(input, "", documentation, markers, "\n")

      assert(output.contains("manual content"))
      assert(output.split("<!-- types -->", -1).length == 3)
      assert(output.contains("<!-- /types -->"))
      assert(ScaladocEditor.update(output, "", documentation, markers, "\n") == output)
    }
  }
}
