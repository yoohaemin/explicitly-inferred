package explicitlyinferred

import utest.*

object ExplicitlyInferredPluginTests extends TestSuite {
  import CompilerPluginTestSupport.*

  val tests = Tests {
    test("preserves prose and tags in attached Scaladoc") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |
          |  /** Existing documentation.
          |   * @param input existing tag
          |   */
          |  def value(input: Int) = null.asInstanceOf[Effect[Any, Nothing, Int]]
          |}
          |""".stripMargin

      val output = rewrite(input)

      assert(output.contains("Existing documentation."))
      assert(output.contains("@param input existing tag"))
      assert(output.indexOf("<!-- /types -->") < output.indexOf("@param input"))
    }

    test("finds attached block comments throughout a compilation unit") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |  /** first */
          |  def first = null.asInstanceOf[Effect[Any, Nothing, Int]]
          |
          |  /** middle */
          |  def middle = null.asInstanceOf[Effect[Any, Nothing, String]]
          |
          |  /** last */
          |  def last = null.asInstanceOf[Effect[Any, Nothing, Long]]
          |}
          |""".stripMargin

      val output = rewrite(input)

      assert(output.contains("first"))
      assert(output.contains("middle"))
      assert(output.contains("last"))
      assert(output.split("<!-- types -->", -1).length == 4)
    }

    test("does not attach an older block comment past a nearer line comment") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |  /** old block */
          |  // nearest comment
          |  def value = null.asInstanceOf[Effect[Any, Nothing, Int]]
          |}
          |""".stripMargin

      val output = rewrite(input)

      assert(output.contains("/** old block */"))
      assert(output.indexOf("// nearest comment") < output.indexOf("<!-- types -->"))
      assert(output.indexOf("<!-- types -->") < output.indexOf("def value"))
    }

    test("inserts above multiline annotations") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |  @deprecated(
          |    "old",
          |    "1.0"
          |  )
          |  def value = null.asInstanceOf[Effect[Any, Nothing, Int]]
          |}
          |""".stripMargin

      val output = rewrite(input)

      assert(output.indexOf("<!-- types -->") < output.indexOf("@deprecated"))
      assert(output.indexOf("@deprecated") < output.indexOf("def value"))
    }

    test("applies method matching and rewrites") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |  def `prefix.keep` = null.asInstanceOf[Effect[Any, Nothing, Int]]
          |  def `prefix.skip` = null.asInstanceOf[Effect[Any, Nothing, Int]]
          |}
          |""".stripMargin
      val options = Seq(
        "methodRegexRewrite=$1",
        "methodRegex=keep"
      )

      val output = rewrite(input, extraOptions = options, methodRegex = "prefix\\.(.*)")

      assert(output.split("<!-- types -->", -1).length == 2)
      assert(output.indexOf("<!-- types -->") < output.indexOf("def `prefix.keep`"))
    }

    test("scope controls local methods") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |  def outer =
          |    def inner = null.asInstanceOf[Effect[Any, Nothing, Int]]
          |    inner
          |}
          |""".stripMargin

      val members = rewrite(input)
      val all = rewrite(input, extraOptions = Seq("scope=all"))

      assert(members.split("<!-- types -->", -1).length == 2)
      assert(all.split("<!-- types -->", -1).length == 3)
    }

    test("leaves explicit return types untouched") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |  def value: Effect[Any, Nothing, Int] = null
          |}
          |""".stripMargin

      assert(rewrite(input) == input)
    }

    test("rewrites multiple source files") {
      val sources = Seq(
        "One.scala" -> "object One { final class Effect[R, E, A]; def one = null.asInstanceOf[Effect[Any, Nothing, Int]] }\n",
        "Two.scala" -> "object Two { final class Effect[R, E, A]; def two = null.asInstanceOf[Effect[Any, String, Int]] }\n"
      )

      val output = rewriteFiles(sources)

      assert(output.values.forall(_.contains("<!-- types -->")))
    }

    test("does not modify sources without -rewrite") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |  def value = null.asInstanceOf[Effect[Any, Nothing, Int]]
          |}
          |""".stripMargin

      assert(compileWithoutRewrite(input) == input)
    }

    test("uses custom markers through the compiler options") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |  def value = null.asInstanceOf[Effect[Any, Nothing, Int]]
          |}
          |""".stripMargin

      val output = rewrite(input, extraOptions = Seq("startMarker=effect-types", "endMarker=/effect-types"))

      assert(output.contains("<!-- effect-types -->"))
      assert(output.contains("<!-- /effect-types -->"))
    }
  }
}
