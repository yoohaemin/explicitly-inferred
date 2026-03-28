import utest.*

object InferredReturnCommentPluginTests extends TestSuite {
  import CompilerPluginTestSupport.*

  val tests = Tests {
    test("inserts a block comment for an inferred member def and stays idempotent") {
      val input =
        s"""object Sample {
           |  def value = List(1, 2, 3)
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  /*
           |   * @inferredReturnType List[A = Int]
           |   */
           |  def value = List(1, 2, 3)
           |}
           |""".stripMargin

      val once = rewrite(input)
      assert(once == expected)
      assert(rewrite(once) == expected)
    }

    test("updates an existing block comment in place") {
      val input =
        s"""object Sample {
           |  /*
           |   * keep me
           |   */
           |  def value = Option(1)
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  /*
           |   * keep me
           |   * @inferredReturnType Option[A = Int]
           |   */
           |  def value = Option(1)
           |}
           |""".stripMargin

      assert(rewrite(input) == expected)
    }

    test("inserts between line comments and def") {
      val input =
        s"""object Sample {
           |  // keep me
           |  def value = 1
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  // keep me
           |  /*
           |   * @inferredReturnType Int
           |   */
           |  def value = 1
           |}
           |""".stripMargin

      assert(rewrite(input) == expected)
    }

    test("leaves explicit return types untouched") {
      val input =
        s"""object Sample {
           |  def value: Int = 1
           |}
           |""".stripMargin

      assert(rewrite(input) == input)
    }

    test("scope option controls local defs") {
      val input =
        s"""object Sample {
           |  def outer =
           |    def inner = 1
           |    inner
           |}
           |""".stripMargin

      val defaultExpected =
        s"""object Sample {
           |  /*
           |   * @inferredReturnType Int
           |   */
           |  def outer =
           |    def inner = 1
           |    inner
           |}
           |""".stripMargin

      val allExpected =
        s"""object Sample {
           |  /*
           |   * @inferredReturnType Int
           |   */
           |  def outer =
           |    /*
           |     * @inferredReturnType Int
           |     */
           |    def inner = 1
           |    inner
           |}
           |""".stripMargin

      assert(rewrite(input) == defaultExpected)
      assert(rewrite(input, extraOptions = Seq("scope=all")) == allExpected)
    }

    test("method regex filters matching defs") {
      val input =
        s"""object Sample {
           |  def keep = 1
           |  def skip = 2
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  /*
           |   * @inferredReturnType Int
           |   */
           |  def keep = 1
           |  def skip = 2
           |}
           |""".stripMargin

      assert(rewrite(input, methodRegex = "keep") == expected)
    }

    test("normalizes aliases and orders union members deterministically") {
      val input =
        s"""object Sample {
           |  final class Foo
           |  final class Bar
           |  type Alias = Bar | Foo
           |
           |  def value = null.asInstanceOf[Alias]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  final class Foo
           |  final class Bar
           |  type Alias = Bar | Foo
           |
           |  /*
           |   * @inferredReturnType Foo | Bar
           |   */
           |  def value = null.asInstanceOf[Alias]
           |}
           |""".stripMargin

      assert(rewrite(input) == expected)
    }

    test("normalizes aliases and orders intersections deterministically") {
      val input =
        s"""object Sample {
           |  trait Foo
           |  trait Bar
           |  type Alias = Bar & Foo
           |
           |  def value = null.asInstanceOf[Alias]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  trait Foo
           |  trait Bar
           |  type Alias = Bar & Foo
           |
           |  /*
           |   * @inferredReturnType Foo & Bar
           |   */
           |  def value = null.asInstanceOf[Alias]
           |}
           |""".stripMargin

      assert(rewrite(input) == expected)
    }

    test("renders type arguments in named or positional form") {
      val input =
        s"""object Sample {
           |  final class Box[A]
           |
           |  def value = new Box[Int]
           |}
           |""".stripMargin

      val namedExpected =
        s"""object Sample {
           |  final class Box[A]
           |
           |  /*
           |   * @inferredReturnType Box[A = Int]
           |   */
           |  def value = new Box[Int]
           |}
           |""".stripMargin

      val positionalExpected =
        s"""object Sample {
           |  final class Box[A]
           |
           |  /*
           |   * @inferredReturnType Box[Int]
           |   */
           |  def value = new Box[Int]
           |}
           |""".stripMargin

      assert(rewrite(input) == namedExpected)
      assert(rewrite(input, extraOptions = Seq("showTypeParamNames=false")) == positionalExpected)
    }

    test("can suppress type arguments entirely") {
      val input =
        s"""object Sample {
           |  final class Box[A]
           |
           |  def value = new Box[Int]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  final class Box[A]
           |
           |  /*
           |   * @inferredReturnType Box
           |   */
           |  def value = new Box[Int]
           |}
           |""".stripMargin

      assert(rewrite(input, extraOptions = Seq("showTypeArgs=false")) == expected)
    }

    test("wraps long normalized types into a multiline payload block and stays idempotent") {
      val input =
        s"""object Sample {
           |  final class Box[A]
           |  final class Wrap[A]
           |  type Alias = Wrap[String] | Box[Int]
           |
           |  def value = null.asInstanceOf[Alias]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  final class Box[A]
           |  final class Wrap[A]
           |  type Alias = Wrap[String] | Box[Int]
           |
           |  /*
           |   * @inferredReturnType
           |   *   Box[A = Int] |
           |   *   Wrap[A = String]
           |   */
           |  def value = null.asInstanceOf[Alias]
           |}
           |""".stripMargin

      val once = rewrite(input, extraOptions = Seq("maxTypeLength=20"))
      assert(once == expected)
      assert(rewrite(once, extraOptions = Seq("maxTypeLength=20")) == expected)
    }

    test("replaces an existing managed multiline block entry in place") {
      val input =
        s"""object Sample {
           |  final class Box[A]
           |  final class Wrap[A]
           |
           |  /*
           |   * keep me
           |   * @inferredReturnType
           |   *   stale
           |   */
           |  def value = null.asInstanceOf[Wrap[String] | Box[Int]]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  final class Box[A]
           |  final class Wrap[A]
           |
           |  /*
           |   * keep me
           |   * @inferredReturnType
           |   *   Box[A = Int] |
           |   *   Wrap[A = String]
           |   */
           |  def value = null.asInstanceOf[Wrap[String] | Box[Int]]
           |}
           |""".stripMargin

      assert(rewrite(input, extraOptions = Seq("maxTypeLength=20")) == expected)
    }

    test("overwrites an existing managed comment when its type or formatting is wrong") {
      val input =
        s"""object Sample {
           |  final class Box[A]
           |  final class Wrap[A]
           |
           |  /*
           |   * keep me
           |   * @inferredReturnType Wrap[A = String] | Box[A = Int]
           |   */
           |  def value = null.asInstanceOf[Wrap[String] | Box[Int]]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  final class Box[A]
           |  final class Wrap[A]
           |
           |  /*
           |   * keep me
           |   * @inferredReturnType
           |   *   Box[A = Int] |
           |   *   Wrap[A = String]
           |   */
           |  def value = null.asInstanceOf[Wrap[String] | Box[Int]]
           |}
           |""".stripMargin

      assert(rewrite(input, extraOptions = Seq("maxTypeLength=20")) == expected)
    }

    test("rejects unknown options") {
      rewriteExpectFailure(
        """object Sample {
          |  def value = 1
          |}
          |""".stripMargin,
        extraOptions = Seq("bogus=true")
      )
    }

    test("rejects invalid method regex") {
      rewriteExpectFailure(
        """object Sample {
          |  def value = 1
          |}
          |""".stripMargin,
        methodRegex = "("
      )
    }

    test("rejects invalid maxTypeLength") {
      rewriteExpectFailure(
        """object Sample {
          |  def value = 1
          |}
          |""".stripMargin,
        extraOptions = Seq("maxTypeLength=0")
      )
    }

    test("updates an existing single-line block comment in place") {
      val input =
        s"""object Sample {
           |  /* keep me */
           |  def value = 1
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  /*
           |   * keep me
           |   * @inferredReturnType Int
           |   */
           |  def value = 1
           |}
           |""".stripMargin

      assert(rewrite(input) == expected)
    }

    test("updates an existing scaladoc comment in place") {
      val input =
        s"""object Sample {
           |  /**
           |   * keep me
           |   */
           |  def value = 1
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  /**
           |   * keep me
           |   * @inferredReturnType Int
           |   */
           |  def value = 1
           |}
           |""".stripMargin

      assert(rewrite(input) == expected)
    }

    test("inserts below contiguous line comments") {
      val input =
        s"""object Sample {
           |  // keep one
           |  // keep two
           |  def value = 1
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  // keep one
           |  // keep two
           |  /*
           |   * @inferredReturnType Int
           |   */
           |  def value = 1
           |}
           |""".stripMargin

      assert(rewrite(input) == expected)
    }

    test("blank lines break comment attachment") {
      val input =
        s"""object Sample {
           |  /*
           |   * keep me
           |   */
           |
           |  def value = 1
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  /*
           |   * keep me
           |   */
           |
           |  /*
           |   * @inferredReturnType Int
           |   */
           |  def value = 1
           |}
           |""".stripMargin

      assert(rewrite(input) == expected)
    }

    test("scope nonPrivate skips private members and keeps protected members") {
      val input =
        s"""class Sample {
           |  private def hidden = 1
           |  protected def visible = 2
           |}
           |""".stripMargin

      val expected =
        s"""class Sample {
           |  private def hidden = 1
           |  /*
           |   * @inferredReturnType Int
           |   */
           |  protected def visible = 2
           |}
           |""".stripMargin

      assert(rewrite(input, extraOptions = Seq("scope=nonPrivate")) == expected)
    }

    test("multiple eligible methods in one file are all rewritten") {
      val input =
        s"""object Sample {
           |  def first = 1
           |  def second = List(1)
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  /*
           |   * @inferredReturnType Int
           |   */
           |  def first = 1
           |  /*
           |   * @inferredReturnType List[A = Int]
           |   */
           |  def second = List(1)
           |}
           |""".stripMargin

      assert(rewrite(input) == expected)
    }

    test("compile without rewrite leaves source untouched") {
      val input =
        s"""object Sample {
           |  def value = 1
           |}
           |""".stripMargin

      assert(compileWithoutRewrite(input) == input)
    }

    test("multiple source files are rewritten in one compiler run") {
      val inputs = Seq(
        "Alpha.scala" ->
          s"""object Alpha {
             |  def value = 1
             |}
             |""".stripMargin,
        "Beta.scala" ->
          s"""object Beta {
             |  def value = List(1)
             |}
             |""".stripMargin
      )

      val outputs = rewriteFiles(inputs)
      assert(
        outputs("Alpha.scala") ==
          s"""object Alpha {
             |  /*
             |   * @inferredReturnType Int
             |   */
             |  def value = 1
             |}
             |""".stripMargin
      )
      assert(
        outputs("Beta.scala") ==
          s"""object Beta {
             |  /*
             |   * @inferredReturnType List[A = Int]
             |   */
             |  def value = List(1)
             |}
             |""".stripMargin
      )
    }

    test("duplicate managed entries collapse to one correct entry") {
      val input =
        s"""object Sample {
           |  /*
           |   * keep me
           |   * @inferredReturnType String
           |   * @inferredReturnType Int
           |   */
           |  def value = 1
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  /*
           |   * keep me
           |   * @inferredReturnType Int
           |   */
           |  def value = 1
           |}
           |""".stripMargin

      assert(rewrite(input) == expected)
    }

    test("changing formatting options rewrites an existing managed entry") {
      val input =
        s"""object Sample {
           |  final class Box[A]
           |
           |  def value = new Box[Int]
           |}
           |""".stripMargin

      val named = rewrite(input)
      val positional =
        s"""object Sample {
           |  final class Box[A]
           |
           |  /*
           |   * @inferredReturnType Box[Int]
           |   */
           |  def value = new Box[Int]
           |}
           |""".stripMargin
      val hidden =
        s"""object Sample {
           |  final class Box[A]
           |
           |  /*
           |   * @inferredReturnType Box
           |   */
           |  def value = new Box[Int]
           |}
           |""".stripMargin

      assert(rewrite(named, extraOptions = Seq("showTypeParamNames=false")) == positional)
      assert(rewrite(positional, extraOptions = Seq("showTypeArgs=false")) == hidden)
    }

    test("annotations are preserved when a managed block is inserted") {
      val input =
        s"""object Sample {
           |  @deprecated("x", "y")
           |  def value = 1
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  /*
           |   * @inferredReturnType Int
           |   */
           |  @deprecated("x", "y")
           |  def value = 1
           |}
           |""".stripMargin

      assert(rewrite(input) == expected)
    }

    test("preserves CRLF when rewriting raw source") {
      val input = "object Sample {\r\n  def value = 1\r\n}\r\n"
      val expected = "object Sample {\r\n  /*\r\n   * @inferredReturnType Int\r\n   */\r\n  def value = 1\r\n}\r\n"

      assert(rewriteRaw(input) == expected)
    }

    test("repeated options use the last value") {
      val input =
        s"""object Sample {
           |  final class Box[A]
           |
           |  def value = new Box[Int]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  final class Box[A]
           |
           |  /*
           |   * @inferredReturnType Box[Int]
           |   */
           |  def value = new Box[Int]
           |}
           |""".stripMargin

      assert(rewrite(input, extraOptions = Seq("showTypeParamNames=true", "showTypeParamNames=false")) == expected)
    }
  }

}
