package explicitlyinferred

import utest.*

object EffectDocumentationTests extends TestSuite {
  import CompilerPluginTestSupport.*

  val tests = Tests {
    test("renders sorted errors and a preserved result type") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |  object Errors {
          |    final class Zebra
          |    final class Alpha
          |  }
          |
          |  def value = null.asInstanceOf[Effect[Any, Errors.Zebra | Errors.Alpha, Option[String]]]
          |}
          |""".stripMargin
      val expected =
        """object Sample {
          |  final class Effect[R, E, A]
          |  object Errors {
          |    final class Zebra
          |    final class Alpha
          |  }
          |
          |  /**
          |   * <!-- types -->
          |   * Errors:
          |   *   - Errors.Alpha
          |   *   - Errors.Zebra
          |   *
          |   * Returns:
          |   *   - Option[String]
          |   * <!-- /types -->
          |   */
          |  def value = null.asInstanceOf[Effect[Any, Errors.Zebra | Errors.Alpha, Option[String]]]
          |}
          |""".stripMargin

      val once = rewrite(input, extraOptions = Seq("typeNameStyle=owner"))
      assert(once == expected)
      assert(rewrite(once, extraOptions = Seq("typeNameStyle=owner")) == expected)
    }

    test("dealiases errors and deduplicates expanded unions") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |  final class Alpha
          |  final class Zebra
          |  type Left = Zebra | Alpha
          |  type Right = Alpha | Zebra
          |  type Result = Option[String]
          |
          |  def value = null.asInstanceOf[Effect[Any, Left | Right, Result]]
          |}
          |""".stripMargin

      val output = rewrite(input)

      assert(output.linesIterator.count(_.contains("*   - Alpha")) == 1)
      assert(output.linesIterator.count(_.contains("*   - Zebra")) == 1)
      assert(!output.contains("*   - Left"))
      assert(!output.contains("*   - Right"))
      assert(output.contains("*   - Result"))
    }

    test("renders opaque aliases, tuples, and named tuples") {
      val input =
        """object Order {
          |  opaque type Id = String
          |}
          |
          |object Sample {
          |  final class Effect[R, E, A]
          |  def id = null.asInstanceOf[Effect[Any, Nothing, Order.Id]]
          |  def tuple = null.asInstanceOf[Effect[Any, Nothing, (Order.Id, Option[String])]]
          |  def named = null.asInstanceOf[
          |    Effect[Any, Nothing, NamedTuple.NamedTuple[Tuple1["pending"], Tuple1[Option[String]]]]
          |  ]
          |}
          |""".stripMargin

      val output = rewrite(input, extraOptions = Seq("typeNameStyle=owner"))

      assert(output.contains("*   - Order.Id"))
      assert(output.contains("*   - (Order.Id, Option[String])"))
      assert(output.contains("*   - (pending: Option[String])"))
    }

    test("adds and excludes errors") {
      val input =
        """object Sample {
          |  final class Effect[R, E, A]
          |  final class Internal[A]
          |  def value = null.asInstanceOf[Effect[Any, Internal[Int], Unit]]
          |}
          |""".stripMargin

      val output = rewrite(input, extraOptions = Seq(
        "additionalErrorType=UnexpectedError",
        "excludeErrorTypeRegex=.*Internal"
      ))

      assert(output.contains("*   - UnexpectedError"))
      assert(!output.contains("*   - Internal[Int]"))
      assert(output.contains("*   - Unit"))
    }

    test("uses configured error and result parameter names") {
      val input =
        """object Sample {
          |  final class Effect[Env, Failure, Success]
          |  def value = null.asInstanceOf[Effect[Any, String, Int]]
          |}
          |""".stripMargin

      val output = rewrite(input, extraOptions = Seq(
        "errorTypeParam=Failure",
        "resultTypeParam=Success"
      ))

      assert(output.contains("*   - String"))
      assert(output.contains("*   - Int"))
    }

    test("ignores nonmatching effect constructors") {
      val input =
        """object Sample {
          |  final class Other[E, A]
          |  def value = null.asInstanceOf[Other[String, Int]]
          |}
          |""".stripMargin

      assert(rewrite(input) == input)
    }

    test("renders a large balanced union of intersections") {
      val memberCount = 64
      val declarations = (0 until memberCount)
        .flatMap(index => Seq(s"  trait Left$index", s"  trait Right$index"))
        .mkString("\n")
      val members = (0 until memberCount).map(index => s"Left$index & Right$index")
      val errorType = balanced("|", members)
      val input =
        s"""object Sample {
           |  final class Effect[R, E, A]
           |$declarations
           |  def value = null.asInstanceOf[Effect[Any, $errorType, Int]]
           |}
           |""".stripMargin

      val output = rewrite(input)

      assert(output.linesIterator.count(_.contains("*   - ")) == memberCount + 1)
      assert(output.contains("Left0 & Right0"))
      assert(output.contains("Left63 & Right63"))
    }
  }

  private def balanced(operator: String, values: IndexedSeq[String]): String =
    if values.size == 1 then values.head
    else
      val (left, right) = values.splitAt(values.size / 2)
      s"(${balanced(operator, left)} $operator ${balanced(operator, right)})"
}
