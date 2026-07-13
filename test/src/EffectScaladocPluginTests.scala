import utest.*

object EffectScaladocPluginTests extends TestSuite {
  import CompilerPluginTestSupport.*

  private val effectOptions = Seq(
    "mode=effectScaladoc",
    "effectTypeRegex=.*Effect",
    "typeNameStyle=owner"
  )

  val tests = Tests {
    test("writes sorted error and return entries as Scaladoc and stays idempotent") {
      val input =
        s"""object Sample {
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
        s"""object Sample {
           |  final class Effect[R, E, A]
           |  object Errors {
           |    final class Zebra
           |    final class Alpha
           |  }
           |
           |  /**
           |   * <!-- explicitly-inferred:start -->
           |   * Errors:
           |   *   - Errors.Alpha
           |   *   - Errors.Zebra
           |   *
           |   * Returns:
           |   *   - Option[String]
           |   * <!-- explicitly-inferred:end -->
           |   */
           |  def value = null.asInstanceOf[Effect[Any, Errors.Zebra | Errors.Alpha, Option[String]]]
           |}
           |""".stripMargin

      val once = rewrite(input, extraOptions = effectOptions)
      assert(once == expected)
      assert(rewrite(once, extraOptions = effectOptions) == expected)
    }

    test("adds public errors, removes internal errors, and renders Nothing") {
      val input =
        s"""object Sample {
           |  final class Effect[R, E, A]
           |  final class ShortCircuit[A]
           |
           |  def value = null.asInstanceOf[Effect[Any, ShortCircuit[Int], Unit]]
           |  def empty = null.asInstanceOf[Effect[Any, Nothing, Unit]]
           |}
           |""".stripMargin

      val options = effectOptions ++ Seq(
        "additionalErrorType=UnexpectedError",
        "excludeErrorTypeRegex=.*ShortCircuit"
      )
      val output = rewrite(input, extraOptions = options)

      assert(output.contains("*   - UnexpectedError"))
      assert(!output.contains("*   - Sample.ShortCircuit[Int]"))
      assert(output.split("Errors:", -1).length == 3)
    }

    test("preserves manual Scaladoc content and tags") {
      val input =
        s"""object Sample {
           |  final class Effect[R, E, A]
           |
           |  /** Existing documentation.
           |   * @param input existing tag
           |   */
           |  def value(input: Int) = null.asInstanceOf[Effect[Any, Nothing, Int]]
           |}
           |""".stripMargin

      val output = rewrite(input, extraOptions = effectOptions)

      assert(output.contains("Existing documentation."))
      assert(output.contains("@param input existing tag"))
      assert(output.indexOf("explicitly-inferred:end") < output.indexOf("@param input"))
      assert(output.contains("*   - Nothing"))
      assert(output.contains("*   - Int"))
    }

    test("renders owner-qualified aliases, tuples, and named tuples in source syntax") {
      val input =
        s"""object Order {
           |  opaque type Id = String
           |  def id: Id = "order"
           |}
           |
           |object Sample {
           |  final class Effect[R, E, A]
           |
           |  def id = null.asInstanceOf[Effect[Any, Nothing, Order.Id]]
           |  def tuple = null.asInstanceOf[Effect[Any, Nothing, (Order.Id, Option[String])]]
           |  def named = null.asInstanceOf[
           |    Effect[Any, Nothing, NamedTuple.NamedTuple[Tuple1["pending"], Tuple1[Option[String]]]]
           |  ]
           |}
           |""".stripMargin

      val output = rewrite(input, extraOptions = effectOptions)

      assert(output.contains("*   - Order.Id"))
      assert(output.contains("*   - (Order.Id, Option[String])"))
      assert(output.contains("*   - (pending: Option[String])"))
    }

    test("ignores inferred methods whose effect constructor does not match") {
      val input =
        s"""object Sample {
           |  def value = Option(1)
           |}
           |""".stripMargin

      assert(rewrite(input, extraOptions = effectOptions) == input)
    }

    test("updates Scalafmt-style inline opening markers without duplicating the region") {
      val input =
        s"""object Sample {
           |  final class Effect[R, E, A]
           |
           |  /** <!-- explicitly-inferred:start -->
           |    * Errors:
           |    *
           |    *   - `String`
           |    *
           |    * Returns:
           |    *
           |    *   - `String`
           |    * <!-- explicitly-inferred:end -->
           |    */
           |  def value = null.asInstanceOf[Effect[Any, Nothing, Int]]
           |}
           |""".stripMargin

      val output = rewrite(input, extraOptions = effectOptions)

      assert(output.split("explicitly-inferred:start", -1).length == 2)
      assert(output.contains("*   - Nothing"))
      assert(output.contains("*   - Int"))
      assert(!output.contains("*   - String"))
    }
  }
}
