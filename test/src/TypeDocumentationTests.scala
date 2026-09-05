package explicitlyinferred

import utest.*

object TypeDocumentationTests extends TestSuite {
  import CompilerPluginTestSupport.*

  val tests = Tests {
    test("renders ordered mapped sections") {
      val input =
        """object Sample {
          |  final class Container[C, L, R]
          |  object Members {
          |    final class Zebra
          |    final class Alpha
          |  }
          |
          |  def value = null.asInstanceOf[Container[Any, Members.Zebra | Members.Alpha, Option[String]]]
          |}
          |""".stripMargin
      val expected =
        """object Sample {
          |  final class Container[C, L, R]
          |  object Members {
          |    final class Zebra
          |    final class Alpha
          |  }
          |
          |  /**
          |   * <!-- types -->
          |   * Left:
          |   *   - Members.Alpha
          |   *   - Members.Zebra
          |   *
          |   * Right:
          |   *   - Option[String]
          |   * <!-- /types -->
          |   */
          |  def value = null.asInstanceOf[Container[Any, Members.Zebra | Members.Alpha, Option[String]]]
          |}
          |""".stripMargin

      val once = rewrite(input, extraOptions = Seq("typeNameStyle=owner"))
      assert(once == expected)
      assert(rewrite(once, extraOptions = Seq("typeNameStyle=owner")) == expected)
    }

    test("applies each mapping's alias policy") {
      val input =
        """object Sample {
          |  final class Pair[First, Second]
          |  final class Alpha
          |  final class Zebra
          |  type ExpandedAlias = Zebra | Alpha
          |  type PreservedAlias = Option[String]
          |  def value = null.asInstanceOf[Pair[ExpandedAlias, PreservedAlias]]
          |}
          |""".stripMargin
      val mappings = Seq(
        "typeParam=First:Expanded:dealias",
        "typeParam=Second:Preserved:preserve"
      )

      val output = rewrite(input, typeParameters = mappings)

      assert(output.contains("* Expanded:"))
      assert(output.contains("*   - Alpha"))
      assert(output.contains("*   - Zebra"))
      assert(output.contains("* Preserved:"))
      assert(output.contains("*   - PreservedAlias"))
    }

    test("renders opaque aliases tuples and named tuples") {
      val input =
        """object Domain {
          |  opaque type Id = String
          |}
          |
          |object Sample {
          |  final class Container[C, L, R]
          |  def id = null.asInstanceOf[Container[Any, Nothing, Domain.Id]]
          |  def tuple = null.asInstanceOf[Container[Any, Nothing, (Domain.Id, Option[String])]]
          |  def named = null.asInstanceOf[
          |    Container[Any, Nothing, NamedTuple.NamedTuple[Tuple1["pending"], Tuple1[Option[String]]]]
          |  ]
          |}
          |""".stripMargin

      val output = rewrite(input, extraOptions = Seq("typeNameStyle=owner"))

      assert(output.contains("*   - Domain.Id"))
      assert(output.contains("*   - (Domain.Id, Option[String])"))
      assert(output.contains("*   - (pending: Option[String])"))
    }

    test("preserves the concrete owner of inherited opaque Type aliases") {
      val dependency =
        """import neotype.Subtype
          |
          |object FirstOwner {
          |  object Code extends Subtype[String]
          |}
          |
          |object SecondOwner {
          |  object Code extends Subtype[String]
          |}
          |""".stripMargin
      val input =
        """object Sample {
          |
          |  final class Container[C, L, R]
          |  def code = null.asInstanceOf[
          |    Container[Any, Nothing, FirstOwner.Code.Type | SecondOwner.Code.Type]
          |  ]
          |}
          |""".stripMargin

      val output = rewriteWithPrecompiledSource(dependency, input, extraOptions = Seq("typeNameStyle=owner"))

      assert(output.contains("*   - FirstOwner.Code"))
      assert(output.contains("*   - SecondOwner.Code"))
      assert(!output.contains("*   - Subtype.Type"))
    }

    test("hides synthetic package owners for top-level aliases") {
      val dependency =
        """package domain
          |
          |import neotype.Subtype
          |
          |type Timestamp = Timestamp.Type
          |object Timestamp extends Subtype[java.time.Instant]
          |""".stripMargin
      val input =
        """package usage
          |
          |import domain.Timestamp
          |
          |object Sample {
          |  final class Container[C, L, R]
          |  def timestamp = null.asInstanceOf[Container[Any, Nothing, Option[(Timestamp, String)]]]
          |}
          |""".stripMargin

      val owner = rewriteWithPrecompiledSource(dependency, input, extraOptions = Seq("typeNameStyle=owner"))
      val full = rewriteWithPrecompiledSource(dependency, input, extraOptions = Seq("typeNameStyle=full"))

      assert(owner.contains("*   - Option[(Timestamp, String)]"))
      assert(!owner.contains("$package"))
      assert(full.contains("domain.Timestamp"))
      assert(!full.contains("$package"))
    }

    test("uses Nothing only as the empty section fallback") {
      val input =
        """object Sample {
          |  final class Container[C, L, R]
          |  def value = null.asInstanceOf[Container[Any, Nothing, Unit]]
          |}
          |""".stripMargin

      val empty = rewrite(input)
      val augmented = rewrite(input, extraOptions = Seq("additionalType=L:Unexpected"))

      assert(empty.contains("*   - Nothing"))
      assert(augmented.contains("*   - Unexpected"))
      assert(!augmented.contains("*   - Nothing"))
    }

    test("applies additions and exclusions to their target parameter") {
      val input =
        """object Sample {
          |  final class Container[C, L, R]
          |  final class Internal[A]
          |  def value = null.asInstanceOf[Container[Any, Internal[Int], Unit]]
          |}
          |""".stripMargin
      val options = Seq(
        "additionalType=L:Fallback",
        "excludeTypeRegex=L:.*Internal",
        "additionalType=R:Companion"
      )

      val output = rewrite(input, extraOptions = options)

      assert(output.contains("*   - Fallback"))
      assert(!output.contains("*   - Internal[Int]"))
      assert(output.contains("*   - Companion"))
      assert(output.contains("*   - Unit"))
    }

    test("supports one or many freely selected parameters") {
      val input =
        """object Sample {
          |  final class Triple[First, Second, Third]
          |  def value = null.asInstanceOf[Triple[String, Int, Boolean]]
          |}
          |""".stripMargin
      val one = rewrite(input, typeParameters = Seq("typeParam=Second:Only:preserve"))
      val three = rewrite(input, typeParameters = Seq(
        "typeParam=Third:Third section:preserve",
        "typeParam=First:First section:preserve",
        "typeParam=Second:Second section:preserve"
      ))

      assert(one.contains("* Only:"))
      assert(one.contains("*   - Int"))
      assert(!one.contains("* First section:"))
      assert(three.indexOf("Third section:") < three.indexOf("First section:"))
      assert(three.indexOf("First section:") < three.indexOf("Second section:"))
      assert(three.contains("*   - Boolean"))
      assert(three.contains("*   - String"))
      assert(three.contains("*   - Int"))
    }

    test("optionally filters the outer constructor") {
      val input =
        """object Sample {
          |  final class Container[C, L, R]
          |  final class Alternative[L, R]
          |  def first = null.asInstanceOf[Container[Any, String, Int]]
          |  def second = null.asInstanceOf[Alternative[Boolean, Long]]
          |}
          |""".stripMargin

      val unrestricted = rewrite(input)
      val filtered = rewrite(input, extraOptions = Seq("typeRegex=.*Container"))

      assert(unrestricted.split("<!-- types -->", -1).length == 3)
      assert(filtered.split("<!-- types -->", -1).length == 2)
      assert(filtered.indexOf("<!-- types -->") < filtered.indexOf("def first"))
      assert(!filtered.substring(filtered.indexOf("def second") - 1).contains("<!-- types -->"))
    }

    test("requires every mapping and does not search nested types") {
      val input =
        """object Sample {
          |  final class Container[C, L, R]
          |  final class Partial[L]
          |  final class Wrapper[X]
          |  def direct = null.asInstanceOf[Container[Any, String, Int]]
          |  def missing = null.asInstanceOf[Partial[String]]
          |  def nested = null.asInstanceOf[Wrapper[Container[Any, String, Int]]]
          |}
          |""".stripMargin

      val output = rewrite(input)

      assert(output.split("<!-- types -->", -1).length == 2)
      assert(output.indexOf("<!-- types -->") < output.indexOf("def direct"))
    }

    test("renders a large balanced union of intersections") {
      val memberCount = 64
      val declarations = (0 until memberCount)
        .flatMap(index => Seq(s"  trait Left$index", s"  trait Right$index"))
        .mkString("\n")
      val members = (0 until memberCount).map(index => s"Left$index & Right$index")
      val selectedType = balanced("|", members)
      val input =
        s"""object Sample {
           |  final class Container[C, L, R]
           |$declarations
           |  def value = null.asInstanceOf[Container[Any, $selectedType, Int]]
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
