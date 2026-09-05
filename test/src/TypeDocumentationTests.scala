package explicitlyinferred

import utest.*

object TypeDocumentationTests extends TestSuite {
  import CompilerPluginTestSupport.*

  val tests = Tests {
    test("renders ordered mapped sections") {
      val input =
        """object Sample {
          |  final class Container[C, L, R]
          |  object NodeGroup {
          |    final class LeafTwo
          |    final class LeafOne
          |  }
          |
          |  def value = null.asInstanceOf[Container[Any, NodeGroup.LeafTwo | NodeGroup.LeafOne, Option[String]]]
          |}
          |""".stripMargin
      val expected =
        """object Sample {
          |  final class Container[C, L, R]
          |  object NodeGroup {
          |    final class LeafTwo
          |    final class LeafOne
          |  }
          |
          |  /**
          |   * <!-- types -->
          |   * Left:
          |   *   - Sample.NodeGroup.LeafOne
          |   *   - Sample.NodeGroup.LeafTwo
          |   *
          |   * Right:
          |   *   - Option[String]
          |   * <!-- /types -->
          |   */
          |  def value = null.asInstanceOf[Container[Any, NodeGroup.LeafTwo | NodeGroup.LeafOne, Option[String]]]
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
          |  final class LeafOne
          |  final class LeafTwo
          |  type ExpandedAlias = LeafTwo | LeafOne
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
      assert(output.contains("*   - LeafOne"))
      assert(output.contains("*   - LeafTwo"))
      assert(output.contains("* Preserved:"))
      assert(output.contains("*   - PreservedAlias"))
    }

    test("renders opaque aliases tuples and named tuples") {
      val input =
        """object NodeZero {
          |  opaque type LeafZero = String
          |}
          |
          |object Sample {
          |  final class Container[C, L, R]
          |  def first = null.asInstanceOf[Container[Any, Nothing, NodeZero.LeafZero]]
          |  def tuple = null.asInstanceOf[Container[Any, Nothing, (NodeZero.LeafZero, Option[String])]]
          |  def named = null.asInstanceOf[
          |    Container[Any, Nothing, NamedTuple.NamedTuple[Tuple1["fieldOne"], Tuple1[Option[String]]]]
          |  ]
          |}
          |""".stripMargin

      val output = rewrite(input, extraOptions = Seq("typeNameStyle=owner"))

      assert(output.contains("*   - NodeZero.LeafZero"))
      assert(output.contains("*   - (NodeZero.LeafZero, Option[String])"))
      assert(output.contains("*   - (fieldOne: Option[String])"))
    }

    test("preserves the concrete owner of inherited opaque Type aliases") {
      val dependency =
        """package fixture
          |
          |abstract class Wrapper[A] {
          |  opaque type Type <: A = A
          |}
          |
          |object NodeOne {
          |  object BranchOne {
          |    object LeafOne extends Wrapper[String]
          |  }
          |}
          |
          |object NodeTwo {
          |  object BranchTwo {
          |    object LeafTwo extends Wrapper[String]
          |  }
          |}
          |""".stripMargin
      val input =
        """import fixture.{NodeOne, NodeTwo}
          |
          |object Sample {
          |  final class Container[C, L, R]
          |  def value = null.asInstanceOf[
          |    Container[Any, Nothing, NodeOne.BranchOne.LeafOne.Type | NodeTwo.BranchTwo.LeafTwo.Type]
          |  ]
          |}
          |""".stripMargin

      val simple = rewriteWithPrecompiledSource(dependency, input, extraOptions = Seq("typeNameStyle=simple"))
      val owner = rewriteWithPrecompiledSource(dependency, input, extraOptions = Seq("typeNameStyle=owner"))
      val full = rewriteWithPrecompiledSource(dependency, input, extraOptions = Seq("typeNameStyle=full"))

      assert(simple.contains("*   - LeafOne"))
      assert(simple.contains("*   - LeafTwo"))
      assert(owner.contains("*   - NodeOne.BranchOne.LeafOne"))
      assert(owner.contains("*   - NodeTwo.BranchTwo.LeafTwo"))
      assert(full.contains("*   - fixture.NodeOne.BranchOne.LeafOne"))
      assert(full.contains("*   - fixture.NodeTwo.BranchTwo.LeafTwo"))
      assert(!simple.contains("*   - Wrapper.Type"))
      assert(!owner.contains("*   - Wrapper.Type"))
      assert(!full.contains("*   - fixture.Wrapper.Type"))
    }

    test("hides synthetic package owners for top-level aliases") {
      val dependency =
        """package fixture
          |
          |type AliasOne = AliasOne.Type
          |object AliasOne extends Wrapper[String]
          |
          |abstract class Wrapper[A] {
          |  opaque type Type <: A = A
          |}
          |""".stripMargin
      val input =
        """package sample
          |
          |import fixture.AliasOne
          |
          |object Sample {
          |  final class Container[C, L, R]
          |  def value = null.asInstanceOf[Container[Any, Nothing, Option[(AliasOne, String)]]]
          |}
          |""".stripMargin

      val owner = rewriteWithPrecompiledSource(dependency, input, extraOptions = Seq("typeNameStyle=owner"))
      val full = rewriteWithPrecompiledSource(dependency, input, extraOptions = Seq("typeNameStyle=full"))

      assert(owner.contains("*   - Option[(AliasOne, String)]"))
      assert(!owner.contains("$package"))
      assert(full.contains("fixture.AliasOne"))
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
      val augmented = rewrite(input, extraOptions = Seq("additionalType=L:AddedOne"))

      assert(empty.contains("*   - Nothing"))
      assert(augmented.contains("*   - AddedOne"))
      assert(!augmented.contains("*   - Nothing"))
    }

    test("applies additions and exclusions to their target parameter") {
      val input =
        """object Sample {
          |  final class Container[C, L, R]
          |  final class HiddenOne[A]
          |  def value = null.asInstanceOf[Container[Any, HiddenOne[Int], Unit]]
          |}
          |""".stripMargin
      val options = Seq(
        "additionalType=L:AddedOne",
        "excludeTypeRegex=L:.*HiddenOne",
        "additionalType=R:AddedTwo"
      )

      val output = rewrite(input, extraOptions = options)

      assert(output.contains("*   - AddedOne"))
      assert(!output.contains("*   - HiddenOne[Int]"))
      assert(output.contains("*   - AddedTwo"))
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
