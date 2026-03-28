import utest.*

object InferredReturnCommentNormalizationTests extends TestSuite {
  import CompilerPluginTestSupport.*

  val tests = Tests {
    test("expands alias chains transitively inside nested applied types") {
      val input =
        s"""object Sample {
           |  type Elem = Int
           |  type Items = List[Elem]
           |  type Result = Option[Items]
           |
           |  def value = null.asInstanceOf[Result]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  type Elem = Int
           |  type Items = List[Elem]
           |  type Result = Option[Items]
           |
           |  /*
           |   * @inferredReturnType Option[A = List[A = Int]]
           |   */
           |  def value = null.asInstanceOf[Result]
           |}
           |""".stripMargin

      val output = rewrite(input)
      assert(output == expected)
      assert(!managedCommentBody(output).contains("Elem"))
      assert(!managedCommentBody(output).contains("Items"))
      assert(!managedCommentBody(output).contains("Result"))
    }

    test("deduplicates unions created through alias expansion") {
      val input =
        s"""object Sample {
           |  final class Foo
           |  final class Bar
           |  type Left = Foo | Bar
           |  type Right = Bar | Foo
           |
           |  def value = null.asInstanceOf[Left | Right]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  final class Foo
           |  final class Bar
           |  type Left = Foo | Bar
           |  type Right = Bar | Foo
           |
           |  /*
           |   * @inferredReturnType Foo | Bar
           |   */
           |  def value = null.asInstanceOf[Left | Right]
           |}
           |""".stripMargin

      val output = rewrite(input)
      assert(output == expected)
      assert(!managedCommentBody(output).contains("Left"))
      assert(!managedCommentBody(output).contains("Right"))
    }

    test("reduces stable path dependent aliases to their underlying type") {
      val input =
        s"""object Sample {
           |  final class Box {
           |    type Out = List[Int]
           |  }
           |  val box: Box = null
           |
           |  def value = null.asInstanceOf[box.Out]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  final class Box {
           |    type Out = List[Int]
           |  }
           |  val box: Box = null
           |
           |  /*
           |   * @inferredReturnType List[A = Int]
           |   */
           |  def value = null.asInstanceOf[box.Out]
           |}
           |""".stripMargin

      val output = rewrite(input)
      assert(output == expected)
      assert(!managedCommentBody(output).contains("Out"))
      assert(!managedCommentBody(output).contains("box"))
    }

    test("reduces refined path dependent aliases when the member is concrete") {
      val input =
        s"""object Sample {
           |  trait Provider {
           |    type Out
           |  }
           |  val provider: Provider { type Out = List[Int] } = null
           |
           |  def value = null.asInstanceOf[provider.Out]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  trait Provider {
           |    type Out
           |  }
           |  val provider: Provider { type Out = List[Int] } = null
           |
           |  /*
           |   * @inferredReturnType List[A = Int]
           |   */
           |  def value = null.asInstanceOf[provider.Out]
           |}
           |""".stripMargin

      val output = rewrite(input)
      assert(output == expected)
      assert(!managedCommentBody(output).contains("provider"))
      assert(!managedCommentBody(output).contains("Out"))
    }

    test("reduces match types before rendering") {
      val input =
        s"""object Sample {
           |  type Elem[X] = X match
           |    case List[t] => t
           |    case _ => X
           |
           |  def value = null.asInstanceOf[Elem[List[Int]]]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  type Elem[X] = X match
           |    case List[t] => t
           |    case _ => X
           |
           |  /*
           |   * @inferredReturnType Int
           |   */
           |  def value = null.asInstanceOf[Elem[List[Int]]]
           |}
           |""".stripMargin

      val output = rewrite(input)
      assert(output == expected)
      assert(!managedCommentBody(output).contains("Elem"))
    }

    test("keeps opaque names outside their transparent scope but normalizes inside") {
      val input =
        s"""object Sample {
           |  object Api {
           |    opaque type Id = Int
           |    def make: Id = 1
           |    def inside = make
           |  }
           |
           |  def outside = Api.make
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  object Api {
           |    opaque type Id = Int
           |    def make: Id = 1
           |    /*
           |     * @inferredReturnType Int
           |     */
           |    def inside = make
           |  }
           |
           |  /*
           |   * @inferredReturnType Id
           |   */
           |  def outside = Api.make
           |}
           |""".stripMargin

      val output = rewrite(input)
      assert(output == expected)
      val body = managedCommentBody(output)
      assert(body.contains("@inferredReturnType Int"))
      assert(body.contains("@inferredReturnType Id"))
    }

    test("rewrites stale alias based managed comments to normalized output") {
      val input =
        s"""object Sample {
           |  type Elem = Int
           |  type Items = List[Elem]
           |
           |  /*
           |   * keep me
           |   * @inferredReturnType Items
           |   */
           |  def value = null.asInstanceOf[Items]
           |}
           |""".stripMargin

      val expected =
        s"""object Sample {
           |  type Elem = Int
           |  type Items = List[Elem]
           |
           |  /*
           |   * keep me
           |   * @inferredReturnType List[A = Int]
           |   */
           |  def value = null.asInstanceOf[Items]
           |}
           |""".stripMargin

      val output = rewrite(input)
      assert(output == expected)
      assert(!managedCommentBody(output).contains("Items"))
      assert(!managedCommentBody(output).contains("Elem"))
    }
  }
}
