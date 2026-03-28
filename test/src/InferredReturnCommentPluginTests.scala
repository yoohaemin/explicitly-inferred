import commenter.InferredReturnCommentPlugin
import utest.*

import java.io.File
import java.nio.file.Paths

object InferredReturnCommentPluginTests extends TestSuite {
  private val newline = "\n"

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
           |   * @inferredReturnType List[Int]
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
           |   * @inferredReturnType Option[Int]
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
  }

  private def rewrite(
      source: String,
      methodRegex: String = ".*",
      extraOptions: Seq[String] = Seq.empty
  ): String = {
    val workspace = os.temp.dir(prefix = "commenter-test")
    val sourceFile = workspace / "Sample.scala"
    val outDir = workspace / "out"
    os.write(sourceFile, source)
    os.makeDir.all(outDir)

    val args = Seq(
      "java",
      "-cp",
      sys.props("java.class.path"),
      "dotty.tools.dotc.Main",
      "-rewrite",
      "-classpath",
      sys.props("java.class.path"),
      "-d",
      outDir.toString,
      s"-Xplugin:${pluginPathString}",
      s"-P:inferredReturnComment:methodRegex=$methodRegex"
    ) ++ extraOptions.map(option => s"-P:inferredReturnComment:$option") ++ Seq(sourceFile.toString)

    val result = os.proc(args).call(cwd = workspace, check = false)
    if result.exitCode != 0 then
      throw new java.lang.AssertionError(result.out.text() + newline + result.err.text())
    normalize(os.read(sourceFile))
  }

  private def pluginPathString: String = {
    val classPath = Paths.get(classOf[InferredReturnCommentPlugin].getProtectionDomain.getCodeSource.getLocation.toURI).toString
    val resourcePath = Paths.get(getClass.getClassLoader.getResource("plugin.properties").toURI).getParent.toString
    Seq(classPath, resourcePath).distinct.mkString(File.pathSeparator)
  }

  private def normalize(text: String): String =
    text.replace("\r\n", newline)
}
