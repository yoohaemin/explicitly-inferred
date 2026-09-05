package explicitlyinferred

import utest.*

object SourceLayoutTests extends TestSuite {
  val tests = Tests {
    test("anchors above a multiline annotation and its comment") {
      val source =
        """object Sample {
          |  /* attached */
          |  @deprecated(
          |    "def",
          |    "1.0"
          |  )
          |  def value = 1
          |}
          |""".stripMargin

      val layout = SourceLayout.declaration(source, source.lastIndexOf("def value"))

      assert(source.substring(layout.insertionOffset).startsWith("  /* attached */"))
      assert(layout.indentation == "  ")
      assert(layout.hasAnnotations)
      assert(layout.newline == "\n")
    }

    test("leaves ordinary line comments outside the declaration anchor") {
      val source =
        """object Sample {
          |  // documentation
          |  def value = 1
          |}
          |""".stripMargin
      val declaration = source.indexOf("  def value")
      val layout = SourceLayout.declaration(source, declaration)

      assert(layout.insertionOffset == declaration)
      assert(!layout.hasAnnotations)
    }

    test("detects attached gaps and newline styles") {
      assert(SourceLayout.isAttachedGap("  \n", 0, 3))
      assert(!SourceLayout.isAttachedGap("\n\n", 0, 2))
      assert(!SourceLayout.isAttachedGap(" text \n", 0, 7))
      assert(SourceLayout.detectNewline("a\r\nb") == "\r\n")
    }

    test("stops after the local declaration context in a long source") {
      val unrelated = (0 until 10000).map(index => s"val value$index = $index").mkString("\n")
      val source = s"$unrelated\n\n@deprecated(\"old\", \"1.0\")\ndef target = 1\n"
      val layout = SourceLayout.declaration(source, source.indexOf("def target"))

      assert(source.substring(layout.insertionOffset).startsWith("@deprecated"))
      assert(layout.hasAnnotations)
    }
  }
}
