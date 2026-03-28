import commenter.InferredReturnCommentPlugin

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

object CompilerPluginTestSupport {
  val newline = "\n"
  val defaultManagedTag = "@inferredReturnType"

  def rewrite(
      source: String,
      methodRegex: String = ".*",
      extraOptions: Seq[String] = Seq.empty
  ): String =
    rewriteFiles(Seq("Sample.scala" -> source), methodRegex = methodRegex, extraOptions = extraOptions)("Sample.scala")

  def rewriteRaw(
      source: String,
      methodRegex: String = ".*",
      extraOptions: Seq[String] = Seq.empty
  ): String =
    rewriteFilesRaw(Seq("Sample.scala" -> source), methodRegex = methodRegex, extraOptions = extraOptions)("Sample.scala")

  def compileWithoutRewrite(
      source: String,
      methodRegex: String = ".*",
      extraOptions: Seq[String] = Seq.empty
  ): String =
    rewriteFiles(
      Seq("Sample.scala" -> source),
      methodRegex = methodRegex,
      extraOptions = extraOptions,
      includeRewrite = false
    )("Sample.scala")

  def rewriteFiles(
      sources: Seq[(String, String)],
      methodRegex: String = ".*",
      extraOptions: Seq[String] = Seq.empty,
      includeRewrite: Boolean = true
  ): Map[String, String] =
    rewriteFilesRaw(sources, methodRegex = methodRegex, extraOptions = extraOptions, includeRewrite = includeRewrite)
      .view
      .mapValues(normalize)
      .toMap

  def rewriteFilesRaw(
      sources: Seq[(String, String)],
      methodRegex: String = ".*",
      extraOptions: Seq[String] = Seq.empty,
      includeRewrite: Boolean = true
  ): Map[String, String] = {
    val result = runCompiler(sources, methodRegex = methodRegex, extraOptions = extraOptions, includeRewrite = includeRewrite)
    if result.exitCode != 0 then
      throw new java.lang.AssertionError(result.out + newline + result.err)
    result.files
  }

  def rewriteExpectFailure(
      source: String,
      methodRegex: String = ".*",
      extraOptions: Seq[String] = Seq.empty
  ): Unit = {
    val result = runCompiler(Seq("Sample.scala" -> source), methodRegex = methodRegex, extraOptions = extraOptions)
    assert(result.exitCode != 0)
    assert(result.out.contains("An unhandled exception was thrown in the compiler."))
    assert(result.files("Sample.scala") == source)
  }

  def managedCommentLines(text: String, managedTag: String = defaultManagedTag): Seq[String] =
    normalize(text)
      .linesIterator
      .map(_.trim)
      .filter(line => line.startsWith(s"* $managedTag") || line.startsWith("*   "))
      .toSeq
      .map(_.stripPrefix("* ").trim)

  def managedCommentBody(text: String, managedTag: String = defaultManagedTag): String =
    managedCommentLines(text, managedTag).mkString(newline)

  private def runCompiler(
      sources: Seq[(String, String)],
      methodRegex: String = ".*",
      extraOptions: Seq[String] = Seq.empty,
      includeRewrite: Boolean = true
  ): CompileResult = {
    val workspace = os.temp.dir(prefix = "commenter-test")
    val outDir = workspace / "out"
    os.makeDir.all(outDir)
    sources.foreach { (name, content) =>
      os.write.over(workspace / name, content.getBytes(StandardCharsets.UTF_8), createFolders = true)
    }

    val args = Seq(
      "java",
      "-cp",
      sys.props("java.class.path"),
      "dotty.tools.dotc.Main"
    ) ++
      (if includeRewrite then Seq("-rewrite") else Seq.empty) ++
      Seq(
        "-classpath",
        sys.props("java.class.path"),
        "-d",
        outDir.toString,
        s"-Xplugin:${pluginPathString}",
        s"-P:inferredReturnComment:methodRegex=$methodRegex"
      ) ++
      extraOptions.map(option => s"-P:inferredReturnComment:$option") ++
      sources.map { (name, _) => (workspace / name).toString }

    val result = os.proc(args).call(cwd = workspace, check = false)
    val files = sources.map { (name, _) =>
      name -> new String(os.read.bytes(workspace / name), StandardCharsets.UTF_8)
    }.toMap
    CompileResult(result.exitCode, result.out.text(), result.err.text(), files)
  }

  private final case class CompileResult(
      exitCode: Int,
      out: String,
      err: String,
      files: Map[String, String]
  )

  private def pluginPathString: String = {
    val classPath = Paths.get(classOf[InferredReturnCommentPlugin].getProtectionDomain.getCodeSource.getLocation.toURI).toString
    val resourcePath = Paths.get(getClass.getClassLoader.getResource("plugin.properties").toURI).getParent.toString
    Seq(classPath, resourcePath).distinct.mkString(File.pathSeparator)
  }

  private def normalize(text: String): String =
    text.replace("\r\n", newline)
}
