package explicitlyinferred

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

object CompilerPluginTestSupport {
  private val PluginPrefix = "-P:explicitlyInferred:"
  private val DefaultEffectRegex = ".*Effect"
  private val DefaultSyntaxOptions = Seq("-no-indent", "-old-syntax")

  def rewrite(
      source: String,
      extraOptions: Seq[String] = Seq.empty,
      methodRegex: String = ".*"
  ): String =
    rewriteFiles(Seq("Sample.scala" -> source), extraOptions, methodRegex)("Sample.scala")

  def rewriteRaw(source: String, extraOptions: Seq[String] = Seq.empty): String =
    compile(Seq("Sample.scala" -> source), extraOptions = extraOptions).files("Sample.scala")

  def rewriteFiles(
      sources: Seq[(String, String)],
      extraOptions: Seq[String] = Seq.empty,
      methodRegex: String = ".*"
  ): Map[String, String] = {
    val result = compile(sources, extraOptions = extraOptions, methodRegex = methodRegex)
    if result.exitCode != 0 then throw new AssertionError(result.out + "\n" + result.err)
    result.files.view.mapValues(normalize).toMap
  }

  def compileWithoutRewrite(source: String, extraOptions: Seq[String] = Seq.empty): String = {
    val result = compile(Seq("Sample.scala" -> source), extraOptions = extraOptions, includeRewrite = false)
    if result.exitCode != 0 then throw new AssertionError(result.out + "\n" + result.err)
    result.files("Sample.scala")
  }

  def rewriteExpectFailure(
      source: String,
      extraOptions: Seq[String] = Seq.empty,
      includeDefaultEffectRegex: Boolean = true
  ): CompileResult = {
    val result = compile(
      Seq("Sample.scala" -> source),
      extraOptions = extraOptions,
      includeDefaultEffectRegex = includeDefaultEffectRegex
    )
    assert(result.exitCode != 0)
    assert(result.files("Sample.scala") == source)
    result
  }

  def compile(
      sources: Seq[(String, String)],
      extraOptions: Seq[String] = Seq.empty,
      methodRegex: String = ".*",
      includeRewrite: Boolean = true,
      includeDefaultEffectRegex: Boolean = true
  ): CompileResult = {
    val workspace = os.temp.dir(prefix = "explicitly-inferred-test")
    val outputDirectory = workspace / "out"
    os.makeDir.all(outputDirectory)
    sources.foreach { (name, content) =>
      os.write.over(workspace / name, content.getBytes(StandardCharsets.UTF_8), createFolders = true)
    }

    val pluginOptions =
      Seq(s"${PluginPrefix}methodRegex=$methodRegex") ++
        (if includeDefaultEffectRegex then Seq(s"${PluginPrefix}effectTypeRegex=$DefaultEffectRegex") else Seq.empty) ++
        extraOptions.map(PluginPrefix + _)
    val arguments =
      Seq("java", "-cp", sys.props("java.class.path"), "dotty.tools.dotc.Main") ++
        (if includeRewrite then Seq("-rewrite") else DefaultSyntaxOptions) ++
        Seq(
          "-classpath",
          sys.props("java.class.path"),
          "-d",
          outputDirectory.toString,
          s"-Xplugin:$pluginPath"
        ) ++
        pluginOptions ++
        sources.map { (name, _) => (workspace / name).toString }

    val process = os.proc(arguments).call(cwd = workspace, check = false)
    val files = sources.map { (name, _) =>
      name -> new String(os.read.bytes(workspace / name), StandardCharsets.UTF_8)
    }.toMap
    CompileResult(process.exitCode, process.out.text(), process.err.text(), files)
  }

  final case class CompileResult(exitCode: Int, out: String, err: String, files: Map[String, String])

  def expectIllegalArgument(body: => Any): IllegalArgumentException =
    try
      body
      throw new AssertionError("Expected IllegalArgumentException")
    catch
      case error: IllegalArgumentException => error

  private def pluginPath: String = {
    val classes = Paths.get(classOf[ExplicitlyInferredPlugin].getProtectionDomain.getCodeSource.getLocation.toURI).toString
    val resources = Paths.get(getClass.getClassLoader.getResource("plugin.properties").toURI).getParent.toString
    Seq(classes, resources).distinct.mkString(File.pathSeparator)
  }

  private def normalize(text: String): String = text.replace("\r\n", "\n")
}
