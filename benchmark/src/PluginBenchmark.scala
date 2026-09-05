package explicitlyinferred.benchmark

import explicitlyinferred.ExplicitlyInferredPlugin

import java.io.File
import java.nio.file.Paths

object PluginBenchmark {
  private final case class Settings(warmups: Int = 2, iterations: Int = 7, scenario: Option[String] = None)
  private final case class Scenario(name: String, source: String, pluginOptions: List[String] = Nil)
  private final case class Result(name: String, baselineMillis: Double, pluginMillis: Double) {
    def overheadMillis: Double = pluginMillis - baselineMillis
    def ratio: Double = pluginMillis / baselineMillis
  }

  def main(args: Array[String]): Unit = {
    val settings = parseArguments(args.toList, Settings())
    val selected = scenarios.filter(scenario => settings.scenario.forall(_ == scenario.name))
    if selected.isEmpty then
      throw new IllegalArgumentException(s"Unknown benchmark scenario: ${settings.scenario.getOrElse("")}")

    println(s"JVM: ${sys.props("java.vm.name")} ${sys.props("java.version")}")
    println(s"Warmups: ${settings.warmups}, measured iterations: ${settings.iterations}")
    println("scenario\tbaseline_ms\tplugin_ms\toverhead_ms\tratio")
    selected.foreach { scenario =>
      val result = run(scenario, settings)
      println(f"${result.name}\t${result.baselineMillis}%.1f\t${result.pluginMillis}%.1f\t${result.overheadMillis}%.1f\t${result.ratio}%.3f")
    }
  }

  private def run(scenario: Scenario, settings: Settings): Result = {
    (0 until settings.warmups).foreach { _ =>
      compile(scenario, withPlugin = false)
      compile(scenario, withPlugin = true)
    }

    val baseline = collection.mutable.ArrayBuffer.empty[Double]
    val plugin = collection.mutable.ArrayBuffer.empty[Double]
    (0 until settings.iterations).foreach { iteration =>
      if iteration % 2 == 0 then {
        baseline += compile(scenario, withPlugin = false)
        plugin += compile(scenario, withPlugin = true)
      } else {
        plugin += compile(scenario, withPlugin = true)
        baseline += compile(scenario, withPlugin = false)
      }
    }

    Result(scenario.name, median(baseline), median(plugin))
  }

  private def compile(scenario: Scenario, withPlugin: Boolean): Double = {
    val workspace = os.temp.dir(prefix = s"explicitly-inferred-benchmark-${scenario.name}")
    val output = workspace / "out"
    val source = workspace / "Benchmark.scala"
    os.makeDir(output)
    os.write(source, scenario.source)

    val pluginArguments =
      if withPlugin then
        List(
          s"-Xplugin:$pluginPath",
          "-P:explicitlyInferred:typeParam=L:Left:dealias",
          "-P:explicitlyInferred:typeParam=R:Right:preserve"
        ) ++ scenario.pluginOptions.map("-P:explicitlyInferred:" + _)
      else Nil
    val command = Seq(
      "java",
      "-cp",
      sys.props("java.class.path"),
      "dotty.tools.dotc.Main",
      "-color:never",
      "-rewrite",
      "-classpath",
      sys.props("java.class.path"),
      "-d",
      output.toString
    ) ++ pluginArguments ++ Seq(source.toString)

    val started = System.nanoTime()
    val process = os.proc(command).call(cwd = workspace, check = false, stdout = os.Pipe, stderr = os.Pipe)
    val elapsed = (System.nanoTime() - started).toDouble / 1000000.0
    if process.exitCode != 0 then
      throw new IllegalStateException(
        s"${scenario.name} ${if withPlugin then "plugin" else "baseline"} compilation failed:\n${process.out.text()}\n${process.err.text()}"
      )
    elapsed
  }

  private def median(values: collection.Seq[Double]): Double = {
    val sorted = values.sorted
    val middle = sorted.size / 2
    if sorted.size % 2 == 0 then (sorted(middle - 1) + sorted(middle)) / 2.0 else sorted(middle)
  }

  private def parseArguments(arguments: List[String], settings: Settings): Settings = arguments match
    case Nil => settings
    case "--warmups" :: value :: tail => parseArguments(tail, settings.copy(warmups = positiveInt("warmups", value, allowZero = true)))
    case "--iterations" :: value :: tail => parseArguments(tail, settings.copy(iterations = positiveInt("iterations", value)))
    case "--scenario" :: value :: tail => parseArguments(tail, settings.copy(scenario = Some(value)))
    case option :: _ => throw new IllegalArgumentException(s"Unknown benchmark option: $option")

  private def positiveInt(name: String, value: String, allowZero: Boolean = false): Int =
    value.toIntOption.filter(number => number > 0 || allowZero && number == 0).getOrElse {
      throw new IllegalArgumentException(s"Invalid $name: $value")
    }

  private def pluginPath: String = {
    val classes = Paths.get(classOf[ExplicitlyInferredPlugin].getProtectionDomain.getCodeSource.getLocation.toURI).toString
    val resources = Paths.get(getClass.getClassLoader.getResource("plugin.properties").toURI).getParent.toString
    Seq(classes, resources).distinct.mkString(File.pathSeparator)
  }

  private val scenarios = List(
    Scenario("many-methods", manyMethods(1000, withComments = false)),
    Scenario("many-comments", manyMethods(1000, withComments = true)),
    Scenario("large-union", largeUnion(256, 100)),
    Scenario(
      "early-mismatch",
      manyMethods(5000, withComments = false),
      "methodRegex=never.*" :: List.fill(7)("methodRegex=.*")
    )
  )

  private def manyMethods(count: Int, withComments: Boolean): String = {
    val methods = (0 until count).map { index =>
      val comment = if withComments then s"  /** Existing documentation $index. */\n" else ""
      s"${comment}  def method$index = null.asInstanceOf[Container[Any, Nothing, Int]]"
    }.mkString("\n")
    s"""object Benchmark {
       |  final class Container[C, L, R]
       |$methods
       |}
       |""".stripMargin
  }

  private def largeUnion(memberCount: Int, methodCount: Int): String = {
    val types = (0 until memberCount).map(index => s"  final class Member$index").mkString("\n")
    val union = balancedUnion((0 until memberCount).map(index => s"Member$index"))
    val methods = (0 until methodCount)
      .map(index => s"  def method$index = null.asInstanceOf[Container[Any, Members, Int]]")
      .mkString("\n")
    s"""object Benchmark {
       |  final class Container[C, L, R]
       |$types
       |  type Members = $union
       |$methods
       |}
       |""".stripMargin
  }

  private def balancedUnion(types: IndexedSeq[String]): String =
    if types.size == 1 then types.head
    else
      val (left, right) = types.splitAt(types.size / 2)
      s"(${balancedUnion(left)} | ${balancedUnion(right)})"
}
