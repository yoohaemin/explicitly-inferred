package explicitlyinferred

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

private[explicitlyinferred] final class MethodMatcher private (steps: Array[MethodMatcher.Step]) {
  def matches(name: String): Boolean = {
    var currentName = name
    var index = 0
    while index < steps.length do
      val step = steps(index)
      val matcher = step.pattern.matcher(currentName)
      if !matcher.matches() then return false
      step.rewrite.foreach(replacement => currentName = MethodMatcher.rewrite(matcher, replacement))
      index += 1
    true
  }
}

private[explicitlyinferred] object MethodMatcher {
  private final case class Step(pattern: Pattern, groupCount: Int, rewrite: Option[String])

  final class Builder {
    private val steps = ArrayBuffer.empty[Step]

    def addRegex(value: String): Unit = {
      val pattern = OptionParsers.regex("methodRegex", value)
      steps += Step(pattern, pattern.matcher("").groupCount(), None)
    }

    def addRewrite(value: String): Unit =
      steps.lastOption match
        case Some(Step(_, groupCount, None)) if groupCount > 0 =>
          validateRewrite(value, groupCount)
          steps(steps.size - 1) = steps.last.copy(rewrite = Some(value))
        case _ =>
          invalidRewrite(value)

    def result(): MethodMatcher = {
      if steps.lastOption.exists(_.rewrite.nonEmpty) then invalidRewrite(steps.last.rewrite.get)
      if steps.isEmpty then addRegex(".*")
      new MethodMatcher(steps.toArray)
    }
  }

  private def validateRewrite(rewriteValue: String, groupCount: Int): Unit = {
    val validationPattern = Pattern.compile(List.fill(groupCount)("()").mkString)
    val matcher = validationPattern.matcher("")
    if !matcher.matches() then throw new IllegalStateException("Internal rewrite validation failed")
    rewrite(matcher, maskNamedReferences(rewriteValue))
  }

  private def maskNamedReferences(rewriteValue: String): String = {
    val builder = new StringBuilder
    var index = 0

    while index < rewriteValue.length do
      rewriteValue.charAt(index) match
        case '\\' =>
          if index + 1 >= rewriteValue.length then invalidRewrite(rewriteValue)
          builder.append('\\').append(rewriteValue.charAt(index + 1))
          index += 2
        case '$' if index + 1 < rewriteValue.length && rewriteValue.charAt(index + 1) == '{' =>
          val nameEnd = rewriteValue.indexOf('}', index + 2)
          if nameEnd <= index + 2 then invalidRewrite(rewriteValue)
          builder.append("namedGroup")
          index = nameEnd + 1
        case char =>
          builder.append(char)
          index += 1

    builder.toString
  }

  private def rewrite(matcher: java.util.regex.Matcher, replacement: String): String =
    try
      val builder = new StringBuffer
      matcher.appendReplacement(builder, replacement)
      matcher.appendTail(builder)
      builder.toString
    catch
      case cause: IllegalArgumentException => invalidRewrite(replacement, cause)
      case cause: IndexOutOfBoundsException => invalidRewrite(replacement, cause)
      case cause: IllegalStateException => invalidRewrite(replacement, cause)

  private def invalidRewrite(value: String, cause: Throwable = null): Nothing =
    throw new IllegalArgumentException(s"Invalid ${PluginMetadata.Name} methodRegexRewrite: $value", cause)
}
