package explicitlyinferred

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

private[explicitlyinferred] final case class PluginConfig(
    methodMatcher: MethodMatcher,
    scope: Scope,
    effect: EffectConfig
)

private[explicitlyinferred] final case class EffectConfig(
    effectTypeRegex: Pattern,
    errorTypeParam: String,
    resultTypeParam: String,
    additionalErrorTypes: List[String],
    excludedErrorTypes: List[Pattern],
    typeNameStyle: TypeNameStyle,
    markers: Markers
)

private[explicitlyinferred] object PluginConfig {
  val OptionsHelp: String =
    """-P:explicitlyInferred:effectTypeRegex=<java-regex> (required)
      |-P:explicitlyInferred:methodRegex=<java-regex>
      |-P:explicitlyInferred:methodRegexRewrite=<java-replacement>
      |-P:explicitlyInferred:scope=members|all|nonPrivate
      |-P:explicitlyInferred:errorTypeParam=<type-parameter-name>
      |-P:explicitlyInferred:resultTypeParam=<type-parameter-name>
      |-P:explicitlyInferred:additionalErrorType=<display-name>
      |-P:explicitlyInferred:excludeErrorTypeRegex=<java-regex>
      |-P:explicitlyInferred:typeNameStyle=simple|owner|full
      |-P:explicitlyInferred:startMarker=<html-comment-text>
      |-P:explicitlyInferred:endMarker=<html-comment-text>
      |
      |Repeat methodRegex to build a left-to-right match pipeline.
      |methodRegexRewrite must immediately follow a capturing methodRegex and
      |rewrites the matched name before the next methodRegex stage.""".stripMargin

  def parse(options: List[String]): PluginConfig = {
    val methodMatcher = new MethodMatcher.Builder
    var scope = Scope.Members
    var effectTypeRegex = Option.empty[Pattern]
    var errorTypeParam = "E"
    var resultTypeParam = "A"
    val additionalErrorTypes = ArrayBuffer.empty[String]
    val excludedErrorTypes = ArrayBuffer.empty[Pattern]
    var typeNameStyle = TypeNameStyle.Simple
    var startMarker = Markers.DefaultStart
    var endMarker = Markers.DefaultEnd

    options.foreach {
      case option if option.startsWith("methodRegex=") =>
        methodMatcher.addRegex(option.stripPrefix("methodRegex="))
      case option if option.startsWith("methodRegexRewrite=") =>
        methodMatcher.addRewrite(option.stripPrefix("methodRegexRewrite="))
      case option if option.startsWith("scope=") =>
        scope = Scope.parse(option.stripPrefix("scope="))
      case option if option.startsWith("effectTypeRegex=") =>
        effectTypeRegex = Some(OptionParsers.regex("effectTypeRegex", option.stripPrefix("effectTypeRegex=")))
      case option if option.startsWith("errorTypeParam=") =>
        errorTypeParam = OptionParsers.typeParameter("errorTypeParam", option.stripPrefix("errorTypeParam="))
      case option if option.startsWith("resultTypeParam=") =>
        resultTypeParam = OptionParsers.typeParameter("resultTypeParam", option.stripPrefix("resultTypeParam="))
      case option if option.startsWith("additionalErrorType=") =>
        additionalErrorTypes += OptionParsers.singleLine("additionalErrorType", option.stripPrefix("additionalErrorType="))
      case option if option.startsWith("excludeErrorTypeRegex=") =>
        excludedErrorTypes += OptionParsers.regex("excludeErrorTypeRegex", option.stripPrefix("excludeErrorTypeRegex="))
      case option if option.startsWith("typeNameStyle=") =>
        typeNameStyle = TypeNameStyle.parse(option.stripPrefix("typeNameStyle="))
      case option if option.startsWith("startMarker=") =>
        startMarker = OptionParsers.marker("startMarker", option.stripPrefix("startMarker="))
      case option if option.startsWith("endMarker=") =>
        endMarker = OptionParsers.marker("endMarker", option.stripPrefix("endMarker="))
      case option =>
        throw new IllegalArgumentException(s"Unknown ${PluginMetadata.Name} option: $option")
    }

    if startMarker == endMarker then
      throw new IllegalArgumentException(s"${PluginMetadata.Name} markers must be different")

    PluginConfig(
      methodMatcher.result(),
      scope,
      EffectConfig(
        effectTypeRegex.getOrElse {
          throw new IllegalArgumentException(s"Missing required ${PluginMetadata.Name} option: effectTypeRegex")
        },
        errorTypeParam,
        resultTypeParam,
        additionalErrorTypes.toList,
        excludedErrorTypes.toList,
        typeNameStyle,
        Markers(startMarker, endMarker)
      )
    )
  }
}

private[explicitlyinferred] object PluginMetadata {
  val Name = "explicitlyInferred"
  val Description = "Adds inferred effect Scaladoc during -rewrite"
}

private[explicitlyinferred] final case class Markers(start: String, end: String) {
  def startDelimiter: String = s"<!-- $start -->"
  def endDelimiter: String = s"<!-- $end -->"
}

private[explicitlyinferred] object Markers {
  val DefaultStart = "types"
  val DefaultEnd = "/types"
}

private[explicitlyinferred] enum Scope {
  case Members, All, NonPrivate
}

private[explicitlyinferred] object Scope {
  def parse(value: String): Scope = value match
    case "members" => Scope.Members
    case "all" => Scope.All
    case "nonPrivate" => Scope.NonPrivate
    case _ => throw new IllegalArgumentException(s"Unknown ${PluginMetadata.Name} scope: $value")
}

private[explicitlyinferred] enum TypeNameStyle {
  case Simple, Owner, Full
}

private[explicitlyinferred] object TypeNameStyle {
  def parse(value: String): TypeNameStyle = value match
    case "simple" => TypeNameStyle.Simple
    case "owner" => TypeNameStyle.Owner
    case "full" => TypeNameStyle.Full
    case _ => throw new IllegalArgumentException(s"Unknown ${PluginMetadata.Name} typeNameStyle: $value")
}

private[explicitlyinferred] object OptionParsers {
  def regex(optionName: String, value: String): Pattern =
    Try(Pattern.compile(value)).getOrElse {
      throw new IllegalArgumentException(s"Invalid ${PluginMetadata.Name} $optionName: $value")
    }

  def typeParameter(optionName: String, value: String): String = {
    val name = value.trim
    if name.matches("[A-Za-z_$][A-Za-z0-9_$]*") then name
    else throw new IllegalArgumentException(s"Invalid ${PluginMetadata.Name} $optionName: $value")
  }

  def singleLine(optionName: String, value: String): String = {
    val result = value.trim
    if result.nonEmpty && !result.contains('\n') && !result.contains('\r') then result
    else throw new IllegalArgumentException(s"Invalid ${PluginMetadata.Name} $optionName: $value")
  }

  def marker(optionName: String, value: String): String = {
    val result = singleLine(optionName, value)
    if !result.contains("--") then result
    else throw new IllegalArgumentException(s"Invalid ${PluginMetadata.Name} $optionName: $value")
  }
}
