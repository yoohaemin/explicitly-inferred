package explicitlyinferred

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

private[explicitlyinferred] final case class PluginConfig(
    methodMatcher: MethodMatcher,
    scope: Scope,
    documentation: DocumentationConfig
)

private[explicitlyinferred] final case class DocumentationConfig(
    typeRegex: Option[Pattern],
    parameters: List[TypeParameterConfig],
    typeNameStyle: TypeNameStyle,
    markers: Markers
)

private[explicitlyinferred] final case class TypeParameterConfig(
    name: String,
    heading: String,
    aliasPolicy: AliasPolicy,
    additionalTypes: List[String],
    excludedTypes: List[Pattern]
)

private[explicitlyinferred] object PluginConfig {
  val OptionsHelp: String =
    """-P:explicitlyInferred:typeParam=<name>:<heading>:preserve|dealias (required, repeatable)
      |-P:explicitlyInferred:typeRegex=<java-regex>
      |-P:explicitlyInferred:additionalType=<type-parameter-name>:<display-name>
      |-P:explicitlyInferred:excludeTypeRegex=<type-parameter-name>:<java-regex>
      |-P:explicitlyInferred:methodRegex=<java-regex>
      |-P:explicitlyInferred:methodRegexRewrite=<java-replacement>
      |-P:explicitlyInferred:scope=members|all|nonPrivate
      |-P:explicitlyInferred:typeNameStyle=simple|owner|full
      |-P:explicitlyInferred:startMarker=<html-comment-text>
      |-P:explicitlyInferred:endMarker=<html-comment-text>
      |
      |Repeat typeParam to document multiple type arguments in option order.
      |Repeat methodRegex to build a left-to-right match pipeline.
      |methodRegexRewrite must immediately follow a capturing methodRegex and
      |rewrites the matched name before the next methodRegex stage.""".stripMargin

  def parse(options: List[String]): PluginConfig = {
    val methodMatcher = new MethodMatcher.Builder
    var scope = Scope.Members
    var typeRegex = Option.empty[Pattern]
    val parameters = ArrayBuffer.empty[TypeParameterConfig]
    val additionalTypes = ArrayBuffer.empty[(String, String)]
    val excludedTypes = ArrayBuffer.empty[(String, Pattern)]
    var typeNameStyle = TypeNameStyle.Simple
    var startMarker = Markers.DefaultStart
    var endMarker = Markers.DefaultEnd

    options.foreach {
      case option if option.startsWith("typeParam=") =>
        parameters += OptionParsers.typeParameterMapping(option.stripPrefix("typeParam="))
      case option if option.startsWith("typeRegex=") =>
        typeRegex = Some(OptionParsers.regex("typeRegex", option.stripPrefix("typeRegex=")))
      case option if option.startsWith("additionalType=") =>
        additionalTypes += OptionParsers.targetedValue("additionalType", option.stripPrefix("additionalType="))
      case option if option.startsWith("excludeTypeRegex=") =>
        val (name, value) = OptionParsers.targetedValue(
          "excludeTypeRegex",
          option.stripPrefix("excludeTypeRegex="),
          allowEmptyValue = true
        )
        excludedTypes += name -> OptionParsers.regex("excludeTypeRegex", value)
      case option if option.startsWith("methodRegex=") =>
        methodMatcher.addRegex(option.stripPrefix("methodRegex="))
      case option if option.startsWith("methodRegexRewrite=") =>
        methodMatcher.addRewrite(option.stripPrefix("methodRegexRewrite="))
      case option if option.startsWith("scope=") =>
        scope = Scope.parse(option.stripPrefix("scope="))
      case option if option.startsWith("typeNameStyle=") =>
        typeNameStyle = TypeNameStyle.parse(option.stripPrefix("typeNameStyle="))
      case option if option.startsWith("startMarker=") =>
        startMarker = OptionParsers.marker("startMarker", option.stripPrefix("startMarker="))
      case option if option.startsWith("endMarker=") =>
        endMarker = OptionParsers.marker("endMarker", option.stripPrefix("endMarker="))
      case option =>
        throw new IllegalArgumentException(s"Unknown ${PluginMetadata.Name} option: $option")
    }

    if parameters.isEmpty then
      throw new IllegalArgumentException(s"Missing required ${PluginMetadata.Name} option: typeParam")
    OptionParsers.requireUniqueMappings(parameters.toSeq)

    val parameterNames = parameters.iterator.map(_.name).toSet
    (additionalTypes.iterator.map(_._1) ++ excludedTypes.iterator.map(_._1))
      .find(name => !parameterNames.contains(name))
      .foreach { name =>
        throw new IllegalArgumentException(s"Unknown ${PluginMetadata.Name} type parameter target: $name")
      }

    if startMarker == endMarker then
      throw new IllegalArgumentException(s"${PluginMetadata.Name} markers must be different")

    val configuredParameters = parameters.map { parameter =>
      parameter.copy(
        additionalTypes = additionalTypes.collect { case (name, value) if name == parameter.name => value }.toList,
        excludedTypes = excludedTypes.collect { case (name, pattern) if name == parameter.name => pattern }.toList
      )
    }.toList

    PluginConfig(
      methodMatcher.result(),
      scope,
      DocumentationConfig(typeRegex, configuredParameters, typeNameStyle, Markers(startMarker, endMarker))
    )
  }
}

private[explicitlyinferred] object PluginMetadata {
  val Name = "explicitlyInferred"
  val Description = "Adds inferred type-parameter Scaladoc during -rewrite"
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

  def typeParameterMapping(value: String): TypeParameterConfig = {
    val firstColon = value.indexOf(':')
    val lastColon = value.lastIndexOf(':')
    if firstColon <= 0 || lastColon <= firstColon then invalid("typeParam", value)

    val name = typeParameter("typeParam", value.substring(0, firstColon))
    val heading = singleLine("typeParam", value.substring(firstColon + 1, lastColon))
    if heading.endsWith(":") then invalid("typeParam", value)
    val aliasPolicy = value.substring(lastColon + 1) match
      case "preserve" => AliasPolicy.Preserve
      case "dealias" => AliasPolicy.Dealias
      case _ => invalid("typeParam", value)
    TypeParameterConfig(name, heading, aliasPolicy, Nil, Nil)
  }

  def targetedValue(optionName: String, value: String, allowEmptyValue: Boolean = false): (String, String) = {
    val colon = value.indexOf(':')
    if colon <= 0 then invalid(optionName, value)
    val name = typeParameter(optionName, value.substring(0, colon))
    val rawTargetValue = value.substring(colon + 1)
    val targetValue =
      if allowEmptyValue && !rawTargetValue.contains('\n') && !rawTargetValue.contains('\r') then rawTargetValue
      else singleLine(optionName, rawTargetValue)
    name -> targetValue
  }

  def requireUniqueMappings(parameters: Seq[TypeParameterConfig]): Unit = {
    parameters.groupBy(_.name).collectFirst { case (name, values) if values.size > 1 => name }.foreach { name =>
      throw new IllegalArgumentException(s"Duplicate ${PluginMetadata.Name} type parameter: $name")
    }
    parameters.groupBy(_.heading).collectFirst { case (heading, values) if values.size > 1 => heading }.foreach { heading =>
      throw new IllegalArgumentException(s"Duplicate ${PluginMetadata.Name} heading: $heading")
    }
  }

  def typeParameter(optionName: String, value: String): String = {
    val name = value.trim
    if name.matches("[A-Za-z_$][A-Za-z0-9_$]*") then name
    else invalid(optionName, value)
  }

  def singleLine(optionName: String, value: String): String = {
    val result = value.trim
    if result.nonEmpty && !result.contains('\n') && !result.contains('\r') then result
    else invalid(optionName, value)
  }

  def marker(optionName: String, value: String): String = {
    val result = singleLine(optionName, value)
    if !result.contains("--") then result
    else invalid(optionName, value)
  }

  private def invalid(optionName: String, value: String): Nothing =
    throw new IllegalArgumentException(s"Invalid ${PluginMetadata.Name} $optionName: $value")
}
