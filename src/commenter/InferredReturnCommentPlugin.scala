package commenter

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Comments.Comment
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.rewrites.Rewrites
import dotty.tools.dotc.transform.Pickler
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import java.util.regex.Pattern
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

final class InferredReturnCommentPlugin extends StandardPlugin {
  import InferredReturnCommentPlugin.*

  override val name: String = PluginName
  override val description: String = "Adds inferred return-type comments during -rewrite"
  override val optionsHelp: Option[String] = Some(
    """-P:inferredReturnComment:methodRegex=<java-regex>
      |-P:inferredReturnComment:methodRegexRewrite=<java-replacement>
      |-P:inferredReturnComment:scope=members|all|nonPrivate
      |-P:inferredReturnComment:maxTypeLength=<positive-int>
      |-P:inferredReturnComment:managedTag=<single-line-text>
      |-P:inferredReturnComment:showTypeArgs=true|false
      |-P:inferredReturnComment:showTypeParamNames=true|false
      |-P:inferredReturnComment:mode=returnComment|effectScaladoc
      |-P:inferredReturnComment:effectTypeRegex=<java-regex>
      |-P:inferredReturnComment:errorTypeParam=<type-parameter-name>
      |-P:inferredReturnComment:resultTypeParam=<type-parameter-name>
      |-P:inferredReturnComment:additionalErrorType=<display-name>
      |-P:inferredReturnComment:excludeErrorTypeRegex=<java-regex>
      |-P:inferredReturnComment:typeNameStyle=simple|owner|full
      |
      |Repeat methodRegex to build a left-to-right match pipeline.
      |methodRegexRewrite must immediately follow a capturing methodRegex and
      |rewrites the matched name before the next methodRegex stage.""".stripMargin
  )

  override def initialize(options: List[String])(using Context): List[PluginPhase] =
    parseConfig(options) match
      case Some(config) => List(new InferredReturnCommentPhase(config))
      case None => Nil

  private def parseConfig(options: List[String])(using Context): Option[Config] =
    val methodSteps = ArrayBuffer.empty[MethodRegexStep]
    var scope = Scope.Members
    var maxTypeLength = 80
    var managedTag = DefaultManagedTag
    var showTypeArgs = true
    var showTypeParamNames = true
    var mode = Mode.ReturnComment
    var effectTypeRegex = Pattern.compile(".*")
    var errorTypeParam = "E"
    var resultTypeParam = "A"
    val additionalErrorTypes = ArrayBuffer.empty[String]
    val excludeErrorTypeRegexes = ArrayBuffer.empty[Pattern]
    var typeNameStyle = TypeNameStyle.Simple

    def invalidMethodRegex(value: String): Nothing =
      throw new IllegalArgumentException(s"Invalid $PluginName methodRegex: $value")

    def invalidMethodRegexRewrite(value: String): Nothing =
      throw new IllegalArgumentException(s"Invalid $PluginName methodRegexRewrite: $value")

    def compileMethodRegex(value: String): MethodRegexStep = {
      val pattern = Try(Pattern.compile(value)).getOrElse(invalidMethodRegex(value))
      MethodRegexStep(pattern, pattern.matcher("").groupCount(), None)
    }

    options.foreach {
      case option if option.startsWith("methodRegex=") =>
        val value = option.stripPrefix("methodRegex=")
        methodSteps += compileMethodRegex(value)
      case option if option.startsWith("methodRegexRewrite=") =>
        val value = option.stripPrefix("methodRegexRewrite=")
        methodSteps.lastOption match
          case None =>
            invalidMethodRegexRewrite(value)
          case Some(MethodRegexStep(_, _, Some(_))) =>
            invalidMethodRegexRewrite(value)
          case Some(MethodRegexStep(_, 0, None)) =>
            invalidMethodRegexRewrite(value)
          case Some(step) =>
            validateMethodRegexRewrite(value, step)
            methodSteps(methodSteps.size - 1) = step.copy(rewrite = Some(value))
      case option if option.startsWith("scope=") =>
        Scope.fromOption(option.stripPrefix("scope=")) match
          case Some(value) => scope = value
          case None =>
            throw new IllegalArgumentException(s"Unknown $PluginName scope: ${option.stripPrefix("scope=")}")
      case option if option.startsWith("maxTypeLength=") =>
        IntOption.fromOption(option.stripPrefix("maxTypeLength=")) match
          case Some(value) => maxTypeLength = value
          case None =>
            throw new IllegalArgumentException(s"Invalid $PluginName maxTypeLength: ${option.stripPrefix("maxTypeLength=")}")
      case option if option.startsWith("managedTag=") =>
        ManagedTagOption.fromOption(option.stripPrefix("managedTag=")) match
          case Some(value) => managedTag = value
          case None =>
            throw new IllegalArgumentException(s"Invalid $PluginName managedTag: ${option.stripPrefix("managedTag=")}")
      case option if option.startsWith("showTypeArgs=") =>
        BooleanOption.fromOption(option.stripPrefix("showTypeArgs=")) match
          case Some(value) => showTypeArgs = value
          case None =>
            throw new IllegalArgumentException(s"Invalid $PluginName showTypeArgs: ${option.stripPrefix("showTypeArgs=")}")
      case option if option.startsWith("showTypeParamNames=") =>
        BooleanOption.fromOption(option.stripPrefix("showTypeParamNames=")) match
          case Some(value) => showTypeParamNames = value
          case None =>
            throw new IllegalArgumentException(s"Invalid $PluginName showTypeParamNames: ${option.stripPrefix("showTypeParamNames=")}")
      case option if option.startsWith("mode=") =>
        Mode.fromOption(option.stripPrefix("mode=")) match
          case Some(value) => mode = value
          case None =>
            throw new IllegalArgumentException(s"Unknown $PluginName mode: ${option.stripPrefix("mode=")}")
      case option if option.startsWith("effectTypeRegex=") =>
        val value = option.stripPrefix("effectTypeRegex=")
        effectTypeRegex = Try(Pattern.compile(value)).getOrElse {
          throw new IllegalArgumentException(s"Invalid $PluginName effectTypeRegex: $value")
        }
      case option if option.startsWith("errorTypeParam=") =>
        errorTypeParam = TypeParamOption.parse("errorTypeParam", option.stripPrefix("errorTypeParam="))
      case option if option.startsWith("resultTypeParam=") =>
        resultTypeParam = TypeParamOption.parse("resultTypeParam", option.stripPrefix("resultTypeParam="))
      case option if option.startsWith("additionalErrorType=") =>
        additionalErrorTypes += DisplayTypeOption.parse("additionalErrorType", option.stripPrefix("additionalErrorType="))
      case option if option.startsWith("excludeErrorTypeRegex=") =>
        val value = option.stripPrefix("excludeErrorTypeRegex=")
        excludeErrorTypeRegexes += Try(Pattern.compile(value)).getOrElse {
          throw new IllegalArgumentException(s"Invalid $PluginName excludeErrorTypeRegex: $value")
        }
      case option if option.startsWith("typeNameStyle=") =>
        TypeNameStyle.fromOption(option.stripPrefix("typeNameStyle=")) match
          case Some(value) => typeNameStyle = value
          case None =>
            throw new IllegalArgumentException(s"Unknown $PluginName typeNameStyle: ${option.stripPrefix("typeNameStyle=")}")
      case option =>
        throw new IllegalArgumentException(s"Unknown $PluginName option: $option")
    }

    if methodSteps.lastOption.exists(_.rewrite.nonEmpty) then
      invalidMethodRegexRewrite(methodSteps.last.rewrite.get)

    val compiledMethodSteps =
      if methodSteps.nonEmpty then methodSteps.toList
      else List(compileMethodRegex(".*"))

    val effectSettings = EffectSettings(
      effectTypeRegex,
      errorTypeParam,
      resultTypeParam,
      additionalErrorTypes.toList,
      excludeErrorTypeRegexes.toList,
      typeNameStyle
    )
    Some(Config(
      compiledMethodSteps,
      scope,
      mode,
      RenderSettings(maxTypeLength, managedTag, showTypeArgs, showTypeParamNames, typeNameStyle, true),
      effectSettings
    ))
}

object InferredReturnCommentPlugin {
  private val PluginName = "inferredReturnComment"
  private val DefaultManagedTag = "@inferredReturnType"
  private val ManagedContinuationPrefix = "  "
  private val EffectManagedStart = "<!-- types -->"
  private val EffectManagedEnd = "<!-- /types -->"

  private final case class Config(
      methodSteps: List[MethodRegexStep],
      scope: Scope,
      mode: Mode,
      renderSettings: RenderSettings,
      effectSettings: EffectSettings
  )
  private final case class MethodRegexStep(
      pattern: Pattern,
      groupCount: Int,
      rewrite: Option[String]
  )
  private final case class RenderSettings(
      maxTypeLength: Int,
      managedTag: String,
      showTypeArgs: Boolean,
      showTypeParamNames: Boolean,
      typeNameStyle: TypeNameStyle,
      dealiasTypes: Boolean
  ) {
    def managedPrefix: String = s"$managedTag "
  }

  private final case class EffectSettings(
      effectTypeRegex: Pattern,
      errorTypeParam: String,
      resultTypeParam: String,
      additionalErrorTypes: List[String],
      excludeErrorTypeRegexes: List[Pattern],
      typeNameStyle: TypeNameStyle
  )

  private enum Mode {
    case ReturnComment, EffectScaladoc
  }

  private object Mode {
    def fromOption(value: String): Option[Mode] = value match
      case "returnComment" => Some(Mode.ReturnComment)
      case "effectScaladoc" => Some(Mode.EffectScaladoc)
      case _ => None
  }

  private enum TypeNameStyle {
    case Simple, Owner, Full
  }

  private object TypeNameStyle {
    def fromOption(value: String): Option[TypeNameStyle] = value match
      case "simple" => Some(TypeNameStyle.Simple)
      case "owner" => Some(TypeNameStyle.Owner)
      case "full" => Some(TypeNameStyle.Full)
      case _ => None
  }

  private enum Scope {
    case Members, All, NonPrivate
  }

  private object Scope {
    def fromOption(value: String): Option[Scope] = value match
      case "members" => Some(Scope.Members)
      case "all" => Some(Scope.All)
      case "nonPrivate" => Some(Scope.NonPrivate)
      case _ => None
  }

  private object IntOption {
    def fromOption(value: String): Option[Int] =
      Try(value.toInt).toOption.filter(_ > 0)
  }

  private object ManagedTagOption {
    def fromOption(value: String): Option[String] = {
      val trimmed = value.trim
      Option.when(trimmed.nonEmpty && !trimmed.contains('\n') && !trimmed.contains('\r'))(trimmed)
    }
  }

  private object BooleanOption {
    def fromOption(value: String): Option[Boolean] = value match
      case "true" => Some(true)
      case "false" => Some(false)
      case _ => None
  }

  private object TypeParamOption {
    def parse(optionName: String, value: String): String = {
      val trimmed = value.trim
      if trimmed.matches("[A-Za-z_$][A-Za-z0-9_$]*") then trimmed
      else throw new IllegalArgumentException(s"Invalid $PluginName $optionName: $value")
    }
  }

  private object DisplayTypeOption {
    def parse(optionName: String, value: String): String = {
      val trimmed = value.trim
      if trimmed.nonEmpty && !trimmed.contains('\n') && !trimmed.contains('\r') then trimmed
      else throw new IllegalArgumentException(s"Invalid $PluginName $optionName: $value")
    }
  }

  private def validateMethodRegexRewrite(rewrite: String, step: MethodRegexStep): Unit =
    applyMethodRegexRewrite(validationMatcher(step.groupCount), sanitizeNamedGroupReferences(rewrite))

  private def validationMatcher(groupCount: Int): java.util.regex.Matcher = {
    val builder = new StringBuilder
    (1 to groupCount).foreach { _ =>
      builder.append("()")
    }
    val matcher = Pattern.compile(builder.result()).matcher("")
    if !matcher.matches() then
      throw new IllegalStateException("Internal validation regex did not match")
    matcher
  }

  private def sanitizeNamedGroupReferences(rewrite: String): String = {
    val builder = new StringBuilder
    var index = 0

    while index < rewrite.length do
      rewrite.charAt(index) match
        case '\\' =>
          if index + 1 >= rewrite.length then
            throw new IllegalArgumentException(s"Invalid $PluginName methodRegexRewrite: $rewrite")
          builder.append('\\')
          builder.append(rewrite.charAt(index + 1))
          index += 2
        case '$' if index + 1 < rewrite.length && rewrite.charAt(index + 1) == '{' =>
          val nameStart = index + 2
          val nameEnd = rewrite.indexOf('}', nameStart)
          if nameEnd <= nameStart then
            throw new IllegalArgumentException(s"Invalid $PluginName methodRegexRewrite: $rewrite")
          builder.append("namedGroup")
          index = nameEnd + 1
        case ch =>
          builder.append(ch)
          index += 1

    builder.toString
  }

  private def applyMethodRegexRewrite(matcher: java.util.regex.Matcher, rewrite: String): String =
    try
      val builder = new StringBuffer
      matcher.appendReplacement(builder, rewrite)
      matcher.appendTail(builder)
      builder.toString
    catch
      case cause: IllegalArgumentException =>
        throw new IllegalArgumentException(s"Invalid $PluginName methodRegexRewrite: $rewrite", cause)
      case cause: IndexOutOfBoundsException =>
        throw new IllegalArgumentException(s"Invalid $PluginName methodRegexRewrite: $rewrite", cause)
      case cause: IllegalStateException =>
        throw new IllegalArgumentException(s"Invalid $PluginName methodRegexRewrite: $rewrite", cause)

  private object NormalizedTypeRenderer {
    final case class RenderedType(display: String, sortKey: String, fullName: String)

    private sealed trait TypeNode {
      def sortKey: String
      def precedence: Int
    }

    private final case class Leaf(display: String, sortKey: String) extends TypeNode {
      override val precedence: Int = 4
    }

    private final case class Applied(tycon: TypeNode, args: List[RenderedArg], sortKey: String) extends TypeNode {
      override val precedence: Int = 4
    }

    private final case class Union(parts: List[TypeNode], sortKey: String) extends TypeNode {
      override val precedence: Int = 1
    }

    private final case class Intersection(parts: List[TypeNode], sortKey: String) extends TypeNode {
      override val precedence: Int = 2
    }

    private final case class TupleValue(parts: List[TypeNode], sortKey: String) extends TypeNode {
      override val precedence: Int = 4
    }

    private final case class NamedTupleValue(fields: List[(String, TypeNode)], sortKey: String) extends TypeNode {
      override val precedence: Int = 4
    }

    private final case class RenderedArg(label: Option[String], value: TypeNode, sortKey: String)

    def managedLines(tpe: Type, settings: RenderSettings)(using Context): Seq[String] = {
      val node = toNode(tpe, settings)
      val singleLine = renderSingle(node)
      if settings.managedPrefix.length + singleLine.length <= settings.maxTypeLength then
        Seq(settings.managedPrefix + singleLine)
      else
        settings.managedTag +: renderBlock(node).map(line => ManagedContinuationPrefix + line)
    }

    def unionEntries(tpe: Type, settings: RenderSettings)(using Context): List[RenderedType] =
      flattenOr(tpe, settings.dealiasTypes)
        .map(part => (toNode(part, settings), fullTypeName(part)))
        .sortBy(_._1.sortKey)
        .foldLeft(List.empty[(TypeNode, String)]) { (acc, entry) =>
          if acc.lastOption.exists(_._1.sortKey == entry._1.sortKey) then acc else acc :+ entry
        }
        .map { (node, fullName) => RenderedType(renderSingle(node), node.sortKey, fullName) }

    def fullTypeName(tpe: Type)(using Context): String = {
      val normalized = normalizeType(tpe)
      val symbol = normalized.typeSymbol
      if symbol.exists then symbol.fullName.show else normalized.show
    }

    private def toNode(tpe: Type, settings: RenderSettings)(using Context): TypeNode = {
      val normalized = normalizeType(tpe, settings.dealiasTypes)
      if !settings.dealiasTypes && isCompilerInternalAlias(normalized) then
        toNode(normalizeType(tpe, true), settings)
      else normalized match
        case tp: OrType =>
          val parts = flattenOr(tp, settings.dealiasTypes).map(part => toNode(part, settings)).sortBy(_.sortKey)
          val deduped = dedupe(parts)
          if deduped.size == 1 then deduped.head
          else Union(deduped, deduped.map(_.sortKey).mkString("or(", ",", ")"))
        case tp: AndType =>
          val parts = flattenAnd(tp, settings.dealiasTypes).map(part => toNode(part, settings)).sortBy(_.sortKey)
          val deduped = dedupe(parts)
          if deduped.size == 1 then deduped.head
          else Intersection(deduped, deduped.map(_.sortKey).mkString("and(", ",", ")"))
        case AppliedType(tycon, args) if isNamedTuple(tycon, args) =>
          namedTupleNode(args, settings).getOrElse(leafFor(normalized, settings))
        case AppliedType(tycon, args) if isOrdinaryTuple(tycon, args) =>
          val parts = args.map(arg => toNode(arg, settings))
          TupleValue(parts, parts.map(_.sortKey).mkString("tuple(", ",", ")"))
        case AppliedType(tycon, args) =>
          val tyconNode = toNode(tycon, settings)
          if !settings.showTypeArgs || args.isEmpty then tyconNode
          else
            val labels = typeArgumentLabels(tycon, args.length, settings.showTypeParamNames)
            val renderedArgs = args.zip(labels).map { (arg, label) =>
              val node = toNode(arg, settings)
              val key = label.fold(node.sortKey)(name => s"$name=${node.sortKey}")
              RenderedArg(label, node, key)
            }
            Applied(tyconNode, renderedArgs, s"${tyconNode.sortKey}[${renderedArgs.map(_.sortKey).mkString(",")}]")
        case _ =>
          leafFor(normalized, settings)
    }

    private def leafFor(tpe: Type, settings: RenderSettings)(using Context): TypeNode = {
      val display = displayName(tpe, settings.typeNameStyle, settings.dealiasTypes)
      Leaf(display, stableLeafKey(tpe, display, settings.dealiasTypes))
    }

    private def normalizeType(tpe: Type, dealiasTypes: Boolean = true)(using Context): Type =
      if dealiasTypes then tpe.widenDealias.simplified.normalized.dealias
      else tpe.widen.simplified.normalized

    private def flattenOr(tpe: Type, dealiasTypes: Boolean = true)(using Context): List[Type] =
      normalizeType(tpe, dealiasTypes) match
        case OrType(left, right) => flattenOr(left, dealiasTypes) ::: flattenOr(right, dealiasTypes)
        case other => other :: Nil

    private def flattenAnd(tpe: Type, dealiasTypes: Boolean = true)(using Context): List[Type] =
      normalizeType(tpe, dealiasTypes) match
        case AndType(left, right) => flattenAnd(left, dealiasTypes) ::: flattenAnd(right, dealiasTypes)
        case other => other :: Nil

    private def isNamedTuple(tycon: Type, args: List[Type])(using Context): Boolean =
      args.size == 2 &&
        tycon.typeSymbol.name.show == "NamedTuple" &&
        tycon.typeSymbol.owner.name.show == "NamedTuple"

    private def isOrdinaryTuple(tycon: Type, args: List[Type])(using Context): Boolean = {
      val name = fullTypeName(tycon)
      args.size >= 2 && name.matches("scala\\.Tuple[0-9]+")
    }

    private def namedTupleNode(args: List[Type], settings: RenderSettings)(using Context): Option[TypeNode] =
      for
        names <- tupleElements(args.head, settings.dealiasTypes)
        values <- tupleElements(args(1), settings.dealiasTypes)
        if names.size == values.size && names.nonEmpty
      yield {
        val fields = names.zip(values).map { (nameType, valueType) =>
          val rawName = nameType.dealias.show
          val firstQuote = rawName.indexOf('"')
          val secondQuote = if firstQuote >= 0 then rawName.indexOf('"', firstQuote + 1) else -1
          val name =
            if firstQuote >= 0 && secondQuote > firstQuote then rawName.substring(firstQuote + 1, secondQuote)
            else rawName
          name -> toNode(valueType, settings)
        }
        NamedTupleValue(
          fields,
          fields.map { (name, value) => s"$name:${value.sortKey}" }.mkString("namedTuple(", ",", ")")
        )
      }

    private def isCompilerInternalAlias(tpe: Type)(using Context): Boolean = {
      val symbol = tpe.typeSymbol
      symbol.exists && symbol.owner.name.show == "Signature"
    }

    private def tupleElements(tpe: Type, dealiasTypes: Boolean)(using Context): Option[List[Type]] =
      normalizeType(tpe, dealiasTypes) match
        case AppliedType(tycon, args) if fullTypeName(tycon).matches("scala\\.Tuple[0-9]+") =>
          Some(args)
        case _ => None

    private def dedupe(nodes: List[TypeNode]): List[TypeNode] =
      nodes.foldLeft(List.empty[TypeNode]) { (acc, node) =>
        if acc.lastOption.exists(_.sortKey == node.sortKey) then acc else acc :+ node
      }

    private def typeArgumentLabels(tycon: Type, argCount: Int, showNames: Boolean)(using Context): List[Option[String]] =
      if !showNames then List.fill(argCount)(None)
      else
        val labels = tycon.typeParams.map(_.paramName.show).take(argCount).map(Some(_))
        labels.padTo(argCount, None)

    private def displayName(tpe: Type, typeNameStyle: TypeNameStyle, dealiasTypes: Boolean)(using Context): String =
      normalizeType(tpe, dealiasTypes) match
        case tp: TypeRef if tp.symbol.exists =>
          displaySymbol(tp.symbol, typeNameStyle)
        case tp: TermRef if tp.symbol.exists =>
          displaySymbol(tp.symbol, typeNameStyle)
        case tp: ThisType =>
          displaySymbol(tp.tref.symbol, typeNameStyle)
        case tp: ConstantType =>
          tp.show
        case tp: TypeBounds =>
          tp.show
        case tp =>
          tp.show

    private def displaySymbol(symbol: Symbol, typeNameStyle: TypeNameStyle)(using Context): String =
      typeNameStyle match
        case TypeNameStyle.Simple => symbol.name.show
        case TypeNameStyle.Full => symbol.fullName.show
        case TypeNameStyle.Owner =>
          val owner = symbol.owner
          if symbol.fullName.show.startsWith("scala.") then
            symbol.name.show
          else if owner.exists && !owner.is(Flags.Package) then
            s"${owner.name.show}.${symbol.name.show}"
          else
            symbol.name.show

    private def stableLeafKey(tpe: Type, display: String, dealiasTypes: Boolean)(using Context): String = {
      val normalized = normalizeType(tpe, dealiasTypes)
      val symbol = normalized.typeSymbol
      if symbol.exists then
        val coord = symbol.coord.toString
        s"$coord:${symbol.owner.fullName.show}.${symbol.name.show}"
      else
        s"$display#${normalized.show}"
    }

    private def renderSingle(node: TypeNode, parentPrecedence: Int = 0): String = {
      val rendered = node match
        case Leaf(display, _) => display
        case Applied(tycon, args, _) =>
          s"${renderSingle(tycon, node.precedence)}[${args.map(renderArg).mkString(", ")}]"
        case Union(parts, _) =>
          parts.map(renderSingle(_, node.precedence)).mkString(" | ")
        case Intersection(parts, _) =>
          parts.map(renderSingle(_, node.precedence)).mkString(" & ")
        case TupleValue(parts, _) =>
          parts.map(renderSingle(_)).mkString("(", ", ", ")")
        case NamedTupleValue(fields, _) =>
          fields.map { (name, value) => s"$name: ${renderSingle(value)}" }.mkString("(", ", ", ")")
      if node.precedence < parentPrecedence then s"($rendered)" else rendered
    }

    private def renderArg(arg: RenderedArg): String =
      arg.label match
        case Some(label) => s"$label = ${renderSingle(arg.value)}"
        case None => renderSingle(arg.value)

    private def renderBlock(node: TypeNode, parentPrecedence: Int = 0): List[String] = {
      val core = node match
        case leaf: Leaf =>
          leaf.display :: Nil
        case Applied(tycon, args, _) =>
          val rendered = renderSingle(node, parentPrecedence)
          if rendered.length <= 60 then rendered :: Nil
          else renderAppliedBlock(tycon, args)
        case Union(parts, _) =>
          renderJoinedBlock(parts, " |", node.precedence)
        case Intersection(parts, _) =>
          renderJoinedBlock(parts, " &", node.precedence)
        case tuple: TupleValue =>
          renderSingle(tuple) :: Nil
        case tuple: NamedTupleValue =>
          renderSingle(tuple) :: Nil
      if node.precedence < parentPrecedence then wrapWithParens(core) else core
    }

    private def renderAppliedBlock(tycon: TypeNode, args: List[RenderedArg]): List[String] = {
      val head = renderSingle(tycon) + "["
      val argBlocks = args.map(renderArgBlock)
      val lines = ArrayBuffer(head)
      argBlocks.zipWithIndex.foreach { (argBlock, index) =>
        val prefixed = prefixLines(argBlock, "  ")
        if prefixed.nonEmpty then
          lines ++= prefixed.init
          val suffix = if index == args.size - 1 then "" else ","
          lines += prefixed.last + suffix
      }
      lines += "]"
      lines.toList
    }

    private def renderArgBlock(arg: RenderedArg): List[String] =
      arg.label match
        case Some(label) =>
          val valueLines = renderBlock(arg.value)
          valueLines match
            case head :: tail =>
              s"$label = $head" :: prefixLines(tail, "  ")
            case Nil =>
              s"$label = ${renderSingle(arg.value)}" :: Nil
        case None =>
          renderBlock(arg.value)

    private def renderJoinedBlock(parts: List[TypeNode], operatorSuffix: String, parentPrecedence: Int): List[String] = {
      val blocks = parts.map(renderBlock(_, parentPrecedence))
      val lines = ArrayBuffer.from(blocks.headOption.getOrElse(Nil))
      blocks.drop(1).foreach { block =>
        if lines.nonEmpty then lines(lines.size - 1) = lines.last + operatorSuffix
        lines ++= block
      }
      lines.toList
    }

    private def prefixLines(lines: List[String], prefix: String): List[String] =
      lines.map(prefix + _)

    private def wrapWithParens(lines: List[String]): List[String] = lines match
      case Nil => "()" :: Nil
      case head :: Nil => s"($head)" :: Nil
      case many =>
        "(" :: prefixLines(many, "  ") ::: ")" :: Nil
  }

  private final class InferredReturnCommentPhase(config: Config) extends PluginPhase {
    import tpd.*

    private val orderedCommentsByUnit = mutable.HashMap.empty[CompilationUnit, IndexedSeq[Comment]]

    override val phaseName: String = "inferredReturnCommentPhase"
    override val runsAfter: Set[String] = Set(TyperPhase.name)
    override val runsBefore: Set[String] = Set(Pickler.name)

    override def transformDefDef(tree: DefDef)(using Context): Tree =
      maybePatch(tree)
      tree

    private def maybePatch(tree: DefDef)(using ctx: Context): Unit =
      if !ctx.compilationUnit.source.exists then
        ()
      else if !tree.tpt.isInstanceOf[InferredTypeTree] then
        ()
      else if isSkipped(tree.symbol, tree.name.toString) then
        ()
      else
        val source = ctx.compilationUnit.source
        val text = sourceText(source)
        val defLineStart = lineStart(text, tree.span.start)
        val insertionLineStart = declarationAnchorLineStart(text, defLineStart)
        val indent = text.substring(insertionLineStart, indentationEnd(text, insertionLineStart))
        val newline = detectNewline(text)
        val commentOpt =
          if !declarationHasAnnotations(text, defLineStart) then
            nearestAttachedComment(ctx.compilationUnit, text, insertionLineStart)
          else
            None

        config.mode match
          case Mode.ReturnComment =>
            val managedLines = NormalizedTypeRenderer.managedLines(tree.tpt.tpe, config.renderSettings)
            commentOpt match
              case Some(comment) if isBlockComment(comment) =>
                patchSpan(comment.span, updateExistingBlockComment(comment, text, managedLines, newline))
              case _ =>
                patchSpan(Span(insertionLineStart), newManagedBlock(indent, managedLines, newline))
          case Mode.EffectScaladoc =>
            effectScaladocLines(tree.tpt.tpe).foreach { managedLines =>
              commentOpt match
                case Some(comment) if isBlockComment(comment) =>
                  patchSpan(comment.span, updateExistingEffectScaladoc(comment, text, managedLines, newline))
                case _ =>
                  patchSpan(Span(insertionLineStart), newEffectScaladoc(indent, managedLines, newline))
            }

    private def effectScaladocLines(tpe: Type)(using Context): Option[Seq[String]] =
      effectTypeArguments(tpe).map { (errorType, resultType) =>
        val errorRenderSettings = config.renderSettings.copy(showTypeParamNames = false)
        val resultRenderSettings = errorRenderSettings.copy(dealiasTypes = false)
        val excluded = config.effectSettings.excludeErrorTypeRegexes
        val inferredErrors = NormalizedTypeRenderer
          .unionEntries(errorType, errorRenderSettings)
          .filterNot { rendered =>
            excluded.exists(pattern =>
              pattern.matcher(rendered.fullName).matches() || pattern.matcher(rendered.display).matches()
            )
          }
          .map(_.display)
        val errors = (inferredErrors ++ config.effectSettings.additionalErrorTypes).distinct.sorted match
          case Nil => List("Nothing")
          case values => values
        val results = NormalizedTypeRenderer
          .unionEntries(resultType, resultRenderSettings)
          .map(_.display)
          .distinct
          .sorted match
            case Nil => List("Nothing")
            case values => values

        Seq(EffectManagedStart, "Errors:") ++
          errors.map(value => s"  - $value") ++
          Seq("", "Returns:") ++
          results.map(value => s"  - $value") ++
          Seq(EffectManagedEnd)
      }

    private def effectTypeArguments(tpe: Type)(using Context): Option[(Type, Type)] = {
      def extract(normalized: Type): Option[(Type, Type)] = normalized match
        case AppliedType(tycon, args) =>
          val effectName = NormalizedTypeRenderer.fullTypeName(tycon)
          if !config.effectSettings.effectTypeRegex.matcher(effectName).matches() then None
          else
            val byName = tycon.typeParams.map(_.paramName.show).zip(args).toMap
            for
              errorType <- byName.get(config.effectSettings.errorTypeParam)
              resultType <- byName.get(config.effectSettings.resultTypeParam)
            yield (errorType, resultType)
        case _ => None

      extract(tpe.widen.simplified.normalized)
        .orElse(extract(tpe.widenDealias.simplified.normalized.dealias))
    }

    private def isSkipped(symbol: Symbol, name: String)(using Context): Boolean =
      symbol == null ||
      symbol == dotty.tools.dotc.core.Symbols.NoSymbol ||
      symbol.isConstructor ||
      symbol.is(Flags.Synthetic) ||
      !methodNameMatches(name) ||
      !scopeMatches(symbol)

    private def methodNameMatches(name: String): Boolean =
      config.methodSteps
        .foldLeft(Option(name)) { (currentName, step) =>
          currentName.flatMap { value =>
            val matcher = step.pattern.matcher(value)
            if !matcher.matches() then None
            else
              step.rewrite match
                case Some(rewrite) => Some(applyMethodRegexRewrite(matcher, rewrite))
                case None => Some(value)
          }
        }
        .isDefined

    private def scopeMatches(symbol: Symbol)(using Context): Boolean =
      val owner = symbol.denot.maybeOwner
      config.scope match
        case Scope.All => true
        case Scope.Members => owner.isClass
        case Scope.NonPrivate =>
          owner.isClass && !symbol.isOneOf(Flags.Private | Flags.PrivateLocal)

    private def patchSpan(span: Span, replacement: String)(using ctx: Context): Unit =
      if !Rewrites.overlapsPatch(ctx.compilationUnit.source, span) then
        Rewrites.patch(span, replacement)

    private def nearestAttachedComment(unit: CompilationUnit, text: String, defLineStart: Int): Option[Comment] =
      orderedComments(unit)
        .reverseIterator
        .filter(_.span.end <= defLineStart)
        .find(comment => isAttachedGap(text.substring(comment.span.end, defLineStart)))

    private def orderedComments(unit: CompilationUnit): IndexedSeq[Comment] =
      orderedCommentsByUnit.getOrElseUpdate(unit, unit.comments.sortBy(_.span.end).toIndexedSeq)

    private def isAttachedGap(gap: String): Boolean =
      gap.forall(_.isWhitespace) && normalizedNewlineCount(gap) <= 1

    private def isBlockComment(comment: Comment): Boolean =
      comment.raw.startsWith("/*")

    private def newManagedBlock(indent: String, managedLines: Seq[String], newline: String): String = {
      val body = managedLines.map(line => s"${indent} * $line").mkString(newline)
      s"${indent}/*$newline$body$newline${indent} */$newline"
    }

    private def newEffectScaladoc(indent: String, managedLines: Seq[String], newline: String): String = {
      val body = managedLines.map {
        case "" => s"${indent} *"
        case line => s"${indent} * $line"
      }.mkString(newline)
      s"${indent}/**$newline$body$newline${indent} */$newline"
    }

    private def updateExistingEffectScaladoc(
        comment: Comment,
        text: String,
        managedLines: Seq[String],
        sourceNewline: String
    ): String = {
      val raw = comment.raw
      val commentIndent = text.substring(lineStart(text, comment.span.start), comment.span.start)
      val newline = detectNewline(raw, sourceNewline)
      val normalizedRaw =
        if raw.contains(newline) then raw
        else expandSingleLineBlock(raw, commentIndent, newline)
      val scaladocRaw =
        if normalizedRaw.startsWith("/**") then normalizedRaw
        else "/**" + normalizedRaw.stripPrefix("/*")
      updateEffectScaladocLines(scaladocRaw, commentIndent, managedLines, newline)
    }

    private def updateEffectScaladocLines(
        raw: String,
        commentIndent: String,
        managedLines: Seq[String],
        newline: String
    ): String = {
      val lines = ArrayBuffer.from(raw.split(Pattern.quote(newline), -1).toSeq)
      val start = lines.indexWhere(_.contains(EffectManagedStart))
      val end = lines.indexWhere(_.contains(EffectManagedEnd))
      val linePrefix = preferredBlockLinePrefix(lines.toSeq, commentIndent)
      val managedRawLines = managedLines.map {
        case "" => linePrefix.stripSuffix(" ")
        case line => linePrefix + line
      }

      val insertAt =
        if start >= 0 && end >= start then {
          val markerOffset = lines(start).indexOf(EffectManagedStart)
          val markerIsOnOpener = lines(start).take(markerOffset).trim.endsWith("/**")
          if markerIsOnOpener then {
            lines(start) = lines(start).take(markerOffset).stripTrailing()
            lines.remove(start + 1, end - start)
            start + 1
          } else {
            lines.remove(start, end - start + 1)
            start
          }
        } else {
          val tagIndex = lines.indexWhere(line => stripCommentLinePrefix(line).trim.startsWith("@"))
          val closingIndex = lines.lastIndexWhere(_.contains("*/"))
          if tagIndex >= 0 then tagIndex
          else if closingIndex >= 0 then closingIndex
          else lines.length
        }

      val needsLeadingBlank = insertAt > 1 && stripCommentLinePrefix(lines(insertAt - 1)).trim.nonEmpty
      val needsTrailingBlank =
        insertAt < lines.length &&
          !lines(insertAt).contains("*/") &&
          stripCommentLinePrefix(lines(insertAt)).trim.nonEmpty
      val block = ArrayBuffer.empty[String]
      if needsLeadingBlank then block += linePrefix.stripSuffix(" ")
      block ++= managedRawLines
      if needsTrailingBlank then block += linePrefix.stripSuffix(" ")
      lines.insertAll(insertAt, block)
      lines.mkString(newline)
    }

    private def updateExistingBlockComment(comment: Comment, text: String, managedLines: Seq[String], sourceNewline: String): String =
      val raw = comment.raw
      val commentIndent = text.substring(lineStart(text, comment.span.start), comment.span.start)
      val newline = detectNewline(raw, sourceNewline)
      val normalizedRaw =
        if raw.contains(newline) then raw
        else expandSingleLineBlock(raw, commentIndent, newline)
      updateMultilineBlock(normalizedRaw, commentIndent, managedLines, newline)

    private def updateMultilineBlock(raw: String, commentIndent: String, managedLines: Seq[String], newline: String): String =
      val lines = ArrayBuffer.from(raw.split(Pattern.quote(newline), -1).toSeq)
      val managedRanges = managedEntryRanges(lines.toSeq, config.renderSettings.managedTag)
      val linePrefix = preferredBlockLinePrefix(lines.toSeq, commentIndent)
      val managedRawLines = managedLines.map(line => linePrefix + line)

      managedRanges.reverse.foreach { (start, end) =>
        lines.remove(start, end - start)
      }

      val insertAt =
        managedRanges.headOption.map(_._1).getOrElse {
          val closingIndex = lines.lastIndexWhere(_.contains("*/"))
          if closingIndex >= 0 then closingIndex else lines.length
        }

      lines.insertAll(insertAt, managedRawLines)

      lines.mkString(newline)

    private def expandSingleLineBlock(raw: String, commentIndent: String, newline: String): String =
      val opener = if raw.startsWith("/**") then "/**" else "/*"
      val body = raw.stripPrefix(opener).stripSuffix("*/").trim
      val lines = ArrayBuffer(opener)
      if body.nonEmpty then lines += s"$commentIndent * $body"
      lines += s"$commentIndent */"
      lines.mkString(newline)

    private def preferredBlockLinePrefix(lines: Seq[String], commentIndent: String): String =
      val hasStarStyle = lines.exists { line =>
        val trimmed = line.trim
        trimmed.startsWith("*") && !trimmed.startsWith("*/")
      }
      if hasStarStyle then s"$commentIndent * " else s"$commentIndent "

    private def managedEntryRanges(lines: Seq[String], managedTag: String): List[(Int, Int)] = {
      val ranges = ArrayBuffer.empty[(Int, Int)]
      var index = 0
      while index < lines.length do
        if isManagedStartLine(lines(index), managedTag) then
          val start = index
          index += 1
          while index < lines.length && isManagedContinuationLine(lines(index)) do
            index += 1
          ranges += ((start, index))
        else
          index += 1
      ranges.toList
    }

    private def isManagedStartLine(line: String, managedTag: String): Boolean = {
      val content = stripCommentLinePrefix(line)
      content == managedTag || content.startsWith(s"$managedTag ")
    }

    private def isManagedContinuationLine(line: String): Boolean = {
      val content = stripCommentLinePrefix(line)
      content.startsWith(ManagedContinuationPrefix) && content.length > ManagedContinuationPrefix.length
    }

    private def stripCommentLinePrefix(line: String): String =
      val withoutIndent = line.dropWhile(_.isWhitespace)
      val withoutStar = withoutIndent.stripPrefix("*")
      if withoutStar.startsWith(" ") then withoutStar.drop(1) else withoutStar

    private def detectNewline(text: String, fallback: String = "\n"): String =
      if text.contains("\r\n") then "\r\n"
      else if text.contains('\n') then "\n"
      else fallback

    private def normalizedNewlineCount(text: String): Int =
      text.foldLeft((0, false)) {
        case ((count, _), '\r') => (count + 1, true)
        case ((count, true), '\n') => (count, false)
        case ((count, false), '\n') => (count + 1, false)
        case ((count, _), _) => (count, false)
      }._1

    private def sourceText(source: SourceFile): String =
      new String(source.content)

    private def lineStart(text: String, offset: Int): Int =
      var index = math.min(offset, text.length)
      while index > 0 && text.charAt(index - 1) != '\n' && text.charAt(index - 1) != '\r' do
        index -= 1
      index

    private def declarationAnchorLineStart(text: String, defLineStart: Int): Int = {
      val precedingLines = contiguousNonBlankLinesBefore(text, defLineStart)
      var anchorLineStart = defLineStart
      var delimiterBalance = DelimiterBalance.Zero
      var sawAnnotation = isAnnotationLine(lineText(text, defLineStart).trim)
      var stop = false
      var index = precedingLines.length - 1

      while index >= 0 && !stop do
        val (lineStart, line) = precedingLines(index)
        val trimmed = line.trim
        if isCommentLine(trimmed) then
          if sawAnnotation then anchorLineStart = lineStart
        else
          val delimiterDelta = backwardDelimiterDelta(trimmed)
          if sawAnnotation && delimiterBalance.isZero &&
              !isAnnotationLine(trimmed) && !delimiterDelta.hasPositive then
            stop = true
          else
            delimiterBalance = delimiterBalance + delimiterDelta
            if isAnnotationLine(trimmed) && delimiterBalance.isZero then
              sawAnnotation = true
              anchorLineStart = lineStart
            else if sawAnnotation && delimiterBalance.nonZero then
              anchorLineStart = lineStart
            else if sawAnnotation then
              stop = true
            else if delimiterBalance.isZero then
              stop = true
        index -= 1

      anchorLineStart
    }

    private def declarationHasAnnotations(text: String, defLineStart: Int): Boolean =
      isAnnotationLine(lineText(text, defLineStart).trim) ||
        contiguousNonBlankLinesBefore(text, defLineStart).exists { (_, line) =>
          isAnnotationLine(line.trim)
        }

    private def contiguousNonBlankLinesBefore(text: String, defLineStart: Int): IndexedSeq[(Int, String)] = {
      val lines = ArrayBuffer.empty[(Int, String)]
      var currentLineStart = defLineStart
      var continue = true

      while continue do
        previousLineStart(text, currentLineStart) match
          case Some(previousStart) =>
            val previousText = lineText(text, previousStart)
            if previousText.trim.isEmpty then
              continue = false
            else
              lines.prepend((previousStart, previousText))
              currentLineStart = previousStart
          case None =>
            continue = false

      lines.toIndexedSeq
    }

    private final case class DelimiterBalance(parens: Int, brackets: Int, braces: Int) {
      def +(other: DelimiterBalance): DelimiterBalance =
        DelimiterBalance(parens + other.parens, brackets + other.brackets, braces + other.braces)

      def isZero: Boolean =
        parens == 0 && brackets == 0 && braces == 0

      def nonZero: Boolean =
        !isZero

      def hasPositive: Boolean =
        parens > 0 || brackets > 0 || braces > 0
    }

    private object DelimiterBalance {
      val Zero: DelimiterBalance = DelimiterBalance(0, 0, 0)
    }

    private def backwardDelimiterDelta(line: String): DelimiterBalance = {
      var parens = 0
      var brackets = 0
      var braces = 0
      var inSingleQuoted = false
      var inDoubleQuoted = false
      var escaped = false

      line.foreach {
        case _ if escaped =>
          escaped = false
        case '\\' if inSingleQuoted || inDoubleQuoted =>
          escaped = true
        case '\'' if !inDoubleQuoted =>
          inSingleQuoted = !inSingleQuoted
        case '"' if !inSingleQuoted =>
          inDoubleQuoted = !inDoubleQuoted
        case ')' if !inSingleQuoted && !inDoubleQuoted =>
          parens += 1
        case '(' if !inSingleQuoted && !inDoubleQuoted =>
          parens -= 1
        case ']' if !inSingleQuoted && !inDoubleQuoted =>
          brackets += 1
        case '[' if !inSingleQuoted && !inDoubleQuoted =>
          brackets -= 1
        case '}' if !inSingleQuoted && !inDoubleQuoted =>
          braces += 1
        case '{' if !inSingleQuoted && !inDoubleQuoted =>
          braces -= 1
        case _ =>
      }

      DelimiterBalance(parens, brackets, braces)
    }

    private def indentationEnd(text: String, lineStart: Int): Int =
      var index = lineStart
      while index < text.length && {
          val ch = text.charAt(index)
          ch != '\n' && ch != '\r' && ch.isWhitespace
        }
      do index += 1
      index

    private def previousLineStart(text: String, currentLineStart: Int): Option[Int] =
      Option.when(currentLineStart > 0) {
        val previousLineEnd =
          if text.charAt(currentLineStart - 1) == '\n' && currentLineStart >= 2 && text.charAt(currentLineStart - 2) == '\r' then
            currentLineStart - 2
          else if text.charAt(currentLineStart - 1) == '\n' || text.charAt(currentLineStart - 1) == '\r' then
            currentLineStart - 1
          else
            currentLineStart
        lineStart(text, previousLineEnd)
      }

    private def lineText(text: String, lineStart: Int): String = {
      var index = lineStart
      while index < text.length && text.charAt(index) != '\n' && text.charAt(index) != '\r' do
        index += 1
      text.substring(lineStart, index)
    }

    private def isAnnotationLine(line: String): Boolean =
      line.startsWith("@")

    private def isCommentLine(line: String): Boolean =
      line.startsWith("//") || line.startsWith("/*") || line.startsWith("*") || line.startsWith("*/")

  }
}
