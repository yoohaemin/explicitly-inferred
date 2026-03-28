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
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

final class InferredReturnCommentPlugin extends StandardPlugin {
  import InferredReturnCommentPlugin.*

  override val name: String = PluginName
  override val description: String = "Adds inferred return-type comments during -rewrite"
  override val optionsHelp: Option[String] = Some(
    """-P:inferredReturnComment:methodRegex=<java-regex>
      |-P:inferredReturnComment:scope=members|all|nonPrivate
      |-P:inferredReturnComment:maxTypeLength=<positive-int>
      |-P:inferredReturnComment:showTypeArgs=true|false
      |-P:inferredReturnComment:showTypeParamNames=true|false""".stripMargin
  )

  override def initialize(options: List[String])(using Context): List[PluginPhase] =
    parseConfig(options) match
      case Some(config) => List(new InferredReturnCommentPhase(config))
      case None => Nil

  private def parseConfig(options: List[String])(using Context): Option[Config] =
    var methodRegex = ".*"
    var scope = Scope.Members
    var maxTypeLength = 80
    var showTypeArgs = true
    var showTypeParamNames = true
    var ok = true

    options.foreach {
      case option if option.startsWith("methodRegex=") =>
        methodRegex = option.stripPrefix("methodRegex=")
      case option if option.startsWith("scope=") =>
        Scope.fromOption(option.stripPrefix("scope=")) match
          case Some(value) => scope = value
          case None =>
            ok = false
            throw new IllegalArgumentException(s"Unknown $PluginName scope: ${option.stripPrefix("scope=")}")
      case option if option.startsWith("maxTypeLength=") =>
        IntOption.fromOption(option.stripPrefix("maxTypeLength=")) match
          case Some(value) => maxTypeLength = value
          case None =>
            ok = false
            throw new IllegalArgumentException(s"Invalid $PluginName maxTypeLength: ${option.stripPrefix("maxTypeLength=")}")
      case option if option.startsWith("showTypeArgs=") =>
        BooleanOption.fromOption(option.stripPrefix("showTypeArgs=")) match
          case Some(value) => showTypeArgs = value
          case None =>
            ok = false
            throw new IllegalArgumentException(s"Invalid $PluginName showTypeArgs: ${option.stripPrefix("showTypeArgs=")}")
      case option if option.startsWith("showTypeParamNames=") =>
        BooleanOption.fromOption(option.stripPrefix("showTypeParamNames=")) match
          case Some(value) => showTypeParamNames = value
          case None =>
            ok = false
            throw new IllegalArgumentException(s"Invalid $PluginName showTypeParamNames: ${option.stripPrefix("showTypeParamNames=")}")
      case option =>
        ok = false
        throw new IllegalArgumentException(s"Unknown $PluginName option: $option")
    }

    val pattern =
      Try(Pattern.compile(methodRegex)).toOption.orElse {
        ok = false
        throw new IllegalArgumentException(s"Invalid $PluginName methodRegex: $methodRegex")
        None
      }

    if ok then
      pattern.map(Config(_, scope, RenderSettings(maxTypeLength, showTypeArgs, showTypeParamNames)))
    else None
}

object InferredReturnCommentPlugin {
  private val PluginName = "inferredReturnComment"
  private val ManagedTag = "@inferredReturnType"
  private val ManagedPrefix = "@inferredReturnType "
  private val ManagedContinuationPrefix = "  "

  private final case class Config(methodPattern: Pattern, scope: Scope, renderSettings: RenderSettings)
  private final case class RenderSettings(maxTypeLength: Int, showTypeArgs: Boolean, showTypeParamNames: Boolean)

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

  private object BooleanOption {
    def fromOption(value: String): Option[Boolean] = value match
      case "true" => Some(true)
      case "false" => Some(false)
      case _ => None
  }

  private object NormalizedTypeRenderer {
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

    private final case class RenderedArg(label: Option[String], value: TypeNode, sortKey: String)

    def managedLines(tpe: Type, settings: RenderSettings)(using Context): Seq[String] = {
      val node = toNode(tpe, settings)
      val singleLine = renderSingle(node)
      if ManagedPrefix.length + singleLine.length <= settings.maxTypeLength then
        Seq(ManagedPrefix + singleLine)
      else
        ManagedTag +: renderBlock(node).map(line => ManagedContinuationPrefix + line)
    }

    private def toNode(tpe: Type, settings: RenderSettings)(using Context): TypeNode = {
      val normalized = normalizeType(tpe)
      normalized match
        case tp: OrType =>
          val parts = flattenOr(tp).map(part => toNode(part, settings)).sortBy(_.sortKey)
          val deduped = dedupe(parts)
          if deduped.size == 1 then deduped.head
          else Union(deduped, deduped.map(_.sortKey).mkString("or(", ",", ")"))
        case tp: AndType =>
          val parts = flattenAnd(tp).map(part => toNode(part, settings)).sortBy(_.sortKey)
          val deduped = dedupe(parts)
          if deduped.size == 1 then deduped.head
          else Intersection(deduped, deduped.map(_.sortKey).mkString("and(", ",", ")"))
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
          leafFor(normalized)
    }

    private def leafFor(tpe: Type)(using Context): TypeNode = {
      val display = displayName(tpe)
      Leaf(display, stableLeafKey(tpe, display))
    }

    private def normalizeType(tpe: Type)(using Context): Type =
      tpe.widenDealias.simplified.normalized.dealias

    private def flattenOr(tpe: Type)(using Context): List[Type] =
      normalizeType(tpe) match
        case OrType(left, right) => flattenOr(left) ::: flattenOr(right)
        case other => other :: Nil

    private def flattenAnd(tpe: Type)(using Context): List[Type] =
      normalizeType(tpe) match
        case AndType(left, right) => flattenAnd(left) ::: flattenAnd(right)
        case other => other :: Nil

    private def dedupe(nodes: List[TypeNode]): List[TypeNode] =
      nodes.foldLeft(List.empty[TypeNode]) { (acc, node) =>
        if acc.lastOption.exists(_.sortKey == node.sortKey) then acc else acc :+ node
      }

    private def typeArgumentLabels(tycon: Type, argCount: Int, showNames: Boolean)(using Context): List[Option[String]] =
      if !showNames then List.fill(argCount)(None)
      else
        val labels = tycon.typeParams.map(_.paramName.show).take(argCount).map(Some(_))
        labels.padTo(argCount, None)

    private def displayName(tpe: Type)(using Context): String =
      normalizeType(tpe) match
        case tp: TypeRef if tp.symbol.exists =>
          tp.symbol.name.show
        case tp: TermRef if tp.symbol.exists =>
          tp.symbol.name.show
        case tp: ThisType =>
          tp.tref.symbol.name.show
        case tp: ConstantType =>
          tp.show
        case tp: TypeBounds =>
          tp.show
        case tp =>
          tp.show

    private def stableLeafKey(tpe: Type, display: String)(using Context): String = {
      val normalized = normalizeType(tpe)
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
        val managedLines = NormalizedTypeRenderer.managedLines(tree.tpt.tpe, config.renderSettings)
        val envelope = tree.envelope(source)
        val nameStart = tree.span.start
        val defStart = findKeyword(text, envelope.start, nameStart, "def").getOrElse(nameStart)
        val defLineStart = lineStart(text, defStart)
        val indent = text.substring(defLineStart, defStart)
        val newline = detectNewline(text)
        val commentOpt = nearestAttachedComment(ctx.compilationUnit, text, defLineStart)

        commentOpt match
          case Some(comment) if isBlockComment(comment) =>
            patchSpan(comment.span, updateExistingBlockComment(comment, text, managedLines, newline))
          case _ =>
            patchSpan(Span(defLineStart), newManagedBlock(indent, managedLines, newline))

    private def isSkipped(symbol: Symbol, name: String)(using Context): Boolean =
      symbol == null ||
      symbol == dotty.tools.dotc.core.Symbols.NoSymbol ||
      symbol.isConstructor ||
      symbol.is(Flags.Synthetic) ||
      !config.methodPattern.matcher(name).matches() ||
      !scopeMatches(symbol)

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
      unit.comments
        .filter(_.span.end <= defLineStart)
        .sortBy(_.span.end)
        .reverseIterator
        .find(comment => isAttachedGap(text.substring(comment.span.end, defLineStart)))

    private def isAttachedGap(gap: String): Boolean =
      gap.forall(_.isWhitespace) && normalizedNewlineCount(gap) <= 1

    private def isBlockComment(comment: Comment): Boolean =
      comment.raw.startsWith("/*")

    private def newManagedBlock(indent: String, managedLines: Seq[String], newline: String): String = {
      val body = managedLines.map(line => s"${indent} * $line").mkString(newline)
      s"${indent}/*$newline$body$newline${indent} */$newline"
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
      val managedRanges = managedEntryRanges(lines.toSeq)
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

    private def managedEntryRanges(lines: Seq[String]): List[(Int, Int)] = {
      val ranges = ArrayBuffer.empty[(Int, Int)]
      var index = 0
      while index < lines.length do
        if isManagedStartLine(lines(index)) then
          val start = index
          index += 1
          while index < lines.length && isManagedContinuationLine(lines(index)) do
            index += 1
          ranges += ((start, index))
        else
          index += 1
      ranges.toList
    }

    private def isManagedStartLine(line: String): Boolean =
      stripCommentLinePrefix(line).startsWith(ManagedTag)

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

    private def findKeyword(text: String, start: Int, end: Int, keyword: String): Option[Int] =
      val boundedStart = math.max(0, start)
      val boundedEnd = math.min(end, text.length)
      var index = boundedStart
      while index <= boundedEnd - keyword.length do
        if text.regionMatches(index, keyword, 0, keyword.length)
            && isBoundary(text, index - 1)
            && isBoundary(text, index + keyword.length)
        then return Some(index)
        index += 1
      None

    private def isBoundary(text: String, index: Int): Boolean =
      if index < 0 || index >= text.length then true
      else
        val ch = text.charAt(index)
        !(ch.isLetterOrDigit || ch == '_' || ch == '$')
  }
}
