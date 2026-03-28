package commenter

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Comments.Comment
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
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
      |-P:inferredReturnComment:scope=members|all|nonPrivate""".stripMargin
  )

  override def initialize(options: List[String])(using Context): List[PluginPhase] =
    parseConfig(options) match
      case Some(config) => List(new InferredReturnCommentPhase(config))
      case None => Nil

  private def parseConfig(options: List[String])(using Context): Option[Config] =
    var methodRegex = ".*"
    var scope = Scope.Members
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

    if ok then pattern.map(Config(_, scope)) else None
}

object InferredReturnCommentPlugin {
  private val PluginName = "inferredReturnComment"
  private val ManagedPrefix = "@inferredReturnType "

  private final case class Config(methodPattern: Pattern, scope: Scope)

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
        val renderedType = tree.tpt.tpe.show
        val managedLine = ManagedPrefix + renderedType
        val envelope = tree.envelope(source)
        val nameStart = tree.span.start
        val defStart = findKeyword(text, envelope.start, nameStart, "def").getOrElse(nameStart)
        val defLineStart = lineStart(text, defStart)
        val indent = text.substring(defLineStart, defStart)
        val newline = detectNewline(text)
        val commentOpt = nearestAttachedComment(ctx.compilationUnit, text, defLineStart)

        commentOpt match
          case Some(comment) if isBlockComment(comment) =>
            patchSpan(comment.span, updateExistingBlockComment(comment, text, managedLine, newline))
          case _ =>
            patchSpan(Span(defLineStart), newManagedBlock(indent, managedLine, newline))

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

    private def newManagedBlock(indent: String, managedLine: String, newline: String): String =
      s"${indent}/*$newline${indent} * $managedLine$newline${indent} */$newline"

    private def updateExistingBlockComment(comment: Comment, text: String, managedLine: String, sourceNewline: String): String =
      val raw = comment.raw
      val commentIndent = text.substring(lineStart(text, comment.span.start), comment.span.start)
      val newline = detectNewline(raw, sourceNewline)
      if raw.contains(newline) then
        updateMultilineBlock(raw, commentIndent, managedLine, newline)
      else
        expandSingleLineBlock(raw, commentIndent, managedLine, newline)

    private def updateMultilineBlock(raw: String, commentIndent: String, managedLine: String, newline: String): String =
      val lines = ArrayBuffer.from(raw.split(Pattern.quote(newline), -1))
      val closingIndex = lines.lastIndexWhere(_.contains("*/"))
      val managedIndexes = lines.indices.filter(index => stripManagedLinePrefix(lines(index)).startsWith(ManagedPrefix))
      val linePrefix = preferredBlockLinePrefix(lines.toSeq, commentIndent)
      val managedRawLine = linePrefix + managedLine

      managedIndexes.headOption match
        case Some(index) =>
          lines(index) = managedRawLine
          managedIndexes.drop(1).reverse.foreach(lines.remove)
        case None if closingIndex >= 0 =>
          lines.insert(closingIndex, managedRawLine)
        case None =>
          lines += managedRawLine

      lines.mkString(newline)

    private def expandSingleLineBlock(raw: String, commentIndent: String, managedLine: String, newline: String): String =
      val opener = if raw.startsWith("/**") then "/**" else "/*"
      val body = raw.stripPrefix(opener).stripSuffix("*/").trim
      val lines = ArrayBuffer(opener)
      if body.nonEmpty then lines += s"$commentIndent * $body"
      lines += s"$commentIndent * $managedLine"
      lines += s"$commentIndent */"
      lines.mkString(newline)

    private def preferredBlockLinePrefix(lines: Seq[String], commentIndent: String): String =
      val hasStarStyle = lines.exists { line =>
        val trimmed = line.trim
        trimmed.startsWith("*") && !trimmed.startsWith("*/")
      }
      if hasStarStyle then s"$commentIndent * " else s"$commentIndent "

    private def stripManagedLinePrefix(line: String): String =
      line.trim.stripPrefix("*").trim

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
