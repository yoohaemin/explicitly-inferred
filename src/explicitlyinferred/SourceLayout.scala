package explicitlyinferred

import scala.collection.mutable.ArrayBuffer

private[explicitlyinferred] final case class DeclarationLayout(
    insertionOffset: Int,
    indentation: String,
    newline: String,
    hasAnnotations: Boolean
)

private[explicitlyinferred] object SourceLayout {
  def declaration(text: String, treeStart: Int): DeclarationLayout = {
    val declarationLine = lineStart(text, treeStart)
    val insertionOffset = declarationAnchor(text, declarationLine)
    DeclarationLayout(
      insertionOffset,
      text.substring(insertionOffset, indentationEnd(text, insertionOffset)),
      detectNewline(text),
      declarationHasAnnotations(text, declarationLine)
    )
  }

  def commentIndentation(text: String, commentStart: Int): String =
    text.substring(lineStart(text, commentStart), commentStart)

  def isAttachedGap(gap: String): Boolean =
    gap.forall(_.isWhitespace) && newlineCount(gap) <= 1

  def detectNewline(text: String, fallback: String = "\n"): String =
    if text.contains("\r\n") then "\r\n"
    else if text.contains('\n') then "\n"
    else fallback

  private def newlineCount(text: String): Int =
    text.foldLeft((0, false)) {
      case ((count, _), '\r') => (count + 1, true)
      case ((count, true), '\n') => (count, false)
      case ((count, false), '\n') => (count + 1, false)
      case ((count, _), _) => (count, false)
    }._1

  private def declarationAnchor(text: String, declarationLine: Int): Int = {
    val precedingLines = contiguousNonBlankLinesBefore(text, declarationLine)
    var anchor = declarationLine
    var delimiterBalance = DelimiterBalance.Zero
    var sawAnnotation = isAnnotationLine(lineText(text, declarationLine).trim)
    var stop = false
    var index = precedingLines.length - 1

    while index >= 0 && !stop do
      val (lineStart, line) = precedingLines(index)
      val trimmed = line.trim
      if isCommentLine(trimmed) then
        if sawAnnotation then anchor = lineStart
      else
        val delimiterDelta = backwardDelimiterDelta(trimmed)
        if sawAnnotation && delimiterBalance.isZero &&
            !isAnnotationLine(trimmed) && !delimiterDelta.hasPositive then
          stop = true
        else
          delimiterBalance = delimiterBalance + delimiterDelta
          if isAnnotationLine(trimmed) && delimiterBalance.isZero then
            sawAnnotation = true
            anchor = lineStart
          else if sawAnnotation && delimiterBalance.nonZero then
            anchor = lineStart
          else if sawAnnotation then
            stop = true
          else if delimiterBalance.isZero then
            stop = true
      index -= 1

    anchor
  }

  private def declarationHasAnnotations(text: String, declarationLine: Int): Boolean =
    isAnnotationLine(lineText(text, declarationLine).trim) ||
      contiguousNonBlankLinesBefore(text, declarationLine).exists { (_, line) => isAnnotationLine(line.trim) }

  private def contiguousNonBlankLinesBefore(text: String, currentLine: Int): IndexedSeq[(Int, String)] = {
    val lines = ArrayBuffer.empty[(Int, String)]
    var line = currentLine
    var continue = true

    while continue do
      previousLineStart(text, line) match
        case Some(previousStart) =>
          val previousText = lineText(text, previousStart)
          if previousText.trim.isEmpty then continue = false
          else
            lines.prepend(previousStart -> previousText)
            line = previousStart
        case None => continue = false

    lines.toIndexedSeq
  }

  private final case class DelimiterBalance(parens: Int, brackets: Int, braces: Int) {
    def +(other: DelimiterBalance): DelimiterBalance =
      DelimiterBalance(parens + other.parens, brackets + other.brackets, braces + other.braces)

    def isZero: Boolean = parens == 0 && brackets == 0 && braces == 0
    def nonZero: Boolean = !isZero
    def hasPositive: Boolean = parens > 0 || brackets > 0 || braces > 0
  }

  private object DelimiterBalance {
    val Zero = DelimiterBalance(0, 0, 0)
  }

  private def backwardDelimiterDelta(line: String): DelimiterBalance = {
    var parens = 0
    var brackets = 0
    var braces = 0
    var inSingleQuoted = false
    var inDoubleQuoted = false
    var escaped = false

    line.foreach {
      case _ if escaped => escaped = false
      case '\\' if inSingleQuoted || inDoubleQuoted => escaped = true
      case '\'' if !inDoubleQuoted => inSingleQuoted = !inSingleQuoted
      case '"' if !inSingleQuoted => inDoubleQuoted = !inDoubleQuoted
      case ')' if !inSingleQuoted && !inDoubleQuoted => parens += 1
      case '(' if !inSingleQuoted && !inDoubleQuoted => parens -= 1
      case ']' if !inSingleQuoted && !inDoubleQuoted => brackets += 1
      case '[' if !inSingleQuoted && !inDoubleQuoted => brackets -= 1
      case '}' if !inSingleQuoted && !inDoubleQuoted => braces += 1
      case '{' if !inSingleQuoted && !inDoubleQuoted => braces -= 1
      case _ =>
    }

    DelimiterBalance(parens, brackets, braces)
  }

  private def lineStart(text: String, offset: Int): Int = {
    var index = math.min(offset, text.length)
    while index > 0 && text.charAt(index - 1) != '\n' && text.charAt(index - 1) != '\r' do index -= 1
    index
  }

  private def indentationEnd(text: String, start: Int): Int = {
    var index = start
    while index < text.length && {
        val char = text.charAt(index)
        char != '\n' && char != '\r' && char.isWhitespace
      }
    do index += 1
    index
  }

  private def previousLineStart(text: String, currentLineStart: Int): Option[Int] =
    Option.when(currentLineStart > 0) {
      val previousLineEnd =
        if text.charAt(currentLineStart - 1) == '\n' && currentLineStart >= 2 && text.charAt(currentLineStart - 2) == '\r' then
          currentLineStart - 2
        else if text.charAt(currentLineStart - 1) == '\n' || text.charAt(currentLineStart - 1) == '\r' then
          currentLineStart - 1
        else currentLineStart
      lineStart(text, previousLineEnd)
    }

  private def lineText(text: String, start: Int): String = {
    var index = start
    while index < text.length && text.charAt(index) != '\n' && text.charAt(index) != '\r' do index += 1
    text.substring(start, index)
  }

  private def isAnnotationLine(line: String): Boolean = line.startsWith("@")

  private def isCommentLine(line: String): Boolean =
    line.startsWith("//") || line.startsWith("/*") || line.startsWith("*") || line.startsWith("*/")
}
