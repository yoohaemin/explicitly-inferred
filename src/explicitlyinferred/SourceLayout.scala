package explicitlyinferred

private[explicitlyinferred] final case class DeclarationLayout(
    insertionOffset: Int,
    indentation: String,
    newline: String,
    hasAnnotations: Boolean
)

private[explicitlyinferred] object SourceLayout {
  def declaration(text: String, treeStart: Int): DeclarationLayout =
    declaration(text, treeStart, detectNewline(text))

  def declaration(text: String, treeStart: Int, newline: String): DeclarationLayout = {
    val declarationLine = lineStart(text, treeStart)
    val (insertionOffset, hasAnnotations) = declarationAnchor(text, declarationLine)
    DeclarationLayout(
      insertionOffset,
      text.substring(insertionOffset, indentationEnd(text, insertionOffset)),
      newline,
      hasAnnotations
    )
  }

  def commentIndentation(text: String, commentStart: Int): String =
    text.substring(lineStart(text, commentStart), commentStart)

  def isAttachedGap(text: String, start: Int, end: Int): Boolean = {
    var index = start
    var newlines = 0
    while index < end && text.charAt(index).isWhitespace && newlines <= 1 do
      text.charAt(index) match
        case '\r' =>
          newlines += 1
          if index + 1 < end && text.charAt(index + 1) == '\n' then index += 1
        case '\n' => newlines += 1
        case _ =>
      index += 1
    index == end && newlines <= 1
  }

  def detectNewline(text: String, fallback: String = "\n"): String =
    if text.contains("\r\n") then "\r\n"
    else if text.contains('\n') then "\n"
    else fallback

  private def declarationAnchor(text: String, declarationLine: Int): (Int, Boolean) = {
    var anchor = declarationLine
    var delimiterBalance = DelimiterBalance.Zero
    var sawAnnotation = isAnnotationLine(lineText(text, declarationLine).trim)
    var stop = false
    var currentLine = declarationLine

    while currentLine > 0 && !stop do
      previousLineStart(text, currentLine) match
        case None => stop = true
        case Some(previousStart) =>
          val trimmed = lineText(text, previousStart).trim
          if trimmed.isEmpty then stop = true
          else
            if isCommentLine(trimmed) then
              if sawAnnotation then anchor = previousStart
            else
              val delimiterDelta = backwardDelimiterDelta(trimmed)
              if sawAnnotation && delimiterBalance.isZero &&
                  !isAnnotationLine(trimmed) && !delimiterDelta.hasPositive then
                stop = true
              else
                delimiterBalance = delimiterBalance + delimiterDelta
                if isAnnotationLine(trimmed) && delimiterBalance.isZero then
                  sawAnnotation = true
                  anchor = previousStart
                else if sawAnnotation && delimiterBalance.nonZero then
                  anchor = previousStart
                else if sawAnnotation || delimiterBalance.isZero then
                  stop = true
            currentLine = previousStart

    anchor -> sawAnnotation
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
