package explicitlyinferred

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

private[explicitlyinferred] object ScaladocEditor {
  def create(indent: String, documentation: TypeDocumentation, markers: Markers, newline: String): String = {
    val body = documentation.managedLines(markers).map {
      case "" => s"$indent *"
      case line => s"$indent * $line"
    }.mkString(newline)
    s"$indent/**$newline$body$newline$indent */$newline"
  }

  def update(
      rawComment: String,
      indent: String,
      documentation: TypeDocumentation,
      markers: Markers,
      sourceNewline: String
  ): String = {
    val newline = SourceLayout.detectNewline(rawComment, sourceNewline)
    val multiline =
      if rawComment.contains(newline) then rawComment
      else expandSingleLine(rawComment, indent, newline)
    val scaladoc =
      if multiline.startsWith("/**") then multiline
      else "/**" + multiline.stripPrefix("/*")
    updateLines(scaladoc, indent, documentation.managedLines(markers), markers, newline)
  }

  private def updateLines(
      raw: String,
      indent: String,
      managedLines: List[String],
      markers: Markers,
      newline: String
  ): String = {
    val lines = ArrayBuffer.from(raw.split(Pattern.quote(newline), -1).toSeq)
    val linePrefix = preferredLinePrefix(lines.toSeq, indent)
    val renderedRegion = managedLines.map {
      case "" => linePrefix.stripSuffix(" ")
      case line => linePrefix + line
    }

    val insertAt =
      managedRegion(lines, markers) match
        case Some((start, end)) => replaceManagedRegion(lines, start, end, markers.startDelimiter)
        case None => insertionPoint(lines)

    val block = ArrayBuffer.empty[String]
    if needsLeadingBlank(lines, insertAt) then block += linePrefix.stripSuffix(" ")
    block ++= renderedRegion
    if needsTrailingBlank(lines, insertAt) then block += linePrefix.stripSuffix(" ")
    lines.insertAll(insertAt, block)
    lines.mkString(newline)
  }

  private def managedRegion(lines: scala.collection.Seq[String], markers: Markers): Option[(Int, Int)] = {
    var start = Option.empty[Int]
    var result = Option.empty[(Int, Int)]
    var index = 0

    while index < lines.length && result.isEmpty do
      if lines(index).contains(markers.startDelimiter) then start = Some(index)
      else if lines(index).contains(markers.endDelimiter) then
        result = start.map(_ -> index)
      index += 1

    result
  }

  private def replaceManagedRegion(
      lines: ArrayBuffer[String],
      start: Int,
      end: Int,
      startDelimiter: String
  ): Int = {
    val markerOffset = lines(start).indexOf(startDelimiter)
    if lines(start).take(markerOffset).trim.endsWith("/**") then {
      lines(start) = lines(start).take(markerOffset).stripTrailing()
      lines.remove(start + 1, end - start)
      start + 1
    } else {
      lines.remove(start, end - start + 1)
      start
    }
  }

  private def insertionPoint(lines: scala.collection.Seq[String]): Int = {
    val tag = lines.indexWhere(line => stripLinePrefix(line).trim.startsWith("@"))
    if tag >= 0 then tag
    else
      val closing = lines.lastIndexWhere(_.contains("*/"))
      if closing >= 0 then closing else lines.length
  }

  private def needsLeadingBlank(lines: scala.collection.Seq[String], insertAt: Int): Boolean =
    insertAt > 1 && stripLinePrefix(lines(insertAt - 1)).trim.nonEmpty

  private def needsTrailingBlank(lines: scala.collection.Seq[String], insertAt: Int): Boolean =
    insertAt < lines.length &&
      !lines(insertAt).contains("*/") &&
      stripLinePrefix(lines(insertAt)).trim.nonEmpty

  private def expandSingleLine(raw: String, indent: String, newline: String): String = {
    val opener = if raw.startsWith("/**") then "/**" else "/*"
    val body = raw.stripPrefix(opener).stripSuffix("*/").trim
    val lines = ArrayBuffer(opener)
    if body.nonEmpty then lines += s"$indent * $body"
    lines += s"$indent */"
    lines.mkString(newline)
  }

  private def preferredLinePrefix(lines: Seq[String], indent: String): String = {
    val starStyle = lines.exists { line =>
      val trimmed = line.trim
      trimmed.startsWith("*") && !trimmed.startsWith("*/")
    }
    if starStyle then s"$indent * " else s"$indent "
  }

  private def stripLinePrefix(line: String): String = {
    val withoutIndent = line.dropWhile(_.isWhitespace)
    val withoutStar = withoutIndent.stripPrefix("*")
    if withoutStar.startsWith(" ") then withoutStar.drop(1) else withoutStar
  }
}
