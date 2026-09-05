package explicitlyinferred

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Comments.Comment
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.report
import dotty.tools.dotc.rewrites.Rewrites
import dotty.tools.dotc.transform.Pickler
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.util.Spans.Span

import scala.collection.mutable

final class ExplicitlyInferredPlugin extends StandardPlugin {
  override val name = PluginMetadata.Name
  override val description = PluginMetadata.Description
  override val optionsHelp = Some(PluginConfig.OptionsHelp)

  override def initialize(options: List[String])(using Context): List[PluginPhase] =
    try new ExplicitlyInferredPhase(PluginConfig.parse(options)) :: Nil
    catch
      case error: IllegalArgumentException =>
        report.error(error.getMessage)
        Nil
}

private final class ExplicitlyInferredPhase(config: PluginConfig) extends PluginPhase {
  import tpd.*

  private val unitStates = mutable.HashMap.empty[CompilationUnit, UnitState]
  private val documentationBuilder = new TypeDocumentationBuilder(config.documentation)

  override val phaseName = "explicitlyInferredPhase"
  override val runsAfter = Set(TyperPhase.name)
  override val runsBefore = Set(Pickler.name)

  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    rewrite(tree)
    tree
  }

  private def rewrite(tree: DefDef)(using context: Context): Unit =
    if eligible(tree) then
      documentationBuilder.from(tree.tpt.tpe).foreach { documentation =>
        val unit = context.compilationUnit
        val state = unitState(unit)
        val layout = SourceLayout.declaration(state.text, tree.span.start, state.newline)
        val attachedComment =
          if layout.hasAnnotations then None
          else state.nearestAttachedBlockComment(layout.insertionOffset)

        attachedComment match
          case Some(comment) =>
            patch(
              comment.span,
              ScaladocEditor.update(
                comment.raw,
                SourceLayout.commentIndentation(state.text, comment.span.start),
                documentation,
                config.documentation.markers,
                layout.newline
              )
            )
          case None =>
            patch(
              Span(layout.insertionOffset),
              ScaladocEditor.create(layout.indentation, documentation, config.documentation.markers, layout.newline)
            )
      }

  private def eligible(tree: DefDef)(using context: Context): Boolean = {
    val symbol = tree.symbol
    context.compilationUnit.source.exists &&
      tree.tpt.isInstanceOf[InferredTypeTree] &&
      symbol != null &&
      symbol != NoSymbol &&
      !symbol.isConstructor &&
      !symbol.is(Flags.Synthetic) &&
      config.methodMatcher.matches(tree.name.toString) &&
      scopeMatches(symbol)
  }

  private def scopeMatches(symbol: Symbol)(using Context): Boolean = {
    val owner = symbol.denot.maybeOwner
    config.scope match
      case Scope.All => true
      case Scope.Members => owner.isClass
      case Scope.NonPrivate => owner.isClass && !symbol.isOneOf(Flags.Private | Flags.PrivateLocal)
  }

  private def unitState(unit: CompilationUnit): UnitState =
    unitStates.getOrElseUpdate(unit, UnitState(unit))

  private def patch(span: Span, replacement: String)(using context: Context): Unit =
    if !Rewrites.overlapsPatch(context.compilationUnit.source, span) then Rewrites.patch(span, replacement)

  private final class UnitState(val text: String, val newline: String, comments: IndexedSeq[Comment]) {
    private val commentEnds = comments.map(_.span.end).toArray

    def nearestAttachedBlockComment(declarationStart: Int): Option[Comment] = {
      val index = upperBound(commentEnds, declarationStart) - 1
      Option.when(index >= 0)(comments(index))
        .filter(comment => SourceLayout.isAttachedGap(text, comment.span.end, declarationStart))
        .filter(_.raw.startsWith("/*"))
    }
  }

  private object UnitState {
    def apply(unit: CompilationUnit): UnitState = {
      val text = new String(unit.source.content)
      new UnitState(text, SourceLayout.detectNewline(text), unit.comments.sortBy(_.span.end).toIndexedSeq)
    }
  }

  private def upperBound(values: Array[Int], target: Int): Int = {
    var low = 0
    var high = values.length
    while low < high do
      val middle = low + (high - low) / 2
      if values(middle) <= target then low = middle + 1 else high = middle
    low
  }
}
