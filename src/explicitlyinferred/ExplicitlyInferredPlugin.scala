package explicitlyinferred

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Comments.Comment
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
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
    new ExplicitlyInferredPhase(PluginConfig.parse(options)) :: Nil
}

private final class ExplicitlyInferredPhase(config: PluginConfig) extends PluginPhase {
  import tpd.*

  private val commentsByUnit = mutable.HashMap.empty[CompilationUnit, IndexedSeq[Comment]]

  override val phaseName = "explicitlyInferredPhase"
  override val runsAfter = Set(TyperPhase.name)
  override val runsBefore = Set(Pickler.name)

  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    rewrite(tree)
    tree
  }

  private def rewrite(tree: DefDef)(using context: Context): Unit =
    if eligible(tree) then
      EffectDocumentation.from(tree.tpt.tpe, config.effect).foreach { documentation =>
        val source = context.compilationUnit.source
        val text = new String(source.content)
        val layout = SourceLayout.declaration(text, tree.span.start)
        val attachedComment =
          if layout.hasAnnotations then None
          else nearestAttachedBlockComment(context.compilationUnit, text, layout.insertionOffset)

        attachedComment match
          case Some(comment) =>
            patch(
              comment.span,
              ScaladocEditor.update(
                comment.raw,
                SourceLayout.commentIndentation(text, comment.span.start),
                documentation,
                config.effect.markers,
                layout.newline
              )
            )
          case None =>
            patch(
              Span(layout.insertionOffset),
              ScaladocEditor.create(layout.indentation, documentation, config.effect.markers, layout.newline)
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

  private def nearestAttachedBlockComment(
      unit: CompilationUnit,
      text: String,
      declarationStart: Int
  ): Option[Comment] =
    orderedComments(unit)
      .reverseIterator
      .filter(_.span.end <= declarationStart)
      .find(comment => SourceLayout.isAttachedGap(text.substring(comment.span.end, declarationStart)))
      .filter(_.raw.startsWith("/*"))

  private def orderedComments(unit: CompilationUnit): IndexedSeq[Comment] =
    commentsByUnit.getOrElseUpdate(unit, unit.comments.sortBy(_.span.end).toIndexedSeq)

  private def patch(span: Span, replacement: String)(using context: Context): Unit =
    if !Rewrites.overlapsPatch(context.compilationUnit.source, span) then Rewrites.patch(span, replacement)
}
