package explicitlyinferred

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.*

import scala.collection.mutable

private[explicitlyinferred] final case class DocumentationSection(heading: String, entries: List[String])

private[explicitlyinferred] final case class TypeDocumentation(sections: List[DocumentationSection]) {
  def managedLines(markers: Markers): List[String] = {
    val body = sections.zipWithIndex.flatMap { (section, index) =>
      val lines = (section.heading + ":") :: section.entries.map(value => s"  - $value")
      if index == sections.size - 1 then lines else lines :+ ""
    }
    markers.startDelimiter :: body ::: markers.endDelimiter :: Nil
  }
}

private[explicitlyinferred] final class TypeDocumentationBuilder(config: DocumentationConfig) {
  private final case class CacheKey(tpe: Type, owner: Symbol)

  private val documentationCache = mutable.HashMap.empty[CacheKey, Option[TypeDocumentation]]
  private val parameterIndexCache = mutable.HashMap.empty[Symbol, Option[IndexedSeq[Int]]]

  def from(tpe: Type)(using context: Context): Option[TypeDocumentation] =
    documentationCache.getOrElseUpdate(CacheKey(tpe, context.owner), build(tpe))

  private def build(tpe: Type)(using Context): Option[TypeDocumentation] =
    typeArguments(tpe).map { arguments =>
      val sections = config.parameters.zip(arguments).map { (parameter, argument) =>
        val settings = TypeRenderSettings(config.typeNameStyle, parameter.aliasPolicy)
        val inferred =
          if TypeRenderer.isNothing(argument) then Nil
          else
            TypeRenderer
              .unionEntries(argument, settings)
              .filterNot { rendered =>
                parameter.excludedTypes.exists { pattern =>
                  pattern.matcher(rendered.fullName).matches() || pattern.matcher(rendered.display).matches()
                }
              }
              .map(_.display)
        DocumentationSection(
          parameter.heading,
          nonEmpty((inferred ++ parameter.additionalTypes).distinct.sorted)
        )
      }
      TypeDocumentation(sections)
    }

  private def typeArguments(tpe: Type)(using Context): Option[List[Type]] = {
    def extract(candidate: Type): Option[List[Type]] = candidate match
      case AppliedType(tycon, arguments) if matchesConstructor(tycon) =>
        parameterIndexes(tycon.typeSymbol).flatMap { indexes =>
          Option.when(indexes.forall(_ < arguments.size))(indexes.map(arguments).toList)
        }
      case _ => None

    extract(tpe.widen.simplified.normalized)
      .orElse(extract(tpe.widenDealias.simplified.normalized.dealias))
  }

  private def matchesConstructor(tycon: Type)(using Context): Boolean =
    config.typeRegex.forall(_.matcher(TypeRenderer.fullTypeName(tycon)).matches())

  private def parameterIndexes(symbol: Symbol)(using Context): Option[IndexedSeq[Int]] =
    if !symbol.exists then None
    else
      parameterIndexCache.getOrElseUpdate(symbol, {
        val parameters = symbol.typeRef.typeParams
        val indexes = config.parameters.map(configured => parameters.indexWhere(_.paramName.show == configured.name))
        Option.when(indexes.forall(_ >= 0))(indexes.toIndexedSeq)
      })

  private def nonEmpty(values: List[String]): List[String] =
    if values.isEmpty then "Nothing" :: Nil else values
}
