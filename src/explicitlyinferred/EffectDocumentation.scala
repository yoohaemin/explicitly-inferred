package explicitlyinferred

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.*

import scala.collection.mutable

private[explicitlyinferred] final case class EffectDocumentation(errors: List[String], results: List[String]) {
  def managedLines(markers: Markers): List[String] =
    List(markers.startDelimiter, "Errors:") :::
      errors.map(value => s"  - $value") :::
      List("", "Returns:") :::
      results.map(value => s"  - $value") :::
      List(markers.endDelimiter)
}

private[explicitlyinferred] final class EffectDocumentationBuilder(config: EffectConfig) {
  private final case class CacheKey(tpe: Type, owner: Symbol)

  private val documentationCache = mutable.HashMap.empty[CacheKey, Option[EffectDocumentation]]
  private val parameterIndexCache = mutable.HashMap.empty[Symbol, Option[(Int, Int)]]

  def from(tpe: Type)(using context: Context): Option[EffectDocumentation] =
    documentationCache.getOrElseUpdate(CacheKey(tpe, context.owner), build(tpe))

  private def build(tpe: Type)(using Context): Option[EffectDocumentation] =
    effectTypeArguments(tpe).map { (errorType, resultType) =>
      val errorSettings = TypeRenderSettings(config.typeNameStyle, AliasPolicy.Dealias)
      val resultSettings = TypeRenderSettings(config.typeNameStyle, AliasPolicy.Preserve)

      val inferredErrors = TypeRenderer
        .unionEntries(errorType, errorSettings)
        .filterNot { rendered =>
          config.excludedErrorTypes.exists { pattern =>
            pattern.matcher(rendered.fullName).matches() || pattern.matcher(rendered.display).matches()
          }
        }
        .map(_.display)

      EffectDocumentation(
        nonEmpty((inferredErrors ++ config.additionalErrorTypes).distinct.sorted),
        nonEmpty(TypeRenderer.unionEntries(resultType, resultSettings).map(_.display).distinct.sorted)
      )
    }

  private def effectTypeArguments(tpe: Type)(using Context): Option[(Type, Type)] = {
    def extract(candidate: Type): Option[(Type, Type)] = candidate match
      case AppliedType(tycon, args) if config.effectTypeRegex.matcher(TypeRenderer.fullTypeName(tycon)).matches() =>
        parameterIndexes(tycon.typeSymbol).flatMap { (errorIndex, resultIndex) =>
          Option.when(errorIndex < args.size && resultIndex < args.size)(args(errorIndex) -> args(resultIndex))
        }
      case _ => None

    extract(tpe.widen.simplified.normalized)
      .orElse(extract(tpe.widenDealias.simplified.normalized.dealias))
  }

  private def parameterIndexes(symbol: Symbol)(using Context): Option[(Int, Int)] =
    parameterIndexCache.getOrElseUpdate(symbol, {
      val parameters = symbol.typeRef.typeParams
      val errorIndex = parameters.indexWhere(_.paramName.show == config.errorTypeParam)
      val resultIndex = parameters.indexWhere(_.paramName.show == config.resultTypeParam)
      Option.when(errorIndex >= 0 && resultIndex >= 0)(errorIndex -> resultIndex)
    })

  private def nonEmpty(values: List[String]): List[String] =
    if values.isEmpty then "Nothing" :: Nil else values
}
