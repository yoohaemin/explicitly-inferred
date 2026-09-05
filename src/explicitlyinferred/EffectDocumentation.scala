package explicitlyinferred

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.*

private[explicitlyinferred] final case class EffectDocumentation(errors: List[String], results: List[String]) {
  def managedLines(markers: Markers): List[String] =
    List(markers.startDelimiter, "Errors:") :::
      errors.map(value => s"  - $value") :::
      List("", "Returns:") :::
      results.map(value => s"  - $value") :::
      List(markers.endDelimiter)
}

private[explicitlyinferred] object EffectDocumentation {
  def from(tpe: Type, config: EffectConfig)(using Context): Option[EffectDocumentation] =
    effectTypeArguments(tpe, config).map { (errorType, resultType) =>
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

  private def effectTypeArguments(tpe: Type, config: EffectConfig)(using Context): Option[(Type, Type)] = {
    def extract(candidate: Type): Option[(Type, Type)] = candidate match
      case AppliedType(tycon, args) if config.effectTypeRegex.matcher(TypeRenderer.fullTypeName(tycon)).matches() =>
        val arguments = tycon.typeParams.map(_.paramName.show).zip(args).toMap
        for
          errorType <- arguments.get(config.errorTypeParam)
          resultType <- arguments.get(config.resultTypeParam)
        yield (errorType, resultType)
      case _ => None

    extract(tpe.widen.simplified.normalized)
      .orElse(extract(tpe.widenDealias.simplified.normalized.dealias))
  }

  private def nonEmpty(values: List[String]): List[String] =
    if values.isEmpty then "Nothing" :: Nil else values
}
