package explicitlyinferred

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.*

private[explicitlyinferred] enum AliasPolicy {
  case Dealias, Preserve
}

private[explicitlyinferred] final case class TypeRenderSettings(
    nameStyle: TypeNameStyle,
    aliasPolicy: AliasPolicy
)

private[explicitlyinferred] final case class RenderedType(display: String, sortKey: String, fullName: String)

private[explicitlyinferred] object TypeRenderer {
  private sealed trait TypeNode {
    def sortKey: String
    def precedence: Int
  }

  private final case class Leaf(display: String, sortKey: String) extends TypeNode {
    val precedence = 4
  }

  private final case class Applied(tycon: TypeNode, args: List[TypeNode], sortKey: String) extends TypeNode {
    val precedence = 4
  }

  private final case class Union(parts: List[TypeNode], sortKey: String) extends TypeNode {
    val precedence = 1
  }

  private final case class Intersection(parts: List[TypeNode], sortKey: String) extends TypeNode {
    val precedence = 2
  }

  private final case class TupleValue(parts: List[TypeNode], sortKey: String) extends TypeNode {
    val precedence = 4
  }

  private final case class NamedTupleValue(fields: List[(String, TypeNode)], sortKey: String) extends TypeNode {
    val precedence = 4
  }

  def unionEntries(tpe: Type, settings: TypeRenderSettings)(using Context): List[RenderedType] =
    flattenUnion(tpe, settings.aliasPolicy)
      .map(part => (toNode(part, settings), fullTypeName(part)))
      .sortBy(_._1.sortKey)
      .foldLeft(List.empty[(TypeNode, String)]) { (entries, entry) =>
        if entries.lastOption.exists(_._1.sortKey == entry._1.sortKey) then entries else entries :+ entry
      }
      .map { (node, fullName) => RenderedType(render(node), node.sortKey, fullName) }

  def fullTypeName(tpe: Type)(using Context): String = {
    val normalized = normalize(tpe, AliasPolicy.Dealias)
    val symbol = normalized.typeSymbol
    if symbol.exists then symbol.fullName.show else normalized.show
  }

  private def toNode(tpe: Type, settings: TypeRenderSettings)(using Context): TypeNode = {
    val normalized = normalize(tpe, settings.aliasPolicy)
    if settings.aliasPolicy == AliasPolicy.Preserve && isCompilerInternalAlias(normalized) then
      toNode(normalize(tpe, AliasPolicy.Dealias), settings)
    else normalized match
      case union: OrType =>
        val parts = dedupe(flattenUnion(union, settings.aliasPolicy).map(toNode(_, settings)).sortBy(_.sortKey))
        if parts.size == 1 then parts.head
        else Union(parts, parts.map(_.sortKey).mkString("or(", ",", ")"))
      case intersection: AndType =>
        val parts = dedupe(flattenIntersection(intersection, settings.aliasPolicy).map(toNode(_, settings)).sortBy(_.sortKey))
        if parts.size == 1 then parts.head
        else Intersection(parts, parts.map(_.sortKey).mkString("and(", ",", ")"))
      case AppliedType(tycon, args) if isNamedTuple(tycon, args) =>
        namedTuple(args, settings).getOrElse(leaf(normalized, settings))
      case AppliedType(tycon, args) if isOrdinaryTuple(tycon, args) =>
        val parts = args.map(toNode(_, settings))
        TupleValue(parts, parts.map(_.sortKey).mkString("tuple(", ",", ")"))
      case AppliedType(tycon, args) =>
        val renderedTycon = toNode(tycon, settings)
        val renderedArgs = args.map(toNode(_, settings))
        Applied(renderedTycon, renderedArgs, s"${renderedTycon.sortKey}[${renderedArgs.map(_.sortKey).mkString(",")}]")
      case _ =>
        leaf(normalized, settings)
  }

  private def leaf(tpe: Type, settings: TypeRenderSettings)(using Context): TypeNode = {
    val display = displayName(tpe, settings)
    Leaf(display, stableKey(tpe, display, settings.aliasPolicy))
  }

  private def normalize(tpe: Type, aliasPolicy: AliasPolicy)(using Context): Type =
    aliasPolicy match
      case AliasPolicy.Dealias => tpe.widenDealias.simplified.normalized.dealias
      case AliasPolicy.Preserve => tpe.widen.simplified.normalized

  private def flattenUnion(tpe: Type, aliasPolicy: AliasPolicy)(using Context): List[Type] =
    normalize(tpe, aliasPolicy) match
      case OrType(left, right) => flattenUnion(left, aliasPolicy) ::: flattenUnion(right, aliasPolicy)
      case other => other :: Nil

  private def flattenIntersection(tpe: Type, aliasPolicy: AliasPolicy)(using Context): List[Type] =
    normalize(tpe, aliasPolicy) match
      case AndType(left, right) => flattenIntersection(left, aliasPolicy) ::: flattenIntersection(right, aliasPolicy)
      case other => other :: Nil

  private def isNamedTuple(tycon: Type, args: List[Type])(using Context): Boolean =
    args.size == 2 &&
      tycon.typeSymbol.name.show == "NamedTuple" &&
      tycon.typeSymbol.owner.name.show == "NamedTuple"

  private def isOrdinaryTuple(tycon: Type, args: List[Type])(using Context): Boolean =
    args.size >= 2 && fullTypeName(tycon).matches("scala\\.Tuple[0-9]+")

  private def namedTuple(args: List[Type], settings: TypeRenderSettings)(using Context): Option[TypeNode] =
    for
      names <- tupleElements(args.head, settings.aliasPolicy)
      values <- tupleElements(args(1), settings.aliasPolicy)
      if names.size == values.size && names.nonEmpty
    yield
      val fields = names.zip(values).map { (nameType, valueType) =>
        val rawName = nameType.dealias.show
        val firstQuote = rawName.indexOf('"')
        val secondQuote = if firstQuote >= 0 then rawName.indexOf('"', firstQuote + 1) else -1
        val name =
          if firstQuote >= 0 && secondQuote > firstQuote then rawName.substring(firstQuote + 1, secondQuote)
          else rawName
        name -> toNode(valueType, settings)
      }
      NamedTupleValue(fields, fields.map { (name, value) => s"$name:${value.sortKey}" }.mkString("namedTuple(", ",", ")"))

  private def tupleElements(tpe: Type, aliasPolicy: AliasPolicy)(using Context): Option[List[Type]] =
    normalize(tpe, aliasPolicy) match
      case AppliedType(tycon, args) if fullTypeName(tycon).matches("scala\\.Tuple[0-9]+") => Some(args)
      case _ => None

  private def isCompilerInternalAlias(tpe: Type)(using Context): Boolean = {
    val symbol = tpe.typeSymbol
    symbol.exists && symbol.owner.name.show == "Signature"
  }

  private def dedupe(nodes: List[TypeNode]): List[TypeNode] =
    nodes.foldLeft(List.empty[TypeNode]) { (result, node) =>
      if result.lastOption.exists(_.sortKey == node.sortKey) then result else result :+ node
    }

  private def displayName(tpe: Type, settings: TypeRenderSettings)(using Context): String =
    normalize(tpe, settings.aliasPolicy) match
      case ref: TypeRef if ref.symbol.exists => displaySymbol(ref.symbol, settings.nameStyle)
      case ref: TermRef if ref.symbol.exists => displaySymbol(ref.symbol, settings.nameStyle)
      case thisType: ThisType => displaySymbol(thisType.tref.symbol, settings.nameStyle)
      case constant: ConstantType => constant.show
      case bounds: TypeBounds => bounds.show
      case other => other.show

  private def displaySymbol(symbol: Symbol, style: TypeNameStyle)(using Context): String =
    style match
      case TypeNameStyle.Simple => symbol.name.show
      case TypeNameStyle.Full => symbol.fullName.show
      case TypeNameStyle.Owner =>
        val owner = symbol.owner
        if symbol.fullName.show.startsWith("scala.") then symbol.name.show
        else if owner.exists && !owner.is(Flags.Package) then s"${owner.name.show}.${symbol.name.show}"
        else symbol.name.show

  private def stableKey(tpe: Type, display: String, aliasPolicy: AliasPolicy)(using Context): String = {
    val normalized = normalize(tpe, aliasPolicy)
    val symbol = normalized.typeSymbol
    if symbol.exists then s"${symbol.coord}:${symbol.owner.fullName.show}.${symbol.name.show}"
    else s"$display#${normalized.show}"
  }

  private def render(node: TypeNode, parentPrecedence: Int = 0): String = {
    val value = node match
      case Leaf(display, _) => display
      case Applied(tycon, args, _) => s"${render(tycon, node.precedence)}[${args.map(render(_)).mkString(", ")}]"
      case Union(parts, _) => parts.map(render(_, node.precedence)).mkString(" | ")
      case Intersection(parts, _) => parts.map(render(_, node.precedence)).mkString(" & ")
      case TupleValue(parts, _) => parts.map(render(_)).mkString("(", ", ", ")")
      case NamedTupleValue(fields, _) => fields.map { (name, fieldType) => s"$name: ${render(fieldType)}" }.mkString("(", ", ", ")")
    if node.precedence < parentPrecedence then s"($value)" else value
  }
}
