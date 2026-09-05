package explicitlyinferred

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.*

import scala.collection.mutable

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
    dedupeSortedEntries(
      flattenUnion(tpe, settings.aliasPolicy)
        .map(part => (toNode(part, settings), fullTypeName(part)))
        .sortBy(_._1.sortKey)
    )
      .map { (node, fullName) => RenderedType(render(node), node.sortKey, fullName) }

  def fullTypeName(tpe: Type)(using Context): String = {
    val normalized = normalize(tpe, AliasPolicy.Dealias)
    val symbol = normalized.typeSymbol
    if symbol.exists then symbol.fullName.show else normalized.show
  }

  def isNothing(tpe: Type)(using context: Context): Boolean =
    normalize(tpe, AliasPolicy.Dealias).typeSymbol == context.definitions.NothingClass

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
    flatten(tpe, aliasPolicy) {
      case OrType(left, right) => Some(left -> right)
      case _ => None
    }

  private def flattenIntersection(tpe: Type, aliasPolicy: AliasPolicy)(using Context): List[Type] =
    flatten(tpe, aliasPolicy) {
      case AndType(left, right) => Some(left -> right)
      case _ => None
    }

  private def flatten(
      tpe: Type,
      aliasPolicy: AliasPolicy
  )(split: Type => Option[(Type, Type)])(using Context): List[Type] = {
    val pending = mutable.ArrayDeque(tpe)
    val result = mutable.ListBuffer.empty[Type]
    while pending.nonEmpty do
      val current = normalize(pending.removeHead(), aliasPolicy)
      split(current) match
        case Some((left, right)) =>
          pending.prepend(right)
          pending.prepend(left)
        case None => result += current
    result.toList
  }

  private def isNamedTuple(tycon: Type, args: List[Type])(using Context): Boolean =
    args.size == 2 &&
      tycon.typeSymbol.name.show == "NamedTuple" &&
      tycon.typeSymbol.owner.name.show == "NamedTuple"

  private def isOrdinaryTuple(tycon: Type, args: List[Type])(using Context): Boolean =
    args.size >= 2 && isTupleConstructor(tycon)

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
      case AppliedType(tycon, args) if isTupleConstructor(tycon) => Some(args)
      case _ => None

  private def isTupleConstructor(tpe: Type)(using Context): Boolean = {
    val prefix = "scala.Tuple"
    val name = fullTypeName(tpe)
    val arity = name.drop(prefix.length)
    name.startsWith(prefix) && arity.nonEmpty && arity.forall(_.isDigit)
  }

  private def isCompilerInternalAlias(tpe: Type)(using Context): Boolean = {
    val symbol = tpe.typeSymbol
    symbol.exists && symbol.owner.name.show == "Signature"
  }

  private def dedupe(nodes: List[TypeNode]): List[TypeNode] = {
    val result = mutable.ListBuffer.empty[TypeNode]
    nodes.foreach { node =>
      if result.lastOption.forall(_.sortKey != node.sortKey) then result += node
    }
    result.toList
  }

  private def dedupeSortedEntries(entries: List[(TypeNode, String)]): List[(TypeNode, String)] = {
    val result = mutable.ListBuffer.empty[(TypeNode, String)]
    entries.foreach { entry =>
      if result.lastOption.forall(_._1.sortKey != entry._1.sortKey) then result += entry
    }
    result.toList
  }

  private def displayName(tpe: Type, settings: TypeRenderSettings)(using Context): String =
    normalize(tpe, settings.aliasPolicy) match
      case ref: TypeRef if ref.symbol.exists =>
        concreteOpaqueOwner(ref, settings.aliasPolicy).fold(displaySymbol(ref.symbol, settings.nameStyle)) { owner =>
          displaySymbol(owner, settings.nameStyle)
        }
      case ref: TermRef if ref.symbol.exists => displaySymbol(ref.symbol, settings.nameStyle)
      case thisType: ThisType => displaySymbol(thisType.tref.symbol, settings.nameStyle)
      case constant: ConstantType => constant.show
      case bounds: TypeBounds => bounds.show
      case other => other.show

  private def concreteOpaqueOwner(ref: TypeRef, aliasPolicy: AliasPolicy)(using Context): Option[Symbol] = {
    val prefixSymbol = ref.prefix.termSymbol
    Option.when(
      aliasPolicy == AliasPolicy.Preserve &&
        ref.symbol.is(Flags.Opaque) &&
        ref.symbol.name.show == "Type" &&
        prefixSymbol.exists &&
        prefixSymbol != ref.symbol.owner
    )(prefixSymbol)
  }

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
    normalized match
      case ref: TypeRef =>
        concreteOpaqueOwner(ref, aliasPolicy) match
          case Some(owner) => s"${owner.coord}:${owner.fullName.show}"
          case None => symbolKey(normalized, display)
      case _ => symbolKey(normalized, display)
  }

  private def symbolKey(tpe: Type, display: String)(using Context): String = {
    val symbol = tpe.typeSymbol
    if symbol.exists then s"${symbol.coord}:${symbol.owner.fullName.show}.${symbol.name.show}"
    else s"$display#${tpe.show}"
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
