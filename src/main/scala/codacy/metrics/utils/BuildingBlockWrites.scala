package codacy.metrics.utils


import play.api.libs.functional.syntax._
import play.api.libs.json.Writes._
import play.api.libs.json._

import scala.tools.reflect.ToolBox

trait BuildingBlockWrites[U <: scala.reflect.api.Universe] {

  val toolbox: ToolBox[U]

  import toolbox.u._
  import Flag._

  implicit lazy val nameWrites = Writes((n: Name) => Json.toJson(n.decodedName.toString))
  private lazy val allFlags = Map(TRAIT -> "TRAIT",
    INTERFACE -> "INTERFACE",
    MUTABLE -> "MUTABLE",
    MACRO -> "MACRO",
    DEFERRED -> "DEFERRED",
    ABSTRACT -> "ABSTRACT",
    FINAL -> "FINAL",
    SEALED -> "SEALED",
    IMPLICIT -> "IMPLICIT",
    LAZY -> "LAZY",
    OVERRIDE -> "OVERRIDE",
    PRIVATE -> "PRIVATE",
    PROTECTED -> "PROTECTED",
    LOCAL -> "LOCAL",
    CASE -> "CASE",
    ABSOVERRIDE -> "ABSOVERRIDE",
    BYNAMEPARAM -> "BYNAMEPARAM",
    PARAM -> "PARAM",
    COVARIANT -> "COVARIANT",
    CONTRAVARIANT -> "CONTRAVARIANT",
    DEFAULTPARAM -> "DEFAULTPARAM",
    PRESUPER -> "PRESUPER",
    DEFAULTINIT -> "DEFAULTINIT").mapValues(_.toLowerCase)

  def modifiersWrites(implicit parent: OWrites[Tree]): OWrites[Modifiers] =
    (
      (__ \ "flags").write[List[String]] and
        (__ \ "privateWithin").write[Name] and
        (__ \ "annotations").writeNullable[List[Tree]]
      )((_: Modifiers) match {
      case mods @ Modifiers(_, name, annotations) =>
        (flagSetsExtracted(mods), name, Option(annotations).filterNot(_.isEmpty))
    })

  def flagSetsExtracted(mods: Modifiers): List[String] =
    allFlags.collect { case (flag, str) if mods.hasFlag(flag) => str }.toList

  //this should be done automatically via Json.format[ValDef] -> doesn't work because ValDef and others aren't case classes
  def valDefWrites(implicit parent: OWrites[Tree], mods: OWrites[Modifiers]) =
    (
      (__ \ "mods").write[Modifiers] and
        (__ \ "name").write[Name] and
        (__ \ "tpt").write[Tree] and
        (__ \ "rhs").write[Tree]
      )(unlift(ValDef.unapply))

  def selectWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "qual").write[Tree] and
        (__ \ "name").write[Name]
      )(unlift(Select.unapply))

  def ifWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "cond").write[Tree] and
        (__ \ "thenp").write[Tree] and
        (__ \ "elesp").write[Tree]
      )(unlift(If.unapply))

  def blockWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "stats").write[List[Tree]] and
        (__ \ "expr").write[Tree]
      )(unlift(Block.unapply))

  def newWrites(implicit parent: OWrites[Tree]) =
    (__ \ "tpt").write[Tree].contramap(unlift(New.unapply))

  def caseDefWrites(implicit parent: OWrites[Tree]): OWrites[CaseDef] =
    (
      (__ \ "pat").write[Tree] and
        (__ \ "guard").write[Tree] and
        (__ \ "body").write[Tree]
      )(unlift(CaseDef.unapply))

  def throwWrites(implicit parent: OWrites[Tree]) =
    (__ \ "expr").write[Tree].contramap(unlift(Throw.unapply))

  def tryWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "block").write[Tree] and
        (__ \ "catches").write[List[CaseDef]] and
        (__ \ "finalizer").write[Tree]
      )(unlift(Try.unapply))

  def applyWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "fun").write[Tree] and
        (__ \ "args").write[List[Tree]]
      )(unlift(Apply.unapply))

  def typedWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "expr").write[Tree] and
        (__ \ "tpt").write[Tree]
      )(unlift(Typed.unapply))

  def bindWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "name").write[Name] and
        (__ \ "body").write[Tree]
      )(unlift(Bind.unapply))

  def IdentWrites(implicit nameWrites: Writes[Name] = nameWrites) =
    (__ \ "name").write[Name].contramap(unlift(Ident.unapply))

  def typeApplyWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "fun").write[Tree] and
        (__ \ "args").write[List[Tree]]
      )(unlift(TypeApply.unapply))

  def typeDefWrites(implicit parent: OWrites[Tree], mods: OWrites[Modifiers]): OWrites[TypeDef] =
    (
      (__ \ "mods").write[Modifiers] and
        (__ \ "name").write[TypeName] and
        (__ \ "tparams").lazyWrite[List[TypeDef]](traversableWrites(typeDefWrites)) and
        (__ \ "rhs").write[Tree]
      )(unlift(TypeDef.unapply))

  def defdefWrites(implicit parent: OWrites[Tree], mods: OWrites[Modifiers]): OWrites[DefDef] =
    (
      (__ \ "mods").write[Modifiers] and
        (__ \ "name").write[TermName] and
        (__ \ "tparams").write[List[TypeDef]] and
        (__ \ "vparamss").write[List[List[ValDef]]] and
        (__ \ "tpt").write[Tree] and
        (__ \ "rhs").write[Tree]
      )(unlift(DefDef.unapply))

  def packageDefWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "pid").write[RefTree] and
        (__ \ "stats").write[List[Tree]]
      )(unlift(PackageDef.unapply))

  def moduleDefWrites(implicit parent: OWrites[Tree], mods: OWrites[Modifiers]) =
    (
      (__ \ "mods").write[Modifiers] and
        (__ \ "name").write[TermName] and
        (__ \ "impl").write[Template]
      )(unlift(ModuleDef.unapply))

  def classDefWrites(implicit parent: OWrites[Tree], mods: OWrites[Modifiers]) =
    (
      (__ \ "mods").write[Modifiers] and
        (__ \ "name").write[TypeName] and
        (__ \ "tparams").write[List[TypeDef]] and
        (__ \ "impl").write[Template]
      )(unlift(ClassDef.unapply))

  def templateWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "parents").write[List[Tree]] and
        (__ \ "self").write[ValDef] and
        (__ \ "body").write[List[Tree]]
      )(unlift(Template.unapply))

  def superWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "qual").write[Tree] and
        (__ \ "mix").write[TypeName]
      )(unlift(Super.unapply))

  def appliedTypeTreeWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "tpt").write[Tree] and
        (__ \ "args").write[List[Tree]]
      )(unlift(AppliedTypeTree.unapply))

  def functionWrites(implicit parent: OWrites[Tree]) =
    (
      (__ \ "vparams").write[List[ValDef]] and
        (__ \ "body").write[Tree]
      )(unlift(Function.unapply))

  def importSelectorWrites(implicit nameWrites: Writes[Name] = nameWrites): OWrites[ImportSelector] =
    (
      (__ \ "name").write[Name] and
        (__ \ "namePos").write[Int] and
        (__ \ "rename").writeNullable[Name] and
        (__ \ "renamePos").writeNullable[Int]
      )(unlift(ImportSelector.unapply) andThen {
      case (name, namePos, rename, renamePos) =>
        (name, namePos, Option(rename), Option(renamePos).filterNot(_ == -1))
    })

  def importWrites(implicit parent: OWrites[Tree],
                   importSelectorWriter: OWrites[ImportSelector] = importSelectorWrites()): OWrites[Import] =
    (
      (__ \ "expr").write[Tree] and
        (__ \ "selectors").write[List[ImportSelector]]
      )(unlift(Import.unapply))

  def compoundTypeTreeWrites(implicit templateWrites: OWrites[Template]): OWrites[CompoundTypeTree] =
    (__ \ "templ").write[Template].contramap(unlift(CompoundTypeTree.unapply))

  def typeBoundsTreeWrites(implicit parent: OWrites[Tree]): OWrites[TypeBoundsTree] =
    (
      (__ \ "lo").write[Tree] and
        (__ \ "hi").write[Tree]
      )(unlift(TypeBoundsTree.unapply))

  def matchWrites(implicit parent: OWrites[Tree]): OWrites[Match] =
    (
      (__ \ "selector").write[Tree] and
        (__ \ "cases").write[List[CaseDef]]
      )(unlift(Match.unapply))

}
