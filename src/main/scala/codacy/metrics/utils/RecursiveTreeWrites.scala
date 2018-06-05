package codacy.metrics.utils


import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.language.postfixOps

trait RecursiveTreeWrites extends BuildingBlockWrites {
  self: AnyRef { val universe: scala.reflect.api.Universe } =>

  import universe._

  def treeWrites(flavours: OWrites[Tree]*): OWrites[Tree] = new OWrites[Tree] {
    self: OWrites[Tree] =>

    //this is to be able to use self as owrite for lists of trees
    implicit lazy val selfW = self

    //TODO: note to self
    // - is it good practise to have the all the writes implicitly just to save lines?
    // wouldn't it be better to have: writeNullable(Writes.seq(self)) instead of writeNullable[List[Tree]] and
    // relying on implicit self value
    lazy val DefaultWrites: OWrites[Tree] =
    (__ \ "children").writeNullable[List[Tree]].contramap((t: Tree) => Option(t.children).filterNot(_.isEmpty))

    implicit lazy val CaseDefWrites = caseDefWrites
    implicit lazy val ModifiersWrites = modifiersWrites
    lazy val ValDefWrites = valDefWrites
    lazy val BlockWrites = blockWrites
    lazy val IfWrites = ifWrites
    lazy val SelectWrites = selectWrites
    lazy val NewWrites = newWrites
    lazy val ThrowWrites = throwWrites
    lazy val ApplyWrites = applyWrites
    lazy val TryWrites = tryWrites
    lazy val TypedWrites = typedWrites
    lazy val BindWrites = bindWrites
    lazy val TypeApplyWrites = typeApplyWrites
    lazy val DefDefWrites = defdefWrites
    lazy val PackageDefWrites = packageDefWrites
    lazy val ClassDefWrites = classDefWrites
    lazy val TemplateWrites = templateWrites
    lazy val SuperWrites = superWrites
    lazy val AppliedTypeTreeWrites = appliedTypeTreeWrites
    lazy val FunctionWrites = functionWrites
    lazy val ImportWrites = importWrites
    lazy val ModuleDefWrites = moduleDefWrites
    lazy val CompoundTypeTreeWrites = compoundTypeTreeWrites(TemplateWrites)
    lazy val TypeBoundsTreeWrites = typeBoundsTreeWrites
    lazy val TypeDefWrites = typeDefWrites
    lazy val MatchWrites = matchWrites

    //build me a general tree writes from buildingBlockWrites
    lazy val BuildingBlockWrites: OWrites[Tree] = OWrites((_: Tree) match {
      case valDef: ValDef             => ValDefWrites.writes(valDef)
      case block: Block               => BlockWrites.writes(block)
      case if_ : If                   => IfWrites.writes(if_)
      case select: Select             => SelectWrites.writes(select)
      case new_ : New                 => NewWrites.writes(new_)
      case case_ : CaseDef            => CaseDefWrites.writes(case_)
      case throw_ : Throw             => ThrowWrites.writes(throw_)
      case apply: Apply               => ApplyWrites.writes(apply)
      case try_ : Try                 => TryWrites.writes(try_)
      case ident: Ident               => IdentWrites.writes(ident)
      case typed: Typed               => TypedWrites.writes(typed)
      case bind: Bind                 => BindWrites.writes(bind)
      case tApply: TypeApply          => TypeApplyWrites.writes(tApply)
      case defdef: DefDef             => DefDefWrites.writes(defdef)
      case pDef: PackageDef           => PackageDefWrites.writes(pDef)
      case cDef: ClassDef             => ClassDefWrites.writes(cDef)
      case templ: Template            => TemplateWrites.writes(templ)
      case super_ : Super             => SuperWrites.writes(super_)
      case att: AppliedTypeTree       => AppliedTypeTreeWrites.writes(att)
      case func: Function             => FunctionWrites.writes(func)
      case import_ : Import           => ImportWrites.writes(import_)
      case moduleDef: ModuleDef       => ModuleDefWrites.writes(moduleDef)
      case compTree: CompoundTypeTree => CompoundTypeTreeWrites.writes(compTree)
      case typBoTre: TypeBoundsTree   => TypeBoundsTreeWrites.writes(typBoTre)
      case typeDef: TypeDef           => TypeDefWrites.writes(typeDef)
      case match_ : Match             => MatchWrites.writes(match_)
      case undefined                  => DefaultWrites.writes(undefined)
    })

    lazy val AllWrites = flavours.fold(BuildingBlockWrites)(_ and _ join)

    //just a forwarder
    def writes(tree: Tree) = AllWrites.writes(tree)
  }

}
