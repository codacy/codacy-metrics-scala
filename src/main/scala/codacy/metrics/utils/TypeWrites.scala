package codacy.metrics.utils

import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.util.control.NonFatal

trait TypeWrites[U <: scala.reflect.api.Universe] {

  def toolbox: scala.tools.reflect.ToolBox[U]

  private lazy val tb = toolbox
  lazy val universe = tb.u

  import universe._

  //private lazy val freeOptionType = tb.u.typeOf[Option[_]]
  private type Symbol = tb.u.Symbol
  private implicit lazy val SymbolWrites = Writes((ts: Symbol) => Json.toJson(ts.fullName))

  private lazy val TypeSymbolWrites = (
    (__ \ "fullName").write[Symbol] and
      (__ \ "baseClasses").writeNullable[List[Symbol]] and
      (__ \ "typeParams").writeNullable[List[Symbol]] and
      (__ \ "paramLists").writeNullable[List[List[Symbol]]]
    )(
    (type_ : tb.u.Type) =>
      (type_.typeSymbol,
        Option(type_.baseClasses).filterNot(_.isEmpty),
        Option(type_.typeParams).filterNot(_.isEmpty),
        Option(type_.paramLists).filterNot(_.isEmpty)))

  lazy val TypeWrites =
    OWrites.nullable((__ \ "type"))(TypeSymbolWrites).contramap { (tree: Tree) =>
      val cast = tree.asInstanceOf[tb.u.Tree]
      //try typechecking it, this will fail on already checked trees
      val mTyped =
        try {

          Option(tb.typecheck(cast))
        } catch {
          case scala.tools.reflect.ToolBoxError(msg, _) if msg.contains("already typed") => Option(cast)
          case _: Throwable                                                          => Option.empty
        }

      mTyped.flatMap {
        case typedTree =>
          try {
            Option(typedTree.tpe)
          } catch {
            case NonFatal(_) => Option.empty
          }
      }

    }

}
