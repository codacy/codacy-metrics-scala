package codacy.metrics.utils


import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.reflect.internal.util._
import scala.tools.reflect.ToolBox

trait OtherWrites[U <: scala.reflect.api.Universe] {
  val toolbox: ToolBox[U]

  import toolbox.u._

  lazy val LogicalTypeWrites = (
    (__ \ "logicalType").writeNullable[String].contramap((_: Tree).getClass.toString.split('$').lastOption)
    )

  lazy val StartLineWrites = (
    (__ \ "line")
      .writeNullable[Int]
      .contramap((_: Tree).pos match {
        case _: UndefinedPosition => Option.empty
        //case p:FakePos    => Option.empty
        case pos => Option(pos.line)
      })
    )

}
