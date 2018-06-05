package codacy.metrics.utils


import java.io.File

import codacy.foundation.api.FileContents
import codacy.other.Logger
import play.api.libs.json._

import scala.reflect.runtime.universe._
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.util.{Failure, Success, Try}

object ReflectiveParserOps {

  lazy val toolbox = runtimeMirror(getClass.getClassLoader).mkToolBox()
  //check if it start with package then packagename then optional ';' then body
  private lazy val tbParser = ReflectiveScalaParser(toolbox)

  def parsed(file: File): Either[String, JsValue] = {
    val contents = FileContents.getLines(file).map(_.mkString("\n")).getOrElse("")
    parsed(contents, Some(file.getCanonicalPath))
  }

  def parsed(sourceCode: String, filename: Option[String] = None): Either[String, JsValue] = {

    def packagesRemoved(tree: Tree): List[Tree] = {

      //if it is a package definition
      tree match {
        case PackageDef(_, ts) => ts.map(packagesRemoved).flatten
        case _                 => List(tree)
      }
    }

    //TODO: hack due to package bug
    treeFor(sourceCode).left.map {
      case message =>
        Logger.trace(filename.map("In file " + _ + ":").getOrElse(":") + message)
        message
    }.right.map {
      case t =>
        /*val subtrees = packagesRemoved(t)
      val r =
        subtrees.map { case tree =>
          try {
            toolbox.typecheck(tree)
          }
          catch {
            case NonFatal(e) => tree
          }
        }.map(_.asInstanceOf[tbParser.Tree])


      Writes.list(tbParser.DefaultTreeWrites).writes(r)
         */
        tbParser.DefaultTreeWrites.writes(t.asInstanceOf[tbParser.Tree])
    }

  }

  def treeFor(sourceCode: String): Either[String, Tree] = {
    Try(toolbox.parse(sourceCode)).recoverWith {
      case _: ToolBoxError =>
        Try(toolbox.parse(packagesProtected(sourceCode)))
    } match {
      case Success(tree)                     => Right(tree)
      case Failure(ToolBoxError(message, _)) => Left(message)
      case f: Failure[_]                     => Left(f.exception.getMessage)
    }
  }

  /*toolbox.parse hangs on files that have packages!
   * this still has a bug: you cannot have a string that contains package
   * eg val t = " package test "
   * */
  //@deprecated("remove when toolbox bug is cleaned", "")

  private def packagesProtected(in: String): String = {

    val packageStr = "package"

    val tokens = in.split(packageStr).map { case s => s -> 0 }

    tokens
      .reduceLeftOption[(String, Int)] {
      case ((prefix, numBrackets), (suffix, _)) =>
        if (isPartOfName(suffix) || isOpenComment(prefix) || isProtected(suffix) || isInString(prefix)) {
          s"$prefix$packageStr$suffix" -> numBrackets
        } else {
          //have to add bracket at the end of real name
          val whiteSpaces = suffix.takeWhile(Character.isWhitespace(_))
          val suffixNoWhitespaces = suffix.drop(whiteSpaces.length)
          val name = suffixNoWhitespaces.takeWhile { case char => !isBreaker(char) }
          val bracketedName = s"$name{"
          val suffixWithoutName = suffixNoWhitespaces.drop(name.length)
          val newSuffix = s"$bracketedName$suffixWithoutName"
          val newBrackets = numBrackets + 1
          s"$prefix$packageStr$whiteSpaces$newSuffix" -> newBrackets
        }
    }
      .map {
        case (folded, numClosingBrackets) =>
          val closingBracketsString = (1 to numClosingBrackets).map(_ => "}").mkString
          s"$folded$closingBracketsString"
      }
      .getOrElse(in)

  }

  private def isInString(pre: String) = {
    //if pre is opening
    // first simple strings aka \"
    val lastNewLinePos = pre.lastIndexOf(System.lineSeparator())
    val thisLine = pre.drop(lastNewLinePos + 1)
    //see number of " if it's even we are fine if it's odd we are inside a string
    val isInSimpleString = thisLine.filter(_ == '\"').size % 2 == 1
    //then check if it's inside a complex string aka "\"\"\""
    val trippleString = "\"\"\""

    val isInTrippleString = trippleString.r.findAllMatchIn(pre).length % 2 == 1

    isInSimpleString.||(isInTrippleString)
  }

  private def isOpenComment(pre: String) = {
    pre.lastIndexOf("/*") > pre.lastIndexOf("*/") || pre.lastIndexOf("//") > pre.lastIndexOf(System.lineSeparator())
  }

  private def isProtected(s: String) = {
    val trimmed = s.trim
    val name = trimmed.takeWhile(!isBreaker(_))
    val withoutName = trimmed.drop(name.length)

    val isPackageObject = name.trim == "object"
    lazy val hasOpenBracket: Boolean = withoutName.trim.startsWith("{")

    isPackageObject.||(hasOpenBracket)
  }

  private def isBreaker(char: Char) = {
    val isWhitespace: Boolean = Character.isWhitespace(char)
    lazy val isOpenBracket: Boolean = char == '{'
    lazy val isSemiColon: Boolean = char == ';'

    isWhitespace.||(isSemiColon).||(isOpenBracket)
  }

  private def isPartOfName(suffix: String): Boolean = suffix.headOption.map(!Character.isWhitespace(_)).getOrElse(false)

}

trait ReflectiveScalaParser {

  import play.api.libs.json._

  type Tree = universe.Tree
  val universe: scala.reflect.api.Universe

  def DefaultTreeWrites: OWrites[Tree]
}

object ReflectiveScalaParser {

  def apply(uni: scala.reflect.api.Universe): ReflectiveScalaParser =
    new ReflectiveScalaParser with RecursiveTreeWrites with OtherWrites {
      lazy val universe = uni
      lazy val DefaultTreeWrites = treeWrites(LogicalTypeWrites, StartLineWrites)
    }

  def apply[U <: scala.reflect.api.Universe](tb: scala.tools.reflect.ToolBox[U]): ReflectiveScalaParser =
    new ReflectiveScalaParser with RecursiveTreeWrites with OtherWrites with TypeWrites[U] {
      lazy val toolbox = tb
      lazy val DefaultTreeWrites = treeWrites( /*TypeWrites,*/ LogicalTypeWrites, StartLineWrites)
    }

}
