package codacy.metrics.utils


import scala.reflect.runtime.universe._
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.util.{Failure, Success, Try}

object ReflectiveParserOps {

  lazy val toolbox = runtimeMirror(getClass.getClassLoader).mkToolBox()
  //check if it start with package then packagename then optional ';' then body
  lazy val tbParser = new ReflectiveScalaParser(toolbox)

  def treeFor(sourceCode: String) =
    tbParser.treeFor(sourceCode)

}

class ReflectiveScalaParser[U <: scala.reflect.api.Universe](val toolbox: scala.tools.reflect.ToolBox[U]) {

  def treeFor(sourceCode: String): Either[String, toolbox.u.Tree] = {
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
    val isInSimpleString = thisLine.filter(_ == '\"').size % 2 != 0
    //then check if it's inside a complex string aka "\"\"\""
    val trippleString = "\"\"\""

    val isInTrippleString = trippleString.r.findAllMatchIn(pre).length % 2 != 0

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

    isWhitespace || isSemiColon || isOpenBracket
  }

  private def isPartOfName(suffix: String): Boolean = suffix.headOption.map(!Character.isWhitespace(_)).getOrElse(false)
}
