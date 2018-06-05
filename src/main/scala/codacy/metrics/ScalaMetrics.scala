package codacy.metrics

import java.nio.file.Path

import codacy.docker.api.{MetricsConfiguration, Source}
import codacy.docker.api.metrics.{FileMetrics, MetricsTool}
import com.codacy.api.dtos.Language
import utils.ReflectiveParserOps._
import utils.Tools._

import scala.util.Try

final case class Declaration(name: String, line: Int)

object ScalaMetrics extends MetricsTool {
  override def apply(source: Source.Directory,
                     language: Option[Language],
                     files: Option[Set[Source.File]],
                     options: Map[MetricsConfiguration.Key, MetricsConfiguration.Value]): Try[List[FileMetrics]] = {

    Try {
      val filesSeq: List[(Source.File, java.io.File)] = files.getOrElse(allFilesIn(new java.io.File(source.path).toPath)).map(srcFile => (srcFile, new java.io.File(source.path + "/" + srcFile.path)))(collection.breakOut)
      val metricsFn = metricsFunctions(filesSeq.map(_._2))

      filesSeq.map {
        case (srcFile, ioFile) =>
          FileMetrics(
            filename = srcFile.path,
            nrClasses = metricsFn.classes(ioFile).right.toOption.map(_.size),
            nrMethods = metricsFn.methods(ioFile).right.toOption.map(_.size)
          )
      }
    }
  }

  private def allFilesIn(srcPath: Path, relativize: Boolean = true): Set[Source.File] = {
    val betterSrc = better.files.File(srcPath)
    betterSrc.children.flatMap {
      case dir if dir.isDirectory =>
        if(relativize)
          allFilesIn(dir.path, false).map(srcFile => Source.File(betterSrc.relativize(better.files.File(srcFile.path)).toString))
        else
          allFilesIn(dir.path)
      case file => Set(Source.File(file.pathAsString))
    }.to[Set]
  }
  
  import toolbox.u._

  private def isTrait(ast: Tree): Boolean = ast match {
    case ClassDef(mods, _, _, _) => mods.hasFlag(Flag.TRAIT)
    case _                       => false
  }

  private def traitsIn(ast: Tree): List[Tree] =
    ast.children
      .map(traitsIn)
      .:+(if (isTrait(ast)) {
        List(ast)
      } else {
        List.empty
      })
      .flatten

  private def isImplicit(ast: Tree): Boolean =
    (ast match {
      case c: ClassDef => Option(c.mods)
      case c: DefDef   => Option(c.mods)
      case c: ValDef   => Option(c.mods)
      //case v:AnyRef{ def mods:Modifiers } => v.mods.hasFlag(Flag.IMPLICIT)
      //case mods:Modifiers => mods.hasFlag(Flag.IMPLICIT)
      case _ => Option.empty
    }).exists(_.hasFlag(Flag.IMPLICIT))

  private def implicitsAndTraitsIn(ast: Tree): List[Tree] =
    ast.children
      .map(traitsIn)
      .:+(if (isImplicit(ast) || isTrait(ast)) {
        List(ast)
      } else {
        List.empty
      })
      .flatten

  private def astForFileContent(content: String): Either[String, Tree] =
    treeFor(content)

  private def classesForAst(ast: Tree, pNames: List[Name] = List.empty): List[Declaration] = {

    val names = ast match {
      case name: NameTreeApi => pNames :+ name.name
      case _                 => pNames
    }

    ast.children.map(classesForAst(_, names)).flatten ++
      (ast match {
        case _ : ClassDef =>
          List(Declaration(
            name = names.map(_.decodedName.toString).reduce(_ + "." + _),
            line = ast.pos.line
      ))
        case _ => List.empty[Declaration]
      })
  }

  private def methodsForAst(ast: Tree, pNames: List[Name] = List.empty): List[Declaration] = {

    val names = ast match {
      case name: NameTreeApi => pNames :+ name.name
      case _                 => pNames
    }

    ast.children.map(methodsForAst(_, names)).flatten ++
      (ast match {
        case _ : DefDef =>
          val obj = Declaration(
            name = names.map(_.decodedName.toString).reduce(_ + "." + _),
            line = ast.pos.line
          )
          List(obj)
        case _ => List.empty[Declaration]
      })
  }

  private def buildTimeForAst(implicit ast: Tree): Long = {
    //count traits and implicits
    implicitsAndTraitsIn(ast).size.toLong
  }

  private def analysed(files: Seq[java.io.File]) = {
    val fileComplexities = files.map {
      case ioFile =>
        val fileContents = readFile(ioFile)
        ioFile ->
          astForFileContent(fileContents).right.map {
            case ast =>
              val buildtime = buildTimeForAst(ast)
              val classes = classesForAst(ast)
              val methods = methodsForAst(ast)
              (buildtime, classes, methods)
          }.left.map { case error => new Error(s"error in file: ${ioFile.getName} - $error") }
    }

    fileComplexities.toMap
  }

  trait MetricsFunctions {
    def classes(file: java.io.File): Either[Error, Seq[Declaration]]
    def methods(file: java.io.File): Either[Error, Seq[Declaration]]
  }

  def metricsFunctions(files: Seq[java.io.File]): MetricsFunctions = {
    val mapping = analysed(files)
    new MetricsFunctions {
      def classes(f: java.io.File) =
          mapping.get(f) match {
            case Some(Right((_, cs, _))) => Right(cs)
            case _                       => Left(new Error(s"no classes for file: ${f.getName}"))
          }

      def methods(f: java.io.File) =
          mapping.get(f) match {
            case Some(Right((_, _, mts))) => Right(mts)
            case _                        => Left(new Error(s"no methods for file: ${f.getName}"))
          }
    }
  }
}
