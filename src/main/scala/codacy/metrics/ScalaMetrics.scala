package codacy.metrics

import java.nio.file.Path

import codacy.docker.api.{MetricsConfiguration, Source}
import codacy.docker.api.metrics.{FileMetrics, MetricsTool}
import com.codacy.api.dtos.Language
import utils.ReflectiveParserOps._
import utils.Tools._

import scala.util.Try

object ScalaMetrics extends MetricsTool {
  override def apply(source: Source.Directory,
                     language: Option[Language],
                     files: Option[Set[Source.File]],
                     options: Map[MetricsConfiguration.Key, MetricsConfiguration.Value]): Try[List[FileMetrics]] = {

    Try {
      //TODO
      val filesSeq: List[(Source.File, java.io.File)] = files.getOrElse(allFilesIn(new java.io.File(source.path).toPath)).map(srcFile => (srcFile, new java.io.File(source.path + "/" + srcFile.path)))(collection.breakOut)
      val metricsFn = metricsFunctions(source.path, filesSeq.map(_._2))

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
    val betterSrc =better.files.File(srcPath)
    betterSrc.children.flatMap {
      case dir if dir.isDirectory =>
        if(relativize)
          allFilesIn(dir.path, false).map(srcFile => Source.File(betterSrc.relativize(better.files.File(srcFile.path)).toString))
        else
          allFilesIn(dir.path)
      case file => Set(Source.File(file.pathAsString))
    }.to[Set]
  }


  val universe: scala.reflect.api.Universe = toolbox.u
  private type AST = universe.Tree

  import universe._

  private def isTrait(ast: AST): Boolean = ast match {
    case ClassDef(mods, _, _, _) => mods.hasFlag(Flag.TRAIT)
    case _                       => false
  }

  private def traitsIn(ast: AST): List[AST] =
    ast.children
      .map(traitsIn)
      .:+(if (isTrait(ast)) {
        List(ast)
      } else {
        List.empty
      })
      .flatten

  private def isImplicit(ast: AST): Boolean =
    (ast match {
      case c: ClassDef => Option(c.mods)
      case c: DefDef   => Option(c.mods)
      case c: ValDef   => Option(c.mods)
      //case v:AnyRef{ def mods:Modifiers } => v.mods.hasFlag(Flag.IMPLICIT)
      //case mods:Modifiers => mods.hasFlag(Flag.IMPLICIT)
      case _ => Option.empty
    }).exists(_.hasFlag(Flag.IMPLICIT))

  private def implicitsIn(ast: AST): List[AST] =
    ast.children
      .map(traitsIn)
      .:+(if (isImplicit(ast)) {
        List(ast)
      } else {
        List.empty
      })
      .flatten

  private def implicitsAndTraitsIn(ast: AST): List[AST] =
    ast.children
      .map(traitsIn)
      .:+(if (isImplicit(ast) || isTrait(ast)) {
        List(ast)
      } else {
        List.empty
      })
      .flatten

  private def astForFileContent(content: String) =
    treeFor(content).right.map {
      case ast =>
        //sadly we have to cast ...
        val cast = ast.asInstanceOf[AST]
        //count traits and implicits
        cast
    }

  private type Named = AnyRef {
    def name: String
  }

  private type Line = AnyRef {
    def line: Int
  }

  private type Class_ = Named with Line

  private type Method_ = Named with Line

  private def classesForAst(ast: AST, pNames: List[Name] = List.empty): List[Class_] = {

    val names = ast match {
      case name: NameTreeApi => pNames :+ name.name
      case _                 => pNames
    }

    ast.children.map(classesForAst(_, names)).flatten ++
      (ast match {
        case class_ : ClassDef =>
          List(new {
            def name = names.map(_.decodedName.toString).reduce(_ + "." + _)

            def line = ast.pos.line
          })
        case _ => List.empty[Class_]
      })
  }

  private def methodsForAst(ast: AST, pNames: List[Name] = List.empty): List[Method_] = {

    val names = ast match {
      case name: NameTreeApi => pNames :+ name.name
      case _                 => pNames
    }

    ast.children.map(methodsForAst(_, names)).flatten ++
      (ast match {
        case def_ : DefDef =>
          val obj: Method_ = new {
            def name = names.map(_.decodedName.toString).reduce(_ + "." + _)

            def line = ast.pos.line

            //def_.name.decodedName.toString
          }
          List(obj)
        case _ => List.empty[Method_]
      })
  }

  private def buildTimeForAst(implicit ast: AST): Long = {
    //count traits and implicits
    implicitsAndTraitsIn(ast).size.toLong
  }

  private def analysed(files: Seq[java.io.File])(implicit directory: String) = {
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

  type MetricsFunctions = AnyRef {
    def buildtime: java.io.File => Either[Error, Long]
    def classes: java.io.File => Either[Error, Seq[Method_]]
    def methods: java.io.File => Either[Error, Seq[Class_]]
  }

  def metricsFunctions(directory: String, files: Seq[java.io.File]): MetricsFunctions = {
    val mapping = analysed(files)(directory)
    new {
      def buildtime =
        (f: java.io.File) =>
          mapping.get(f) match {
            case Some(Right((bt, _, _))) => Right(bt)
            case _                       => Left(new Error(s"no buildtime for file: ${f.getName}"))
          }

      def classes =
        (f: java.io.File) =>
          mapping.get(f) match {
            case Some(Right((_, cs, _))) => Right(cs)
            case _                       => Left(new Error(s"no classes for file: ${f.getName}"))
          }

      def methods =
        (f: java.io.File) =>
          mapping.get(f) match {
            case Some(Right((_, _, mts))) => Right(mts)
            case _                        => Left(new Error(s"no methods for file: ${f.getName}"))
          }
    }
  }
}
