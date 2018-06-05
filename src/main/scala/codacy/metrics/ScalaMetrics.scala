package codacy.metrics

import java.nio.file.Path

import codacy.docker.api.metrics.{FileMetrics, MetricsTool}
import codacy.docker.api.{MetricsConfiguration, Source}
import codacy.metrics.utils.ReflectiveParserOps._
import com.codacy.api.dtos.Language

import _root_.scala.util.{Properties, Try}

object ScalaMetrics extends MetricsTool {
  override def apply(source: Source.Directory,
                     language: Option[Language],
                     files: Option[Set[Source.File]],
                     options: Map[MetricsConfiguration.Key, MetricsConfiguration.Value]): Try[List[FileMetrics]] = {

    Try {
      val filesSeq: Set[Source.File] = files.getOrElse(allFilesIn(new java.io.File(source.path).toPath))

      filesSeq.map {
        srcFile =>
          val (classCount, methodCount) = classesAndMethods(Source.File(source.path + "/" + srcFile.path)) match {
            case Some((classes, methods)) => (Some(classes), Some(methods))
            case _ => (None, None)
          }
          FileMetrics(
            filename = srcFile.path,
            nrClasses = classCount,
            nrMethods = methodCount
          )
      }(collection.breakOut)
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

  private def readFile(file: Source.File) = {
    better.files.File(file.path).lines.mkString(Properties.lineSeparator)
  }

  import toolbox.u._

  private def countClassesAndMethods(ast: Tree): (Int, Int) = {
    val countOnThisNode = ast match {
      case _: DefDef => (0, 1)
      case _: ClassDef => (1, 0)
      case _ => (0, 0)
    }

    ast.children.map(countClassesAndMethods).fold(countOnThisNode) {
      case ((c1, m1), (c2, m2)) =>
        (c1 + c2, m1 + m2)
    }
  }

  private def classesAndMethods(ioFile: Source.File) = {
    val fileContent = readFile(ioFile)
    treeFor(fileContent).toOption.map(countClassesAndMethods)
  }
}
