package codacy.metrics

import java.nio.file.Path

import codacy.docker.api.metrics.{FileMetrics, MetricsTool}
import codacy.docker.api.{MetricsConfiguration, Source}
import com.codacy.api.dtos.Language

import scala.util.{Properties, Try}

object ScalaMetrics extends MetricsTool {
  override def apply(source: Source.Directory,
                     language: Option[Language],
                     files: Option[Set[Source.File]],
                     options: Map[MetricsConfiguration.Key, MetricsConfiguration.Value]): Try[List[FileMetrics]] = {

    Try {
      val filesSeq: Set[Source.File] = files.getOrElse(allFilesIn(new java.io.File(source.path).toPath))

      filesSeq.map { srcFile =>
        val fileWithFullPath = Source.File(source.path + "/" + srcFile.path)

        val (classCount, methodCount) = classesAndMethods(fileWithFullPath) match {
          case Some((classes, methods)) => (Some(classes), Some(methods))
          case _                        => (None, None)
        }

        FileMetrics(filename = srcFile.path, nrClasses = classCount, nrMethods = methodCount)
      }(collection.breakOut)
    }
  }

  private def allFilesIn(srcPath: Path, relativize: Boolean = true): Set[Source.File] = {
    val betterSrc = better.files.File(srcPath)

    betterSrc.children.flatMap {
      case dir if dir.isDirectory =>
        if (relativize)
          allFilesIn(dir.path, false).map(srcFile =>
            Source.File(betterSrc.relativize(better.files.File(srcFile.path)).toString))
        else
          allFilesIn(dir.path)
      case file => Set(Source.File(file.pathAsString))
    }.to[Set]
  }

  private def classesAndMethods(ioFile: Source.File): Option[(Int, Int)] = {
    val fileContent = readFile(ioFile)
    ScalaParser.treeFor(fileContent).toOption.map(ScalaParser.countClassesAndMethods)
  }

  private def readFile(file: Source.File): String = {
    better.files.File(file.path).lines.mkString(Properties.lineSeparator)
  }

}
