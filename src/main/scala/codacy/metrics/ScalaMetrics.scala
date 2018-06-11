package codacy.metrics

import better.files.File
import codacy.docker.api.metrics.{FileMetrics, MetricsTool}
import codacy.docker.api.{MetricsConfiguration, Source}
import com.codacy.api.dtos.Language
import com.codacy.docker.api.utils.FileHelper

import scala.util.Try

object ScalaMetrics extends MetricsTool {
  override def apply(source: Source.Directory,
                     language: Option[Language],
                     files: Option[Set[Source.File]],
                     options: Map[MetricsConfiguration.Key, MetricsConfiguration.Value]): Try[List[FileMetrics]] = {

    Try {
      val filesSeq: Set[File] =
        files
          .map(_.map(file => File(file.path)))
          .getOrElse(FileHelper.listAllFiles(source.path).map(ioFile => File(ioFile.toPath))(collection.breakOut))

      filesSeq.map { file =>
        val (classCount, methodCount) = classesAndMethods(file) match {
          case Some((classes, methods)) => (Some(classes), Some(methods))
          case _                        => (None, None)
        }

        FileMetrics(
          filename = FileHelper.stripPath(file.path.toString, source.path),
          nrClasses = classCount,
          nrMethods = methodCount)
      }(collection.breakOut)
    }
  }

  private def classesAndMethods(file: File): Option[(Int, Int)] = {
    val fileContent = file.contentAsString
    ScalaParser.treeFor(fileContent).toOption.map(ScalaParser.countClassesAndMethods)
  }

}
