package codacy.metrics

import better.files.File
import com.codacy.plugins.api.languages.Language
import com.codacy.plugins.api.metrics.{FileMetrics, MetricsTool}
import com.codacy.plugins.api.{Options, Source}

import scala.util.Try

object ScalaMetrics extends MetricsTool {
  override def apply(source: Source.Directory,
                     language: Option[Language],
                     files: Option[Set[Source.File]],
                     options: Map[Options.Key, Options.Value]): Try[List[FileMetrics]] = {

    Try {
      val filesSeq: Set[File] =
        files.map(_.map(file => File(file.path))).getOrElse(allRegularFilesIn(source))

      filesSeq.map { file =>
        val (classCount, methodCount) = classesAndMethods(file) match {
          case Some((classes, methods)) => (Some(classes), Some(methods))
          case _                        => (None, None)
        }

        FileMetrics(
          filename = File(source.path).relativize(file).toString,
          nrClasses = classCount,
          nrMethods = methodCount)
      }(collection.breakOut)
    }
  }

  private def allRegularFilesIn(source: Source.Directory): Set[File] = {
    File(source.path).listRecursively.to[Set].filter(_.isRegularFile)
  }

  private def classesAndMethods(file: File): Option[(Int, Int)] = {
    val fileContent = file.contentAsString
    ScalaParser.treeFor(fileContent).toOption.map(ScalaParser.countClassesAndMethods)
  }

}
