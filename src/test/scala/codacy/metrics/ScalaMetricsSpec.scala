package codacy.metrics

import codacy.docker.api.Source
import codacy.docker.api.metrics.FileMetrics
import org.specs2.mutable.Specification

import scala.util.Success

class ScalaMetricsSpec extends Specification {

  val dummyScalaFileMetrics =
    FileMetrics("codacy/metrics/DummyScalaFile.scala", nrClasses = Some(1), nrMethods = Some(2))
  val loggerFileMetrics = FileMetrics("codacy/metrics/Logger.scala", None, nrClasses = Some(1), nrMethods = Some(3))

  val targetDir = "src/test/resources"

  "ScalaMetrics" should {
    "get metrics" in {
      "all files within a directory" in {
        val expectedFileMetrics = List(dummyScalaFileMetrics, loggerFileMetrics)
        val fileMetricsMap =
          ScalaMetrics(source = Source.Directory(targetDir), language = None, files = None, options = Map.empty)

        fileMetricsMap should beLike {
          case Success(elems) => elems should containTheSameElementsAs(expectedFileMetrics)
        }
      }

      "specific files" in {
        val expectedFileMetrics = List(loggerFileMetrics)

        val fileMetricsMap = ScalaMetrics(
          source = Source.Directory(targetDir),
          language = None,
          files = Some(Set(Source.File(loggerFileMetrics.filename))),
          options = Map.empty)

        fileMetricsMap should beLike {
          case Success(elems) => elems should containTheSameElementsAs(expectedFileMetrics)
        }
      }
    }
  }
}
