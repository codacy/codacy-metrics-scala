package codacy.foundation.api

import java.io
import java.nio.charset.CodingErrorAction

import codacy.other.Logger
import play.api.libs.json.Json

import scala.io.{Codec, Source}
import scala.util.control.NonFatal

final case class FileStoreId(id: Long)

final case class FileSource(id: FileStoreId, filename: String, contents: String)

final case class File(name: String, changed: Boolean, ignored: Boolean)

object File {
  implicit val fileFmt = Json.format[File]
}

final case class FileError(filename: String, line: Int, cause: String)

final case class FileContents(contents: Seq[String]) {
  val getLines = (start: Int, end: Int) => FileContents.getLines(contents, start, end)
}

object FileContents {
  def getContentsForFile(ioFile: io.File): Option[FileContents] = {
    getLines(ioFile).map {
      contents =>
        FileContents(contents)
    }
  }

  def getLines(ioFile: io.File): Option[Seq[String]] = {
    getLines(ioFile.getAbsoluteFile.toString)
  }

  def getLines(filename: String): Option[Seq[String]] = {
    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    val sourceOpt = try {
      Some(Source.fromFile(filename))
    } catch {
      case NonFatal(e) =>
        Logger.warn(s"Failed to open file: $filename - ${e.getMessage}")
        None
    }

    try {
      sourceOpt.map {
        source =>
          source.getLines().toList
      }
    } catch {
      case NonFatal(_) =>
        Logger.warn(s"Failed to read file: $filename")
        None
    } finally {
      sourceOpt.foreach(_.close())
    }
  }

  def getLines(contents: Seq[String], start: Int, end: Int): Seq[String] = {

    val nStart = if (start > end) end else start
    val nEnd = if (end < start) start else end


    val normalizedStart = Math.min(Math.max(nStart, 1) - 1, contents.length - 1)
    val normalizedEnd = Math.max(1, Math.min(nEnd, contents.length))

    contents.slice(normalizedStart, normalizedEnd)
  }
}