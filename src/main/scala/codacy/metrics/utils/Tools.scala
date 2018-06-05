package codacy.metrics.utils


import java.io

import codacy.foundation.api.FileContents

import _root_.scala.util.Properties

object Tools {

  def readFile(file: io.File) = {
    if(!file.exists())
      throw new IllegalArgumentException(file.getPath)
    FileContents.getLines(file).map(_.mkString(Properties.lineSeparator)).getOrElse("")
  }

}
