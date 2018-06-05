import sbt._

object Dependencies {

  object Codacy {
    val metricsSeed = "com.codacy" %% "codacy-metrics-scala-seed" % "0.1.149"
  }

  val specs2Version = "4.2.0"
  val specs2 = "org.specs2" %% "specs2-core" % specs2Version

  val betterFiles = "com.github.pathikrit" %% "better-files" % "3.4.0"

  // Scala
  val scalalibrary = "org.scala-lang" % "scala-library" % "2.12.4"
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % "2.12.4"
  val xml = "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
}