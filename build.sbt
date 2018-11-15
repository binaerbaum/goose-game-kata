import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "ch.binaerbaum",
      scalaVersion := "2.12.7",
      version      := "0.1.0"
    )),
    name := "gioco-delloca",
    libraryDependencies ++= {
      Seq(
        parsers,
        scalaTest % Test)
    }
  )