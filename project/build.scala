import sbt._
import Keys._

object MyBuild extends Build {
  lazy val genericSettings = super.settings ++ Seq(scalaVersion := "2.11.8")
  lazy val root = (project in file(".")).settings(genericSettings)
    //.aggregate(regexKeyspace, javaFPE, regexFPE)
    .dependsOn(regexKeyspace, javaFPE, regexFPE, examples)
  lazy val regexKeyspace = (project in file("regex-keyspace")).settings(genericSettings)
  lazy val javaFPE = (project in file("JavaFPE")).settings(genericSettings)
  lazy val regexFPE = (project in file("regex-fpe")).settings(genericSettings).dependsOn(regexKeyspace, javaFPE)
  lazy val examples = (project in file("examples")).settings(genericSettings).dependsOn(regexFPE)
}
