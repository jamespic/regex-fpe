import sbt._
import Keys._

object MyBuild extends Build {
  lazy val genericSettings = super.settings ++ Seq(scalaVersion := "2.11.8")
  lazy val root = (project in file(".")).settings(genericSettings).dependsOn(regexKeyspace, javaFPE, regexFPE)
  lazy val regexKeyspace = (project in file("regex-keyspace")).settings(genericSettings)
  lazy val javaFPE = (project in file("JavaFPE")).settings(genericSettings)
  lazy val regexFPE = (project in file("regex-fpe")).settings(genericSettings).dependsOn(regexKeyspace, javaFPE)
}