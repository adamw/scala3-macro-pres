lazy val commonSettings = Seq(
  organization := "com.softwaremill.demo",
//  scalaVersion := "0.27.0-RC1"
  scalaVersion := "3.0.0-M1"
)

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publishArtifact := false, name := "scala3-macro-pres")
  .aggregate(core)

lazy val core: Project = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(name := "core")
  .settings(libraryDependencies += "com.softwaremill.sttp.model" %% "core" % "1.2.0-RC6")
