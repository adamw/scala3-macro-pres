lazy val commonSettings = Seq(
  organization := "com.softwaremill.demo",
  scalaVersion := "3.0.0-RC3"
)

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publishArtifact := false, name := "scala3-macro-pres")
  .aggregate(core)

lazy val core: Project = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(name := "core")
  .settings(libraryDependencies += "com.softwaremill.sttp.model" %% "core" % "1.4.4")
