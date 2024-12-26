ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "spc",
    idePackagePrefix := Some("org.syspro.spc")
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
)