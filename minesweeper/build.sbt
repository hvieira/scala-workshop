import Dependencies._
import sbt.Keys.libraryDependencies

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "minesweeper",
    libraryDependencies ++= testDeps,
    resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases",
    scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")
  )

