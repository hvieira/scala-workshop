import sbt._

object Dependencies {
  val testDeps = Seq(
    "org.scalactic" %% "scalactic" % "3.1.1" % Test,
    "org.scalatest" %% "scalatest" % "3.1.1" % Test,
    "org.scalamock" %% "scalamock" % "4.4.0" % Test
  )
}
