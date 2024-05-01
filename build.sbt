lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.13.12"
    )),
    name := "matrices"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test
libraryDependencies ++= Seq(
  ("com.storm-enroute" %% "scalameter-core" % "0.21").cross(CrossVersion.for3Use2_13),
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3",
  "org.scalameta" %% "munit" % "0.7.26" % Test
)
