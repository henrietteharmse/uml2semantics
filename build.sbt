ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

scalacOptions ++= Seq(// use ++= to add to existing options
//    "-explaintypes"
)
lazy val root = (project in file("."))
  .settings(
    name := "scala-uml2owl",
    libraryDependencies ++= Seq(
        "com.github.scopt" %% "scopt" % "4.1.0",
        "com.github.tototoshi" %% "scala-csv" % "1.3.10",
        "net.sourceforge.owlapi" % "owlapi-distribution" % "5.5.0",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
        "ch.qos.logback" % "logback-classic" % "1.3.5"
    )
  )
