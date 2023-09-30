ThisBuild / version := "v0.0.2"

ThisBuild / scalaVersion := "3.2.2"

scalacOptions ++= Seq(// use ++= to add to existing options
    "-explaintypes"
)
lazy val root = (project in file("."))
  .settings(
    name := "uml2semantics",
    libraryDependencies ++= Seq(
        "com.github.scopt" %% "scopt" % "4.1.0",
        "com.github.tototoshi" %% "scala-csv" % "1.3.10",
        "net.sourceforge.owlapi" % "owlapi-distribution" % "5.5.0",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
        "ch.qos.logback" % "logback-classic" % "1.3.5",
        "com.lihaoyi" %% "sourcecode" % "0.3.0"
    ),
    assembly / assemblyMergeStrategy := {

      case PathList("module-info.class") => MergeStrategy.discard
      case PathList("META-INF", "versions", xs@_, "module-info.class") => MergeStrategy.discard
      case x =>
          val oldStrategy = (assembly / assemblyMergeStrategy).value
          oldStrategy(x)
    },
    assembly / assemblyJarName := "uml2semantics.jar"
  )
