ThisBuild / version := "v0.0.2"
ThisBuild / scalaVersion := "3.6.3"
Global / onChangedBuildSource := ReloadOnSourceChanges

scalacOptions ++= Seq(// use ++= to add to existing options
    "-explaintypes",
  "-explain",
  "-deprecation"
)
lazy val root = (project in file("."))
  .settings(
    name := "uml2semantics",
    libraryDependencies ++= Seq(
        "com.github.scopt" %% "scopt" % "4.1.0",
        "com.github.tototoshi" %% "scala-csv" % "2.0.0",
        "net.sourceforge.owlapi" % "owlapi-distribution" % "5.5.1",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
        "ch.qos.logback" % "logback-classic" % "1.5.15",
        "com.lihaoyi" %% "sourcecode" % "0.4.2",
        "org.apache.commons" % "commons-collections4" % "4.4"
//        "org.scala-lang" %% "scala3-library" % "3.6.3"
    ),
    dependencyOverrides += "org.scala-lang" % "scala3-library" % scalaVersion.value,
    assembly / assemblyMergeStrategy := {
      case PathList("module-info.class") => MergeStrategy.discard
      case PathList("reference.conf") => MergeStrategy.concat
      case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
      case x => MergeStrategy.first
    },
    assembly / assemblyJarName := "uml2semantics.jar",
    assembly / mainClass := Some("org.uml2semantics.uml2owl")
  )
