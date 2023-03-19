ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "scala-uml2owl",
    libraryDependencies ++= Seq(
//      "com.nrinaudo" %% "kantan.csv-generic" % "0.7.0",
        "com.github.scopt" %% "scopt" % "4.1.0",
       "com.github.tototoshi" %% "scala-csv" % "1.3.10"
    )
  )
