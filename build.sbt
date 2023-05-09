name := "BelegListSentimentAufgabe"

version := "0.1"

scalaVersion := "2.13.10"
run := Defaults.runTask(fullClasspath in Runtime, mainClass in run in Compile, runner in run).evaluated

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.13.2",
  "org.scalactic" %% "scalactic" % "3.2.15",
  "org.scalatest" %% "scalatest" % "3.2.15",
  "org.jfree" % "jfreechart" % "1.0.19")
