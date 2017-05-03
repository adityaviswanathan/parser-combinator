name := "parser-combinators"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalatest"           %% "scalatest"                % "2.2.6" % "test",
  "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.0.4",
  "org.scala-lang" 			% "scala-reflect" 			  % "2.10.0"
)
