import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    version := "1.0.0",
    scalaVersion := "2.11.1",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq("-deprecation","-feature")
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in XML_Quote 
    )
  ) aggregate(XML_Quote, tests)

  lazy val XML_Quote: Project = Project(
    "XML_Quote",
    file("XML_Quote"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies += "org.scalamacros" %% "xml" % "1.0.0-M1"
    )
  )

  lazy val tests: Project = Project(
    "tests",
    file("tests"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.scalamacros" %% "xml" % "1.0.0-M1",
        "org.scalatest" %% "scalatest" % "2.1.3" % "test"
      )
    )
  ) dependsOn(XML_Quote)
}
