name := "stripe-scala"

version := "1.1-SNAPSHOT"

organization := "com.stripe"

publishMavenStyle := true

scalaVersion := "2.11.1"

crossScalaVersions := Seq("2.11.1", "2.11.1")

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.apache.httpcomponents" % "httpclient" % "[4.1, 4.2)",
	"net.liftweb" %% "lift-json" % "2.6.2",
	"org.scalatest" %% "scalatest" % "2.2.4" % "test"
)
