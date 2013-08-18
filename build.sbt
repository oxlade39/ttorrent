
name := "ttorrent"

organization := "com.turn"

version := "1.3-SNAPSHOT"

scalaVersion := "2.10.2"

// Java then Scala for main sources
compileOrder in Compile := CompileOrder.JavaThenScala

githubPagesCheckoutDir := Path.userHome / "proj" / "oxlade39.github.com" / "_site" / "maven"

publishMavenStyle := true

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "oxlade39.github.com" at "http://oxlade39.github.com/maven/"

resolvers += "JBoss Thirdparty Releases" at "https://repository.jboss.org/nexus/content/repositories/thirdparty-releases"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.2.0",
  "com.typesafe.akka" %% "akka-slf4j" % "2.2.0",
  "commons-io" % "commons-io" % "2.1",
  "org.simpleframework" % "simple" % "4.1.21",
  "org.slf4j" % "slf4j-log4j12" % "1.6.4",
  "net.sf" % "jargs" % "1.0",
  "org.pegdown" % "pegdown" % "1.0.2" % "test",
  "org.mockito" % "mockito-all" % "1.9.0" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.2.0"
)

libraryDependencies <+= scalaVersion {
  case "2.9.1" => "org.specs2" %% "specs2" % "1.12.3" % "test"
  case "2.9.2" => "org.specs2" %% "specs2" % "1.12.3" % "test"
  case _ => "org.specs2" %% "specs2" % "1.14" % "test"
}