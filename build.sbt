name := "mouse"

version := "0.1"

scalaVersion := "2.12.0"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "xyz.hyperreal"

//resolvers += Resolver.sonatypeRepo( "snapshots" )

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies ++= {
	val akkaV = "2.4.12"
	val sprayV = "1.3.4"
	Seq(
//		"io.spray"          %% "spray-can"     % sprayV,
//		"io.spray"          %% "spray-routing" % sprayV,
//		"io.spray"          %% "spray-json"    % "1.3.2",
//		"io.spray"          %% "spray-testkit" % sprayV   % "test",
		"com.typesafe.akka" %% "akka-actor"    % akkaV
//		"com.typesafe.akka" %% "akka-remote"   % akkaV,
//		"com.typesafe.akka" %% "akka-testkit"  % akkaV    % "test",
//		"org.specs2"        %% "specs2-core"   % "2.3.11" % "test"
	)
}

libraryDependencies ++= Seq(
	"org.scalatest" % "scalatest_2.11" % "3.0.0" % "test",
	"org.scalacheck" % "scalacheck_2.11" % "1.13.2" % "test"
)

//libraryDependencies ++= Seq(
//	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
//	"org.scala-lang.modules" %% "scala-swing" % "1.0.2"
//)

mainClass in (Compile, run) := Some( "xyz.hyperreal." + name.value + ".Main" )

mainClass in assembly := Some( "xyz.hyperreal." + name.value + ".Main" )

assemblyJarName in assembly := name.value + "-" + version.value + ".jar"

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/edadma/" + name.value))

pomExtra := (
  <scm>
    <url>git@github.com:edadma/{name.value}.git</url>
    <connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>https://github.com/edadma</url>
    </developer>
  </developers>)
