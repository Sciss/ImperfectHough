name         := "ImperfectHough"
version      := "0.1.3"
description  := "An algorithmic art project (video installation)"
organization := "de.sciss"
homepage     := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses     := Seq("gpl v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))
scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint")

libraryDependencies ++= Seq(
  "de.sciss"          %% "fileutil"   % "1.1.2",
  "de.sciss"          %% "swingplus"  % "0.2.1",
  "de.sciss"          %% "numbers"    % "0.1.1",
  "de.sciss"          %% "kollflitz"  % "0.2.0",
  "de.sciss"          %% "scalaosc"   % "1.1.5",
  "com.github.scopt"  %% "scopt"      % "3.5.0",
  "com.typesafe.akka" %% "akka-actor" % "2.4.12"
)

mainClass in assembly := Some("de.sciss.imperfect.hough.View")
assemblyJarName in assembly := s"${name.value}.jar"
target in assembly := baseDirectory.value
