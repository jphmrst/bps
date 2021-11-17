
val scala3Version = "3.1.0"

// library name
name := "scala-tms"

// library version
version := "0.0.1"

/////////////////////////////////////////////////////////////////
// begin maven etc. publishing information

// groupId, SCM, license information
organization := "org.maraist"
homepage := Some(url("https://github.com/jphmrst/scala-tms"))
scmInfo := Some(ScmInfo(
  url("https://github.com/jphmrst/scala-tms"),
  "git@github.com:jphmrst/scala-tms.git"))
developers := List(Developer(
  "jphmrst", "jphmrst", "via-github@maraist.org",
  url("https://maraist.org/work/")))
licenses += (
  "Educational",
  url("https://github.com/jphmrst/scala-tms/blob/master/LICENSE.txt"))

// add sonatype repository settings
// snapshot versions publish to sonatype snapshot repository
// other versions publish to sonatype staging repository
pomIncludeRepository := { _ => false }
val nexus = "https://s01.oss.sonatype.org/"
publishTo := {
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
publishMavenStyle := true

ThisBuild / versionScheme := Some("semver-spec")

// end of maven etc. publishing section
/////////////////////////////////////////////////////////////////

Global / excludeLintKeys ++= Set(scalacOptions)
Compile / doc / scalacOptions ++= Seq(
  "-groups",
  "-doc-root-content", "src/main/rootdoc.txt"
)

lazy val main = project
  .in(file("."))
  .settings(
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      // "org.scala-lang.modules" %% "scala-xml" % "2.0.1",
      // "org.typelevel" %% "paiges-core" % "0.4.2",
      "org.scalactic" %% "scalactic" % "3.2.9",
      "org.scalatest" %% "scalatest" % "3.2.9" % "test"
      // , "org.maraist" %% "scala-latex" % "1.1.1"
    ),
    compile / watchTriggers += baseDirectory.value.toGlob / "build.sbt",
    unmanagedSources / excludeFilter := ".#*",
    scalacOptions ++= Seq( "-source:future-migration" ),
  ) // .dependsOn(RootProject(file("/home/jm/Lib/Scala/LaTeX/")))

