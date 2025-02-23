import sbt.Keys.*

object BuildSettings {

  lazy val basicSettings = Seq(
    name                  := "recursive-comparator",
    startYear             := Some(2017),
    scalaVersion          := "2.13.6",
    resolvers             ++= Dependencies.repoResolvers,
    organization := "com.example"
  )
}
