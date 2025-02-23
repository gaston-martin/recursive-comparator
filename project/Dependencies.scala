import sbt.*

object Dependencies {

  val externalRepos = Seq(
    "Maven Central" at "https://repo1.maven.org/maven2/",
    "Sbt Plugins" at "https://repo.scala-sbt.org/scalasbt/sbt-plugin-releases",
    "Typesafe repository snapshots" at "https://repo.typesafe.com/typesafe/snapshots/",
    "Typesafe repository releases" at "https://repo.typesafe.com/typesafe/releases/",
    "Java.net Maven2 Repository" at "https://download.java.net/maven/2/"
  )

  val repoResolvers = externalRepos

  object Versions {
    val scalike = "4.0.0"
    val h2 = "1.4.193"
    val jackson = "2.14.3"
    val scalatest = "3.2.15"
    val mockito = "5.8.0"
    val jodaTime = "2.12.5"
    val ficus = "1.5.1" // si tuvieramos scala 2.12.x podriamos usar pureconfig que esta mejor mantenida.
    val sttpVersion = "3.4.1"
    val scalajVersion = "2.4.2"
  }

  val basicDependencies = Seq(
    "com.fasterxml.jackson.core" % "jackson-core" % Versions.jackson,
    "com.fasterxml.jackson.core" % "jackson-annotations" % Versions.jackson,
    ("com.fasterxml.jackson.core" % "jackson-databind" % Versions.jackson)
      .exclude("com.fasterxml.jackson.core", "jackson-annotations")
      .exclude("com.fasterxml.jackson.core", "jackson-core"),
    ("com.fasterxml.jackson.module" %% "jackson-module-scala" % Versions.jackson) // jackson_scala
      .exclude("com.fasterxml.jackson.core", "jackson-databind")
      .exclude("org.scala-lang", "scala-library")
      .exclude("com.fasterxml.jackson.core", "jackson-annotations")
      .exclude("com.fasterxml.jackson.core", "jackson-core")
      .exclude("org.scala-lang", "scala-reflect"),
    "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % "2.14.3", // jackson_databind_joda
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-csv" % Versions.jackson,
    "joda-time" % "joda-time" % Versions.jodaTime, // joda_time
    "com.iheart" %% "ficus" % Versions.ficus,
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    "ch.qos.logback" % "logback-classic" % "1.2.10",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"
  )

  val testDependencies = Seq(
    "org.scalatest" %% "scalatest" % Versions.scalatest, // scalatest
    "org.scalatestplus" %% "mockito-5-12" % "3.2.19.0",
    "org.mockito" % "mockito-core" % Versions.mockito, // mockito
    "org.scalaj" %% "scalaj-http" % Versions.scalajVersion,
    "com.softwaremill.sttp.client3" %% "core" % Versions.sttpVersion // client used for regression module
  ).map(_ % Test)

  val dbSupportDependencies = Seq(
    "com.h2database" % "h2" % Versions.h2 // h2
  )

   val dependencies: Seq[ModuleID] = basicDependencies ++ testDependencies ++ dbSupportDependencies

  val dependenciesOverride: Seq[ModuleID] = Seq(
    "org.scala-lang.modules" %% "scala-xml" % "2.2.0",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
    "com.google.inject" % "guice" % "6.0.0"
  )

}
