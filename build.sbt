import BuildSettings.*
import Dependencies.*

scalacOptions += "-target:jvm-17"
javacOptions := Seq("-source", "17", "-target", "17")

ThisBuild / useCoursier := false

lazy val parent = (project in file("."))
  .settings(
    basicSettings,
    libraryDependencies ++= dependencies,
    Release.settings,
    publishArtifact := false,
    dependencyOverrides ++= dependenciesOverride
  )

resolvers += "Local Maven Repository" at "file:///" + Path.userHome + "/.m2/repository"

ThisBuild / parallelExecution := false
