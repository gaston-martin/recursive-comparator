import sbt.*
import sbt.Keys.*
import sbtrelease.*
import sbtrelease.ReleasePlugin.autoImport.*
import sbtrelease.ReleaseStateTransformations.*

object Release {

  val settings = Seq(
    releaseVersion := { ver =>
      Option(System.getenv("RELEASE_VERSION")) filter (_.nonEmpty) map { v ⇒
        Version(v) map (_.unapply) getOrElse versionFormatError(v)
      } getOrElse {
        Version(ver) map (_.withoutQualifier.unapply) getOrElse versionFormatError(ver)
      }
    },
    releaseNextVersion := { ver =>
      Version(ver) map (_.bumpBugfix.asSnapshot.unapply) getOrElse versionFormatError(ver)
    },
    releaseIgnoreUntrackedFiles := true,
    releaseCrossBuild := false,
    releaseTagName := (ThisBuild / version).value,
    releaseProcess := Seq[ReleaseStep](
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )

}
