import java.io.File

import gov.nasa.jpl.mbee.sbt._
import sbt.Keys._
import sbt._
import scala.language.postfixOps

lazy val archivesToExtract = TaskKey[Map[File, (File, File)]]("archives-to-extract", "ZIP files to be extracted at a target directory according to the 'extract' attribute of the corresponding library dependency")

lazy val extractArchives = TaskKey[Unit]("extract-archives", "Extracts ZIP files")

lazy val core = Project("omf-scala-core-binding-owlapi",
  file(".")).
  settings(GitVersioning.buildSettings). // in principle, unnecessary; in practice: doesn't work without this
  enablePlugins(MBEEGitPlugin).
  settings(MBEEPlugin.mbeeDynamicScriptsProjectResourceSettings(Some("gov.nasa.jpl.omf.scala.binding.owlapi"))).
  settings(
  MBEEKeys.mbeeLicenseYearOrRange := "2014-2015",
  MBEEKeys.mbeeOrganizationInfo := MBEEPlugin.MBEEOrganizations.imce,
  // include all test artifacts
  publishArtifact in Test := true,
  scalaSource in Compile := baseDirectory.value / "src",
  scalaSource in Test := baseDirectory.value / "test",
  classDirectory in Compile := baseDirectory.value / "bin",
  classDirectory in Test := baseDirectory.value / "bin.tests",

  // TODO: Jenkins CI: This should be unnecessary since the repo is in the library dependency POM!!!
  resolvers += new MavenRepository("bintray-pchiusano-scalaz-stream", "http://dl.bintray.com/pchiusano/maven"),

  libraryDependencies ++= Seq(
    MBEEPlugin.MBEEOrganizations.imce.mbeeZipArtifactVersion(
      "jpl-mbee-common-scala-libraries_core",
      MBEEKeys.mbeeReleaseVersionPrefix.value, Versions.jpl_mbee_common_scala_libraries_revision),
    MBEEPlugin.MBEEOrganizations.imce.mbeeZipArtifactVersion(
      "jpl-mbee-common-scala-libraries_other",
      MBEEKeys.mbeeReleaseVersionPrefix.value, Versions.jpl_mbee_common_scala_libraries_revision),
    MBEEPlugin.MBEEOrganizations.imce.mbeeZipArtifactVersion(
      "jpl-mbee-common-owlapi-libraries",
      MBEEKeys.mbeeReleaseVersionPrefix.value, Versions.jpl_mbee_common_scala_libraries_revision),
    MBEEPlugin.MBEEOrganizations.imce.mbeeArtifactVersion(
      "omf-scala-core",
      MBEEKeys.mbeeReleaseVersionPrefix.value, Versions.jpl_omf_core_revision) % "compile" withSources() withJavadoc(),
  // This form does get the test sources with IntelliJ
    MBEEPlugin.MBEEOrganizations.imce.mbeeArtifactVersion(
      "omf-scala-core",
      MBEEKeys.mbeeReleaseVersionPrefix.value, Versions.jpl_omf_core_revision) % "test" classifier "tests"
      artifacts(
      Artifact.classified("omf-scala-core", "tests-sources"),
      Artifact.classified("omf-scala-core", "tests-javadoc")),
  // This form does not get the test sources with IntelliJ
//    MBEEPlugin.MBEEOrganizations.imce.mbeeArtifactVersion(
//      "omf-scala-core",
//      MBEEKeys.mbeeReleaseVersionPrefix.value, Versions.jpl_omf_core_revision) % "test" classifier "tests" withSources() withJavadoc(),
    MBEEPlugin.MBEEOrganizations.imce.mbeeZipArtifactVersion(
      "gov-nasa-jpl-imce-ontologies",
      MBEEKeys.mbeeReleaseVersionPrefix.value, Versions.imce_loadprod_revision) % "runtime"
  ),

  archivesToExtract <<= (libraryDependencies, update, scalaBinaryVersion, baseDirectory, streams) map { (deps, up, ver, base, s) =>
    val artifact2extract = (for {
      dep <- deps
      tuple = (dep.name + "_" + ver + "-" + dep.revision, dep.name)
      if dep.configurations == Some("runtime")
    } yield dep.name + "_" + ver -> tuple) toMap

    val artifactArchive2extractFolder = (for {
      cReport <- up.configurations
      mReport <- cReport.modules
      (artifact, archive) <- mReport.artifacts
      if artifact.extension == "zip"
      (folder, extract) <- artifact2extract.get(artifact.name)
      subFolder = new File(folder)
      extractFolder = new File(base.getAbsolutePath + File.separator + extract)
      tuple = (subFolder, extractFolder)
    } yield archive -> tuple) toMap

    artifactArchive2extractFolder
  },

  extractArchives <<= (archivesToExtract, streams) map { (a2e, s) =>
    a2e foreach { case (archive, (subFolder, extractFolder)) =>
      val files = IO.unzip(archive, extractFolder)
      require(files.nonEmpty)
      require(extractFolder.exists)
      val extractSubFolder = extractFolder / "scala-2.11" / subFolder.name
      require(extractSubFolder.exists)
      val extractPrefix = extractSubFolder.getAbsolutePath + "/"
      for {
        file <- files
      } {
        val to = file.getAbsolutePath.stripPrefix(extractPrefix)
        IO.move(file, extractFolder / to)
      }
      IO.delete(extractSubFolder)
      IO.delete(extractFolder / "scala-2.11")
    }
  },

  test <<= (test in Test) dependsOn extractArchives,

  unmanagedClasspath in Test += baseDirectory.value / "gov-nasa-jpl-imce-ontologies"
)