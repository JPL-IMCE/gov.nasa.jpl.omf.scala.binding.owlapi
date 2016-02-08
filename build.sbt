
import sbt.Keys._
import sbt._
import scala.language.postfixOps

import gov.nasa.jpl.imce.sbt._

import java.io.File

useGpg := true

developers := List(
  Developer(
    id="rouquett",
    name="Nicolas F. Rouquette",
    email="nicolas.f.rouquette@jpl.nasa.gov",
    url=url("https://gateway.jpl.nasa.gov/personal/rouquett/default.aspx")))

lazy val archivesToExtract = TaskKey[Map[File, (File, File)]](
     "archives-to-extract", 
     "ZIP files to be extracted at a target directory according to the 'extract' attribute of the corresponding library dependency")

lazy val extractArchives = TaskKey[Unit]("extract-archives", "Extracts ZIP files")

lazy val core =
  Project("omf-scala-core-binding-owlapi", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(IMCEPlugin.dynamicScriptsProjectResourceSettings(Some("gov.nasa.jpl.omf.scala.binding.owlapi")))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(IMCEPlugin.scalaDocSettings(diagrams=false))
  .settings(
    IMCEKeys.licenseYearOrRange := "2014-2016",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,
    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,
    git.baseVersion := Versions.version,
    // include all test artifacts
    publishArtifact in Test := true,
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",
    classDirectory in Compile := baseDirectory.value / "bin",
    classDirectory in Test := baseDirectory.value / "bin.tests",
    resourceDirectory in Test := baseDirectory.value / "gov-nasa-jpl-imce-ontologies",

    IMCEKeys.nexusJavadocRepositoryRestAPIURL2RepositoryName := Map(
      "https://oss.sonatype.org/service/local" -> "releases",
      "https://cae-nexuspro.jpl.nasa.gov/nexus/service/local" -> "JPL"),
    IMCEKeys.pomRepositoryPathRegex := """\<repositoryPath\>\s*([^\"]*)\s*\<\/repositoryPath\>""".r,

    // TODO: Jenkins CI: This should be unnecessary since the repo is in the library dependency POM!!!
    //resolvers += new MavenRepository("bintray-pchiusano-scalaz-stream", "http://dl.bintray.com/pchiusano/maven"),

    libraryDependencies ++= Seq(

      "gov.nasa.jpl.imce.thirdParty" %% "owlapi-libraries"
        % Versions_owlapi_libraries.version artifacts
        Artifact("owlapi-libraries", "zip", "zip", Some("resource"), Seq(), None, Map()),

      "gov.nasa.jpl.imce.omf" %% "omf-scala-core"
        % Versions_omf_scala_core.version % "compile" withSources() withJavadoc() artifacts
        Artifact("omf-scala-core", "zip", "zip", Some("resource"), Seq(), None, Map()),

      "gov.nasa.jpl.imce.omf" %% "omf-scala-core" % Versions_omf_scala_core.version %
        "test" classifier "tests" withSources() withJavadoc() artifacts(
        Artifact.classified("omf-scala-core", "tests-sources"),
        Artifact.classified("omf-scala-core", "tests-javadoc")),

      "gov.nasa.jpl.imce.omf" %% "imce-omf_ontologies" % Versions_imce_omf_ontologies.version %
      "runtime" artifacts Artifact("imce-omf_ontologies", "zip", "zip")
    ),

    archivesToExtract <<=
      (libraryDependencies, update, scalaBinaryVersion, baseDirectory, streams)
      .map { (deps, up, ver, base, s) =>
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
        } yield archive -> tuple)
        .toMap

        artifactArchive2extractFolder
      },

    extractArchives <<= (archivesToExtract, streams).map { (a2e, s) =>
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
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)
