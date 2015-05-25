import java.io.File

import com.banno.license.Plugin.LicenseKeys._
import com.typesafe.sbt.SbtGit._
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import sbt.Keys._
import sbt._
import xerial.sbt.Pack._

/**
 * sbt \
 * -DJPL_MBEE_LOCAL_REPOSITORY=<directory path for a local Ivy2 repository (will be created if necessary)>
 */
object OMFBindingOWLAPI extends Build {

  lazy val jplSettings = Seq(
    scalaVersion := Versions.scala,
    organization := "gov.nasa.jpl.mbee.omf",
    organizationName := "JPL, Caltech",
    organizationHomepage := Some(url("https://mbse.jpl.nasa.gov")),
    publishMavenStyle := false,
    publishTo := {
      Option.apply(System.getProperty("JPL_MBEE_LOCAL_REPOSITORY")) match {
        case Some(dir) => Some(Resolver.file("file", new File(dir))(Resolver.ivyStylePatterns))
        case None => sys.error("Set -DJPL_MBEE_LOCAL_REPOSITORY=<dir> where <dir> is a local Ivy repository directory")
      }
    },
    resolvers += {
      Option.apply(System.getProperty("JPL_MBEE_LOCAL_REPOSITORY")) match {
        case Some(dir) => Resolver.file("file", new File(dir))(Resolver.ivyStylePatterns)
        case None => sys.error("Set -DJPL_MBEE_LOCAL_REPOSITORY=<dir> where <dir> is a local Ivy repository directory")
      }
    }
  )

  lazy val commonSettings =
    Defaults.coreDefaultSettings ++ Defaults.runnerSettings ++ Defaults.baseTasks ++ graphSettings

  lazy val sourcePublishSettings = Seq(
    // include all test artifacts
    publishArtifact in Test := true
  )

  def mappingFromProject(mappings: ((Seq[TaskKey[File]], Seq[Configuration]), String)*)(currentProject: ProjectRef, structure: BuildStructure): Task[Seq[(File, String)]] = {
    (mappings flatMap { case ((targetTasks: Seq[TaskKey[File]], configs: Seq[Configuration]), where: String) =>
      targetTasks flatMap { t: TaskKey[File] =>
        configs map { c =>
          Def.task {
            val file = ((t in c) in currentProject).value
            (file, where + "/" + file.getName)
          } evaluate structure.data
        }
      }
    }).join
  }

  lazy val sourcePackSettings = packSettings ++ Seq(
    packExpandedClasspath := false,
    packLibJars := Seq.empty,
    packUpdateReports := Seq.empty,
    mappings in pack <<= (thisProjectRef, buildStructure) flatMap mappingFromProject(
      (Seq(packageBin), Seq(Compile, Test)) -> "lib",
      (Seq(packageSrc), Seq(Compile, Test)) -> "lib.srcs",
      (Seq(packageDoc), Seq(Compile, Test)) -> "lib.javadoc"
    )
  ) ++ publishPackZipArchive

  lazy val archivesToExtract = TaskKey[Map[File, (File, File)]]("archives-to-extract", "ZIP files to be extracted at a target directory according to the 'extract' attribute of the corresponding library dependency")

  lazy val extractArchives = TaskKey[Unit]("extract-archives", "Extracts ZIP files")

  lazy val extractSettings = Seq(

    archivesToExtract <<= (libraryDependencies, update, scalaBinaryVersion, baseDirectory) map { (deps, up, ver, base) =>
      val artifact2extract = (for {
        dep <- deps
        tuple = (dep.name + "-" + dep.revision, dep.name)
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
        s.log.info(s"Extracting archive $archive\n=> $extractFolder (sub-folder=${subFolder.name})")
        val files = IO.unzip(archive, extractFolder)
        require(files.nonEmpty)
        require(extractFolder.exists)
        val extractSubFolder = extractFolder / subFolder.name
        require(extractSubFolder.exists)
        val extractPrefix = extractSubFolder.getAbsolutePath + "/"
        for {
          file <- files
        } {
          val to = file.getAbsolutePath.stripPrefix(extractPrefix)
          IO.move(file, extractFolder / to)
        }
        IO.delete(extractSubFolder)
      }
    }
  )

  lazy val owlapi = Project(
    "omf-scala-core-binding-owlapi",
    file(".")).
    settings(versionWithGit: _*).
    settings(showCurrentGitBranch: _*).
    settings(jplSettings: _*).
    settings(commonSettings: _*).
    settings(sourcePublishSettings: _*).
    settings(com.banno.license.Plugin.licenseSettings: _*).
    settings(sourcePackSettings: _*).
    settings(extractSettings: _*).
    settings(
      removeExistingHeaderBlock := true,
      scalaSource in Compile := baseDirectory.value / "src",
      scalaSource in Test := baseDirectory.value / "test",

      libraryDependencies ++= Seq(
        "gov.nasa.jpl.mbee.omf" %% "omf-scala-core" % Versions.jpl_omf_core % "compile" withSources() withJavadoc(),
        "gov.nasa.jpl.mbee.omf" %% "omf-scala-core" % Versions.jpl_omf_core % "test" classifier "tests"
          artifacts(
            Artifact.classified("omf-scala-core", "tests-sources"),
            Artifact.classified("omf-scala-core", "tests-javadoc")),
        "gov.nasa.jpl.mbee" %% "jpl-mbee-common-scala-libraries_core" % Versions.jpl_mbee_core,
        "gov.nasa.jpl.mbee" %% "jpl-mbee-common-owlapi-libraries" % Versions.jpl_owlapi,
        "gov.nasa.jpl.imce" %% "gov-nasa-jpl-imce-ontologies" % Versions.imce_loadprod
      )
    )
}