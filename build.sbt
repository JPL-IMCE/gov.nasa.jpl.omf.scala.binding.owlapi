
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

lazy val buildUTCDate = SettingKey[String]("build-utc-date", "The UDC Date of the build")

buildUTCDate in Global := {
  import java.util.{ Date, TimeZone }
  val formatter = new java.text.SimpleDateFormat("yyyy-MM-dd-HH:mm")
  formatter.setTimeZone(TimeZone.getTimeZone("UTC"))
  formatter.format(new Date)
}

lazy val core =
  Project("omf-scala-core-binding-owlapi", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(dynamicScriptsResourceSettings(Some("gov.nasa.jpl.omf.scala.binding.owlapi")))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(IMCEPlugin.scalaDocSettings(diagrams=false))
  .settings(
    IMCEKeys.licenseYearOrRange := "2014-2016",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,

    buildInfoPackage := "gov.nasa.jpl.omf.scala.binding.owlapi",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    projectID := {
      val previous = projectID.value
      previous.extra("build.date.utc" -> buildUTCDate.value)
    },

    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,
    git.baseVersion := Versions.version,
    // include all test artifacts
    publishArtifact in Test := true,
    scalaSource in Compile := baseDirectory.value / "src",
    classDirectory in Compile := baseDirectory.value / "bin",
    cleanFiles += (classDirectory in Compile).value,

    scalaSource in Test := baseDirectory.value / "test",
    classDirectory in Test := baseDirectory.value / "bin.tests",
    cleanFiles += (classDirectory in Test).value,

    resourceDirectory in Test := baseDirectory.value / "target" / "extracted" / "imce-omf_ontologies",

    IMCEKeys.nexusJavadocRepositoryRestAPIURL2RepositoryName := Map(
      "https://oss.sonatype.org/service/local" -> "releases",
      "https://cae-nexuspro.jpl.nasa.gov/nexus/service/local" -> "JPL"),
    IMCEKeys.pomRepositoryPathRegex := """\<repositoryPath\>\s*([^\"]*)\s*\<\/repositoryPath\>""".r,

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
          extractFolder = base / "target" / "extracted" / extract
          tuple = (subFolder, extractFolder)
        } yield archive -> tuple)
        .toMap

        artifactArchive2extractFolder
      },

    extractArchives <<= (archivesToExtract, streams).map { (a2e, s) =>
      a2e foreach { case (archive, (subFolder, extractFolder)) =>
        val files = IO.unzip(archive, extractFolder)
        require(files.nonEmpty)
        require(extractFolder.exists, extractFolder)
      }
    },

    test <<= (test in Test) dependsOn extractArchives,

    unmanagedClasspath in Test += baseDirectory.value / "target" / "extracted" / "imce-omf_ontologies"
  )
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)

def dynamicScriptsResourceSettings(dynamicScriptsProjectName: Option[String] = None): Seq[Setting[_]] = {

  import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._

  def addIfExists(f: File, name: String): Seq[(File, String)] =
    if (!f.exists) Seq()
    else Seq((f, name))

  val QUALIFIED_NAME = "^[a-zA-Z][\\w_]*(\\.[a-zA-Z][\\w_]*)*$".r

  Seq(
    // the '*-resource.zip' archive will start from: 'dynamicScripts/<dynamicScriptsProjectName>'
    com.typesafe.sbt.packager.Keys.topLevelDirectory in Universal := {
      val projectName = dynamicScriptsProjectName.getOrElse(baseDirectory.value.getName)
      require(
        QUALIFIED_NAME.pattern.matcher(projectName).matches,
        s"The project name, '$projectName` is not a valid Java qualified name")
      Some("dynamicScripts/" + projectName)
    },

    // name the '*-resource.zip' in the same way as other artifacts
    com.typesafe.sbt.packager.Keys.packageName in Universal :=
      normalizedName.value + "_" + scalaBinaryVersion.value + "-" + version.value + "-resource",

    // contents of the '*-resource.zip' to be produced by 'universal:packageBin'
    mappings in Universal <++= (
      baseDirectory,
      packageBin in Compile,
      packageSrc in Compile,
      packageDoc in Compile,
      packageBin in Test,
      packageSrc in Test,
      packageDoc in Test) map {
      (base, bin, src, doc, binT, srcT, docT) =>
        val dir = base / "svn" / "org.omg.oti"
        (dir ** "*.dynamicScripts").pair(relativeTo(dir)) ++
          ((dir ** "*.md") --- (dir / "sbt.staging" ***)).pair(relativeTo(dir)) ++
          (dir / "models" ** "*.mdzip").pair(relativeTo(dir)) ++
          com.typesafe.sbt.packager.MappingsHelper.directory(dir / "resources") ++
          addIfExists(bin, "lib/" + bin.name) ++
          addIfExists(binT, "lib/" + binT.name) ++
          addIfExists(src, "lib.sources/" + src.name) ++
          addIfExists(srcT, "lib.sources/" + srcT.name) ++
          addIfExists(doc, "lib.javadoc/" + doc.name) ++
          addIfExists(docT, "lib.javadoc/" + docT.name)
    },

    artifacts <+= (name in Universal) { n => Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) },
    packagedArtifacts <+= (packageBin in Universal, name in Universal) map { (p, n) =>
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    }
  )
}