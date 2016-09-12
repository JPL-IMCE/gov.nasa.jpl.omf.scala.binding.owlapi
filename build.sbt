
import sbt.Keys._
import sbt._
import scala.language.postfixOps

import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._

import java.io.File

updateOptions := updateOptions.value.withCachedResolution(true)

resolvers ++= {
  if (git.gitUncommittedChanges.value)
    Seq[Resolver](Resolver.mavenLocal)
  else
    Seq.empty[Resolver]
}

import scala.io.Source
import scala.util.control.Exception._

lazy val core =
  Project("omf-scala-binding-owlapi", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)
  .settings(dynamicScriptsResourceSettings(Some("gov.nasa.jpl.omf.scala.binding.owlapi")))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  //.settings(IMCEPlugin.scalaDocSettings(diagrams=true))
  .settings(
    IMCEKeys.licenseYearOrRange := "2015",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,

    buildInfoPackage := "gov.nasa.jpl.omf.scala.binding.owlapi",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    projectID := {
      val previous = projectID.value
      previous.extra(
        "build.date.utc" -> buildUTCDate.value,
        "artifact.kind" -> "generic.library")
    },

    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,
    git.baseVersion := Versions.version,

    scalacOptions in (Compile,doc) ++= Seq("-diagrams"),

    // include all test artifacts
    publishArtifact in Test := true,

    scalaSource in Test := baseDirectory.value / "test",

    resourceDirectory in Test := baseDirectory.value / "target" / "extracted" / "imce-omf_ontologies",

    IMCEKeys.nexusJavadocRepositoryRestAPIURL2RepositoryName := Map(
      "https://oss.sonatype.org/service/local" -> "releases",
      "https://cae-nexuspro.jpl.nasa.gov/nexus/service/local" -> "JPL"),
    IMCEKeys.pomRepositoryPathRegex := """\<repositoryPath\>\s*([^\"]*)\s*\<\/repositoryPath\>""".r,

    libraryDependencies ++= Seq(

      "gov.nasa.jpl.imce" %% "imce.third_party.scala_graph_libraries"
        % Versions_scala_graph_libraries.version artifacts
        Artifact("imce.third_party.scala_graph_libraries", "zip", "zip", Some("resource"), Seq(), None, Map()),

      "gov.nasa.jpl.imce" %% "imce.third_party.owlapi_libraries"
        % Versions_owlapi_libraries.version artifacts
        Artifact("imce.third_party.owlapi_libraries", "zip", "zip", Some("resource"), Seq(), None, Map()),

      // extra("artifact.kind" -> "omf.ontologies")
      "gov.nasa.jpl.imce.omf" % "imce-omf_ontologies" % Versions_imce_omf_ontologies.version
        % "runtime" artifacts
        Artifact("imce-omf_ontologies", "zip", "zip", Some("resource"), Seq(), None, Map())
    ),

    extractArchives := {
      val deps = libraryDependencies.value
      val up = update.value
      val ver = scalaBinaryVersion.value
      val base = baseDirectory.value
      val s = streams.value

      val artifact2extract = (for {
        dep <- deps
        tuple = (dep.name + "-" + dep.revision, dep.name)
        //if dep.extraAttributes.get("artifact.kind").iterator.contains("omf.ontologies")
        if dep.configurations.iterator.contains("runtime")
      } yield dep.name -> tuple) toMap

      s.log.info(s"artifact2extract: ${artifact2extract.mkString("\n")}")

      val artifactArchive2extractFolder: Map[File, (File, File)] = (for {
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

      artifactArchive2extractFolder foreach { case (archive, (subFolder, extractFolder)) =>
        s.log.info(s"*** Extracting: $archive")
        s.log.info(s"*** Extract to: $extractFolder")
        val files = IO.unzip(archive, extractFolder)
        require(files.nonEmpty)
        require(extractFolder.exists, extractFolder)
      }
    },

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("tiwg", "org.omg.tiwg"),

    compile <<= (compile in Compile) dependsOn extractArchives,

    compile in Test <<= (compile in Test) dependsOn extractArchives,

    unmanagedClasspath in Test += baseDirectory.value / "target" / "extracted" / "imce-omf_ontologies"
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
      "omf-scala-core",
      "gov.nasa.jpl.omf.scala.core",
      Seq(
        "gov.nasa.jpl.imce" %% "gov.nasa.jpl.omf.scala.core"
          % Versions_omf_scala_core.version % "test->compile;compile->compile" artifacts(
          Artifact("gov.nasa.jpl.omf.scala.core"),
          Artifact("gov.nasa.jpl.omf.scala.core", "tests"))
      )
  )

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
      Some(projectName)
    },

    // name the '*-resource.zip' in the same way as other artifacts
    com.typesafe.sbt.packager.Keys.packageName in Universal :=
      normalizedName.value + "_" + scalaBinaryVersion.value + "-" + version.value + "-resource",

    // contents of the '*-resource.zip' to be produced by 'universal:packageBin'
    mappings in Universal in packageBin ++= {
      val dir = baseDirectory.value
      val bin = (packageBin in Compile).value
      val src = (packageSrc in Compile).value
      val doc = (packageDoc in Compile).value
      val binT = (packageBin in Test).value
      val srcT = (packageSrc in Test).value
      val docT = (packageDoc in Test).value

      addIfExists(dir / ".classpath", ".classpath") ++
        addIfExists(dir / "README.md", "README.md") ++
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