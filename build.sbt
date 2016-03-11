
import sbt.Keys._
import sbt._
import scala.language.postfixOps

import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._

import java.io.File

useGpg := true

developers := List(
  Developer(
    id="rouquett",
    name="Nicolas F. Rouquette",
    email="nicolas.f.rouquette@jpl.nasa.gov",
    url=url("https://gateway.jpl.nasa.gov/personal/rouquett/default.aspx")))

resolvers ++= {
  if (git.gitUncommittedChanges.value)
    Seq[Resolver](Resolver.mavenLocal)
  else
    Seq.empty[Resolver]
}

import scala.io.Source
import scala.util.control.Exception._

def docSettings(diagrams:Boolean): Seq[Setting[_]] =
  Seq(
    sources in (Compile,doc) <<= (git.gitUncommittedChanges, sources in (Compile,compile)) map {
      (uncommitted, compileSources) =>
        if (uncommitted)
          Seq.empty
        else
          compileSources
    },

    sources in (Test,doc) <<= (git.gitUncommittedChanges, sources in (Test,compile)) map {
      (uncommitted, testSources) =>
        if (uncommitted)
          Seq.empty
        else
          testSources
    },

    scalacOptions in (Compile,doc) ++=
      (if (diagrams)
        Seq("-diagrams")
      else
        Seq()
        ) ++
        Seq(
          "-doc-title", name.value,
          "-doc-root-content", baseDirectory.value + "/rootdoc.txt"
        ),
    autoAPIMappings := ! git.gitUncommittedChanges.value,
    apiMappings <++=
      ( git.gitUncommittedChanges,
        dependencyClasspath in Compile in doc,
        IMCEKeys.nexusJavadocRepositoryRestAPIURL2RepositoryName,
        IMCEKeys.pomRepositoryPathRegex,
        streams ) map { (uncommitted, deps, repoURL2Name, repoPathRegex, s) =>
        if (uncommitted)
          Map[File, URL]()
        else
          (for {
            jar <- deps
            url <- jar.metadata.get(AttributeKey[ModuleID]("moduleId")).flatMap { moduleID =>
              val urls = for {
                (repoURL, repoName) <- repoURL2Name
                (query, match2publishF) = IMCEPlugin.nexusJavadocPOMResolveQueryURLAndPublishURL(
                  repoURL, repoName, moduleID)
                url <- nonFatalCatch[Option[URL]]
                  .withApply { (_: java.lang.Throwable) => None }
                  .apply({
                    val conn = query.openConnection.asInstanceOf[java.net.HttpURLConnection]
                    conn.setRequestMethod("GET")
                    conn.setDoOutput(true)
                    repoPathRegex
                      .findFirstMatchIn(Source.fromInputStream(conn.getInputStream).getLines.mkString)
                      .map { m =>
                        val javadocURL = match2publishF(m)
                        s.log.info(s"Javadoc for: $moduleID")
                        s.log.info(s"= mapped to: $javadocURL")
                        javadocURL
                      }
                  })
              } yield url
              urls.headOption
            }
          } yield jar.data -> url).toMap
      }
  )

lazy val core =
  Project("omf-scala-binding-owlapi", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)
  .settings(dynamicScriptsResourceSettings(Some("gov.nasa.jpl.omf.scala.binding.owlapi")))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  //.settings(IMCEPlugin.scalaDocSettings(diagrams=false))
  .settings(docSettings(diagrams=false))
  .settings(
    IMCEKeys.licenseYearOrRange := "2014-2016",
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

    // include all test artifacts
    publishArtifact in Test := true,
    scalaSource in Compile := baseDirectory.value / "src",

    scalaSource in Test := baseDirectory.value / "test",

    resourceDirectory in Test := baseDirectory.value / "target" / "extracted" / "imce-omf_ontologies",

    IMCEKeys.nexusJavadocRepositoryRestAPIURL2RepositoryName := Map(
      "https://oss.sonatype.org/service/local" -> "releases",
      "https://cae-nexuspro.jpl.nasa.gov/nexus/service/local" -> "JPL"),
    IMCEKeys.pomRepositoryPathRegex := """\<repositoryPath\>\s*([^\"]*)\s*\<\/repositoryPath\>""".r,

    libraryDependencies ++= Seq(

      "gov.nasa.jpl.imce.thirdParty" %% "scala-graph-libraries"
        % Versions_scala_graph_libraries.version artifacts
        Artifact("scala-graph-libraries", "zip", "zip", Some("resource"), Seq(), None, Map()),

      "gov.nasa.jpl.imce.thirdParty" %% "owlapi-libraries"
        % Versions_owlapi_libraries.version artifacts
        Artifact("owlapi-libraries", "zip", "zip", Some("resource"), Seq(), None, Map()),

      // extra("artifact.kind" -> "omf.ontologies")
      "gov.nasa.jpl.imce.omf" % "imce-omf_ontologies" % Versions_imce_omf_ontologies.version
        % "runtime" artifacts
        Artifact("imce-omf_ontologies", "zip", "zip", Some("resource"), Seq(), None, Map())
    ),

    extractArchives <<=
      (libraryDependencies, update, scalaBinaryVersion, baseDirectory, streams)
      .map { (deps, up, ver, base, s) =>
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

    compile <<= (compile in Compile) dependsOn extractArchives,

    compile in Test <<= (compile in Test) dependsOn extractArchives,

    unmanagedClasspath in Test += baseDirectory.value / "target" / "extracted" / "imce-omf_ontologies"
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "omf-scala-core",
    "gov.nasa.jpl.omf.scala.core",
    Some("compile->compile;test->test"),
    Seq(
      "gov.nasa.jpl.imce.omf" %% "omf-scala-core"
        % Versions_omf_scala_core.version % "compile" withSources() withJavadoc() artifacts
        Artifact("omf-scala-core", "zip", "zip", Some("resource"), Seq(), None, Map()),

      "gov.nasa.jpl.imce.omf" %% "omf-scala-core" % Versions_omf_scala_core.version %
        "test" classifier "tests" withSources() withJavadoc() artifacts(
        Artifact.classified("omf-scala-core", "tests-sources"),
        Artifact.classified("omf-scala-core", "tests-javadoc"))
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
    mappings in packageBin in Universal <++= (
      baseDirectory,
      packageBin in Compile,
      packageSrc in Compile,
      packageDoc in Compile,
      packageBin in Test,
      packageSrc in Test,
      packageDoc in Test) map {
      (base, bin, src, doc, binT, srcT, docT) =>
        val dir = base / "svn" / "org.omg.oti"
        (dir ** "*.md").pair(relativeTo(dir)) ++
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