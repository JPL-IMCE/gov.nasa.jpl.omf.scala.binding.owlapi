
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

// @see https://github.com/jrudolph/sbt-dependency-graph/issues/113
def zipFileSelector
( a: Artifact, f: File)
: Boolean
= a.`type` == "zip" || a.extension == "zip"

// @see https://github.com/jrudolph/sbt-dependency-graph/issues/113
def fromConfigurationReport
(report: ConfigurationReport,
 rootInfo: sbt.ModuleID,
 selector: (Artifact, File) => Boolean)
: net.virtualvoid.sbt.graph.ModuleGraph = {
  implicit def id(sbtId: sbt.ModuleID): net.virtualvoid.sbt.graph.ModuleId
  = net.virtualvoid.sbt.graph.ModuleId(sbtId.organization, sbtId.name, sbtId.revision)

  def moduleEdges(orgArt: OrganizationArtifactReport)
  : Seq[(net.virtualvoid.sbt.graph.Module, Seq[net.virtualvoid.sbt.graph.Edge])]
  = {
    val chosenVersion = orgArt.modules.find(!_.evicted).map(_.module.revision)
    orgArt.modules.map(moduleEdge(chosenVersion))
  }

  def moduleEdge(chosenVersion: Option[String])(report: ModuleReport)
  : (net.virtualvoid.sbt.graph.Module, Seq[net.virtualvoid.sbt.graph.Edge]) = {
    val evictedByVersion = if (report.evicted) chosenVersion else None

    val jarFile = report.artifacts.find(selector.tupled).map(_._2)
    (net.virtualvoid.sbt.graph.Module(
      id = report.module,
      license = report.licenses.headOption.map(_._1),
      evictedByVersion = evictedByVersion,
      jarFile = jarFile,
      error = report.problem),
      report.callers.map(caller ⇒ net.virtualvoid.sbt.graph.Edge(caller.caller, report.module)))
  }

  val (nodes, edges) = report.details.flatMap(moduleEdges).unzip
  val root = net.virtualvoid.sbt.graph.Module(rootInfo)

  net.virtualvoid.sbt.graph.ModuleGraph(root +: nodes, edges.flatten)
}

lazy val core =
  Project("omf-scala-binding-owlapi", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)
  .settings(dynamicScriptsResourceSettings(Some("gov.nasa.jpl.omf.scala.binding.owlapi")))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2015",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,

    buildInfoPackage := "gov.nasa.jpl.omf.scala.binding.owlapi",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    scalacOptions in (Compile,doc) ++= Seq(
      "-diagrams",
      "-doc-title", name.value,
      "-doc-root-content", baseDirectory.value + "/rootdoc.txt"),

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

    libraryDependencies ++= Seq(

      "gov.nasa.jpl.imce" %% "imce.third_party.scala_graph_libraries"
        % Versions_scala_graph_libraries.version artifacts
        Artifact("imce.third_party.scala_graph_libraries", "zip", "zip", Some("resource"), Seq(), None, Map()),

      "gov.nasa.jpl.imce" %% "imce.third_party.owlapi_libraries"
        % Versions_owlapi_libraries.version artifacts
        Artifact("imce.third_party.owlapi_libraries", "zip", "zip", Some("resource"), Seq(), None, Map())
    ),

    extractArchives := {
      val deps = libraryDependencies.value
      val up = update.value
      val ver = scalaBinaryVersion.value
      val base = baseDirectory.value
      val s = streams.value

      // @see https://github.com/jrudolph/sbt-dependency-graph/issues/113
      val g = fromConfigurationReport(
        net.virtualvoid.sbt.graph.DependencyGraphKeys.ignoreMissingUpdate.value.configuration("test").get,
        CrossVersion(scalaVersion.value, scalaBinaryVersion.value)(projectID.value),
        zipFileSelector)

      for {
        module <- g.nodes
        if module.id.name == "gov.nasa.jpl.imce.ontologies"
        archive <- module.jarFile
        extractFolder = base / "target" / "extracted" / module.id.name
        _ = s.log.info(s"*** Extracting: $archive")
        _ = s.log.info(s"*** Extract to: $extractFolder")
        files = IO.unzip(archive, extractFolder)
        _ = require(files.nonEmpty)
        _ = s.log.info(s"*** Extracted ${files.size} files")
      } yield ()

    },

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("tiwg", "org.omg.tiwg"),

    compile <<= (compile in Compile) dependsOn extractArchives,

    compile in Test <<= (compile in Test) dependsOn extractArchives,

    unmanagedClasspath in Test += baseDirectory.value / "target" / "extracted" / "gov.nasa.jpl.imce.ontologies"
    // for local development, use this instead:
    //unmanagedClasspath in Test += baseDirectory.value / ".." / "gov.nasa.jpl.imce.ontologies" / "gov.nasa.jpl.imce.ontologies/"
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "omf-scala-core",
    "gov.nasa.jpl.omf.scala.core",
    Seq(
      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.omf.scala.core"
        % Versions_omf_scala_core.version
        % "compile" artifacts
        Artifact("gov.nasa.jpl.omf.scala.core", "zip", "zip", Some("resource"), Seq(), None, Map()),
      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.omf.scala.core"
        % Versions_omf_scala_core.version
        % "test->compile;compile->compile" artifacts(
        Artifact("gov.nasa.jpl.omf.scala.core"),
        Artifact("gov.nasa.jpl.omf.scala.core", "tests"))
    )
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "imce-omf_ontologies",
    "gov.nasa.jpl.imce.ontologies",
    Seq(
      "gov.nasa.jpl.imce" % "gov.nasa.jpl.imce.ontologies"
        % Versions_imce_omf_ontologies.version
        % "test->compile;compile->compile" artifacts
        Artifact("gov.nasa.jpl.imce.ontologies", "zip", "zip", Some("resource"), Seq(), None, Map())
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