
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

val owlapiLibs = taskKey[Seq[Attributed[File]]]("OWLAPI libraries")

val extractArchives
: TaskKey[Unit]
= TaskKey[Unit]("extract-archives", "Extracts ZIP files")

lazy val core =
  Project("omf-scala-binding-owlapi", file("."))
  .enablePlugins(IMCEGitPlugin)
  .settings(dynamicScriptsResourceSettings("gov.nasa.jpl.omf.scala.binding.owlapi"))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2015",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,

    buildInfoPackage := "gov.nasa.jpl.omf.scala.binding.owlapi",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    scalaVersion := Versions.scala,

    scalacOptions in (Compile, compile) += "-explaintypes",

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

    scalacOptions in (Compile,doc) ++= Seq(
      "-diagrams",
      "-doc-title", name.value,
      "-doc-root-content", baseDirectory.value + "/rootdoc.txt"),

    autoAPIMappings := true,

    apiURL := Some(url("https://jpl-imce.github.io/gov.nasa.jpl.omf.scala.binding.owlapi/latest/api/")),

    // include all test artifacts
    publishArtifact in Test := true,

    resourceDirectory in Test := baseDirectory.value / "target" / "extracted" / "imce-omf_ontologies",

    // Needed to transitively get dependencies from the gov.nasa.jpl.imce:imce.third_party.* zip aggregates
    classpathTypes += "zip",

    libraryDependencies ++= Seq(

      "gov.nasa.jpl.imce" %% "imce.third_party.owlapi_libraries"
        % Versions_owlapi_libraries.version artifacts
        Artifact("imce.third_party.owlapi_libraries", "zip", "zip", "resource")
    ),

    extractArchives := {
      val deps = libraryDependencies.value
      val up = update.value
      val ver = scalaBinaryVersion.value
      val base = baseDirectory.value
      val s = streams.value

      val e = base / "target" / "extracted"
      if (e.exists()) {
        s.log.warn(s"*** Skip extracting to existing folder: $e")
      } else {
        // @see https://github.com/jrudolph/sbt-dependency-graph/issues/113
        val g = fromConfigurationReport(
          net.virtualvoid.sbt.graph.DependencyGraphKeys.ignoreMissingUpdate.value.configuration("test").get,
          CrossVersion(scalaVersion.value, scalaBinaryVersion.value)(projectID.value),
          zipFileSelector)

        for {
          module <- g.nodes
          if module.id.name == "gov.nasa.jpl.imce.ontologies.public"
          archive <- module.jarFile
          extractFolder = e / module.id.name
          _ = s.log.info(s"*** Extracting: $archive")
          _ = s.log.info(s"*** Extract to: $extractFolder")
          files = IO.unzip(archive, extractFolder)
          _ = require(files.nonEmpty)
          _ = s.log.info(s"*** Extracted ${files.size} files")
        } yield ()
      }
    },

    owlapiLibs := {
      val s = streams.value
      val owlapiDir = baseDirectory.value / "target" / "owlapi"
      if (owlapiDir.exists()) {
          s.log.warn(s"*** Skip extracting to folder: $owlapiDir")
      } else {
        owlapiDir.mkdirs()
        for {
          c <- update.value.configurations
          if c.configuration == "compile"
          m <- c.modules
          (artifact, archive) <- m.artifacts
          if artifact.name.startsWith("imce.third_party.owlapi_libraries")
          if artifact.extension.contains("zip")
          _ = s.log.info(s"*** Artifact=$archive")
          files = IO.unzip(archive, owlapiDir)
        } yield ()
      }

      val jars = (owlapiDir ** "lib" * "*.jar").get.map(Attributed.blank)
      s.log.warn(s"=> Adding ${jars.size} unmanaged jars for the owlapi")

      jars
    },

    unmanagedJars in Compile ++= owlapiLibs.value,

    scalacOptions in Compile += "-Xexperimental",

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),

    resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases",
    scalacOptions in (Compile, compile) += s"-P:artima-supersafe:config-file:${baseDirectory.value}/project/supersafe.cfg",
    scalacOptions in (Test, compile) += s"-P:artima-supersafe:config-file:${baseDirectory.value}/project/supersafe.cfg",
    scalacOptions in (Compile, doc) += "-Xplugin-disable:artima-supersafe",
    scalacOptions in (Test, doc) += "-Xplugin-disable:artima-supersafe",

    // If it is necessary to disable unit tests.
//    testOptions in Test := Seq(Tests.Filter(s =>
//      !s.endsWith("IMCEFoundationLoadTestFromOWLAPILocalCatalog")
//    )),

    compile in Test := {
      val _ = extractArchives.value
      (compile in Test).value
    },

    // Avoid unresolvable dependencies from old versions of log4j
    libraryDependencies ~= {
      _ map {
        case m if m.organization == "log4j" =>
          m
            .exclude("javax.jms", "jms")
            .exclude("com.sun.jmx", "jmxri")
            .exclude("com.sun.jdmk", "jmxtools")
        case m => m
      }
    },
    dependencyOverrides += "com.fasterxml.jackson.module" % "jackson-module-paranamer" % Versions.spark_jackson % "compile",
    dependencyOverrides += "com.fasterxml.jackson.module" %% "jackson-module-scala" % Versions.spark_jackson % "compile",

    // Remove when
    libraryDependencies ~= {
      _ map {
        case m if m.organization == "gov.nasa.jpl.imce" =>
          m
            .exclude("net.sourceforge.owlapi", "owlapi-distribution")
        case m => m
      }
    },

    classpathTypes += "test-jar",

    unmanagedResourceDirectories in Test += {
      baseDirectory.value / "target" / "extracted" / "gov.nasa.jpl.imce.ontologies.public"
    }
    // for local development assuming that gov.nasa.jpl.imce.ontologies.public is cloned as a peer project, use this:
    // unmanagedClasspath in Test += baseDirectory.value / ".." / "gov.nasa.jpl.imce.ontologies.public"
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "omf-scala-core",
    "gov.nasa.jpl.omf.scala.core",
    Seq(
      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.omf.scala.core"
        % Versions_omf_scala_core.version
        % "compile" withSources() artifacts(
        Artifact("gov.nasa.jpl.omf.scala.core"),
        Artifact("gov.nasa.jpl.omf.scala.core", "zip", "zip", "resource"))
    )
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "omf-scala-core",
    "gov.nasa.jpl.omf.scala.core",
    Some("test->test"),
    Seq(
      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.omf.scala.core"
        % Versions_omf_scala_core.version
        % "test->compile" withSources() artifacts
        Artifact("gov.nasa.jpl.omf.scala.core", "tests")
    )
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "imce-omf_ontologies",
    "gov.nasa.jpl.imce.ontologies.public",
    Seq(
      "gov.nasa.jpl.imce" % "gov.nasa.jpl.imce.ontologies.public"
        % Versions_imce_omf_ontologies.version
        % "test->compile;compile->compile" artifacts
        Artifact("gov.nasa.jpl.imce.ontologies.public", "zip", "zip", "resource")
    )
  )

def dynamicScriptsResourceSettings(projectName: String): Seq[Setting[_]] = {

  import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._

  def addIfExists(f: File, name: String): Seq[(File, String)] =
    if (!f.exists) Seq()
    else Seq((f, name))

  val QUALIFIED_NAME = "^[a-zA-Z][\\w_]*(\\.[a-zA-Z][\\w_]*)*$".r

  Seq(
    // the '*-resource.zip' archive will start from: 'dynamicScripts'
    com.typesafe.sbt.packager.Keys.topLevelDirectory in Universal := None,

    // name the '*-resource.zip' in the same way as other artifacts
    com.typesafe.sbt.packager.Keys.packageName in Universal :=
      normalizedName.value + "_" + scalaBinaryVersion.value + "-" + version.value + "-resource",

    // contents of the '*-resource.zip' to be produced by 'universal:packageBin'
    mappings in Universal ++= {
      val dir = baseDirectory.value
      val bin = (packageBin in Compile).value
      val src = (packageSrc in Compile).value
      val doc = (packageDoc in Compile).value
      val binT = (packageBin in Test).value
      val srcT = (packageSrc in Test).value
      val docT = (packageDoc in Test).value

      (dir * ".classpath").pair(rebase(dir, projectName)) ++
        (dir * "*.md").pair(rebase(dir, projectName)) ++
        (dir / "resources" ***).pair(rebase(dir, projectName)) ++
        addIfExists(bin, projectName + "/lib/" + bin.name) ++
        addIfExists(binT, projectName + "/lib/" + binT.name) ++
        addIfExists(src, projectName + "/lib.sources/" + src.name) ++
        addIfExists(srcT, projectName + "/lib.sources/" + srcT.name) ++
        addIfExists(doc, projectName + "/lib.javadoc/" + doc.name) ++
        addIfExists(docT, projectName + "/lib.javadoc/" + docT.name)
    },

    artifacts += {
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map())
    },
    packagedArtifacts += {
      val p = (packageBin in Universal).value
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    }
  )
}