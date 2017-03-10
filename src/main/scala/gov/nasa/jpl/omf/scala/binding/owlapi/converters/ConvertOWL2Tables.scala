/*
 * Copyright 2015 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package gov.nasa.jpl.omf.scala.binding.owlapi.converters

import java.lang.System
import java.net.URI
import java.nio.file.{Files, Path, Paths}

import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.oml.resolver.OMLTablesResolver
import gov.nasa.jpl.imce.oml.resolver.impl.OMLResolvedFactoryImpl
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.tables.OMFTabularExport
import org.apache.xml.resolver.CatalogManager
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

import scala.{Array, Either, Left, None, Option, Right, Some, StringContext, Unit}
import scala.Predef.{String, genericWrapArray}
import scala.collection.immutable.{Seq, Set}
import scala.util.control.Exception._

import scalaz._, Scalaz._

object ConvertOWL2Tables {

  case class Config
  ( catalogFile: Option[Path] = None,
    iri: Option[IRI] = None,
    outputDir: Option[Path] = None ) {

    def validate: Seq[String] \/ ValidConfig
    = {

      val c = catalogFile.fold[Seq[String] \/ Path](-\/(Seq("Missing catalogFile argument")))(\/-(_))
      val i = iri.fold[Seq[String] \/ IRI](-\/(Seq("Missing iri argument")))(\/-(_))
      val d = outputDir.fold[Seq[String] \/ Path](-\/(Seq("Missing outputDir argument")))(\/-(_))

      val s =
        c.swap.toOption.getOrElse(Seq.empty) ++
        i.swap.toOption.getOrElse(Seq.empty) ++
        d.swap.toOption.getOrElse(Seq.empty)

      if (s.nonEmpty)
        -\/(s)
      else
        for {
          _c <- c
          _i <- i
          _d <- d
          _r = ValidConfig(_c, _i, _d)
        } yield _r
    }
  }

  case class ValidConfig
  ( catalogFile: Path,
    iri: IRI,
    outputDir: Path )

  def parseExistingFilePath(s: String)
  : Either[String,Path]
  = nonFatalCatch[Either[String,Path]]
    .withApply { (t: java.lang.Throwable) => Left[String,Path](t.getMessage) }
    .apply {
      val p = Paths.get(URI.create(s))
      val f = p.toFile
      if (f.exists && f.isFile && f.canRead)
        Right[String,Path](p)
      else
        Left[String,Path]("Non-existent or unreadable file path: " + s)
    }

  def parseExistingDirectoryPath(s: String)
  : Either[String,Path]
  = nonFatalCatch[Either[String,Path]]
    .withApply { (t: java.lang.Throwable) => Left[String,Path](t.getMessage) }
    .apply {
      val p = Paths.get(URI.create(s))
      val d = p.toFile
      if (d.exists && d.isDirectory && d.canRead && d.canWrite)
        Right[String,Path](p)
      else
        Left[String,Path]("Non-existent or non-writeable directory path: " + s)
    }

  def parseNewDirectoryPath(s: String)
  : Either[String,Path]
  = nonFatalCatch[Either[String,Path]]
    .withApply { (t: java.lang.Throwable) => Left[String,Path](t.getMessage) }
    .apply {
      val p = Paths.get(URI.create(s))
      val d = p.toFile
      if (!d.exists)
        java.nio.file.Files.createDirectories(p)
      if (d.isDirectory && d.canRead && d.canWrite)
        Right[String,Path](p)
      else
        Left[String,Path]("Non-existent or non-writeable directory path: " + s)
    }

  def parseIRI(s: String)
  : Either[String,IRI]
  = nonFatalCatch[Either[String,IRI]]
    .withApply { (t: java.lang.Throwable) => Left[String,IRI](t.getMessage) }
    .apply { Right[String,IRI](IRI.create(s)) }

  val configParser = new scopt.OptionParser[Config]("convertOntologies2Tables") {
    head("convert ontologies to OMF tables")

    opt[Either[String,Path]]('c', "catalog")(scopt.Read.reads { parseExistingFilePath })
      .validate(x => x.map(_ => ()))
      .action( (x, c) => x.fold(
        fa = (_: String) => c,
        fb = (p: Path) => c.copy(catalogFile = Some(p))
      ))
      .text("catalog is a required Path property to an existing catalog file")

    opt[Either[String, IRI]]('i', "iri")(scopt.Read.reads { parseIRI })
      .validate(x => x.map(_ => ()))
      .action( (x, c) => x.fold(
        fa = (_: String) => c,
        fb = (i: IRI) => c.copy(iri=Some(i))
      ))
      .text("iri is a required IRI property")

    opt[Either[String,Path]]('o', "out")(scopt.Read.reads { parseNewDirectoryPath })
      .validate(x => x.map(_ => ()))
      .action( (x, c) => x.fold(
        fa = (_: String) => c,
        fb = (p: Path) => c.copy(outputDir = Some(p))
      ))
      .text("out is a required Path property to a writeable directory")

  }

  def main
  (args: Array[String])
  : Unit
  = configParser.parse(args, Config()) match {
    case Some(c) =>
      c.validate match {
        case -\/(errors) =>
          System.err.println(errors.size + " argument errors")
          errors.foreach(System.err.println)
          System.exit(-1)
        case \/-(vc) =>
          System.out.println("Running...")
          runMain(vc)
      }

    case None =>
      System.out.println("None...")
      // bad arguments; error message will have been displayed.
  }

  def runMain(vc: ValidConfig)
  : Unit
  = {
    System.out.println("... creating catalog manager")
    val catalogManager = new CatalogManager()

    System.out.println("... creating OMF Store")
    implicit val omfStore = OWLAPIOMFGraphStore(
      OWLAPIOMFModule.owlAPIOMFModule(catalogManager, withOMFMetadata = false).valueOr { (errors: Set[java.lang.Throwable]) =>
        val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
        throw new scala.IllegalArgumentException(message)
      },
      OWLManager.createOWLOntologyManager())

    implicit val ops = omfStore.ops

    omfStore.catalogIRIMapper.parseCatalog(vc.catalogFile.toUri).valueOr { (errors: Set[java.lang.Throwable]) =>
      val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
      throw new scala.IllegalArgumentException(message)
    }

    if (!vc.outputDir.toFile.exists())
      Files.createDirectories(vc.outputDir)

    val saveCatalog = vc.outputDir.resolve(vc.catalogFile.toFile.getName)
    if (!saveCatalog.toFile.exists())
      Files.copy(vc.catalogFile, saveCatalog)

    val result: Set[java.lang.Throwable] \/ Unit =
      for {
        loaded <- omfStore.loadTerminologyGraph(vc.iri)
        _ = System.out.println("... loaded ontologies")
        (_, m2i) = loaded
        _ = System.out.println(s"... got ${m2i.size} ontologies")

        tablesJsonZip = vc.outputDir.resolve("tables.json.zip").toFile

        _ = java.nio.file.Files.createDirectories(tablesJsonZip.getParentFile.toPath)

        tables = OMFTabularExport.toTables[OWLAPIOMF](m2i.values.to[Set])
        _ <- OMLSpecificationTables
          .saveOMLSpecificationTables(tables, tablesJsonZip)
          .toDisjunction
          .leftMap(Set[java.lang.Throwable](_))
        _ = System.out.println(s"Saved oml.tables in: $tablesJsonZip")

        resolver <- OMLTablesResolver
          .resolve(tables, OMLResolvedFactoryImpl())
          .toDisjunction
          .leftMap(Set[java.lang.Throwable](_))

        queue = resolver.queue
        _ <- if (queue.isEmpty)
          ().right
        else
          Set[java.lang.Throwable](new java.lang.IllegalArgumentException(s"Incomplete resolution:\n$queue")).left

        context = resolver.context

        tboxOrder <- context.topologicalOrder()
          .toDisjunction
          .leftMap(Set[java.lang.Throwable](_))

      } yield {

        tboxOrder.foreach { tbox =>
          System.out.println(s"tbox: ${tbox.iri}")
        }
        ()
      }

    result.fold[Unit](
      (errors: Set[java.lang.Throwable]) => {
        System.err.println(s"${errors.size} errors:")
        errors.foreach { e =>
          System.err.println(e.getMessage)
          e.printStackTrace(System.err)
          System.err.println
        }
      },
      (_:Unit) => {
        System.out.println(s"Saved terminologies in ${vc.outputDir}")
      }
    )
  }
}
