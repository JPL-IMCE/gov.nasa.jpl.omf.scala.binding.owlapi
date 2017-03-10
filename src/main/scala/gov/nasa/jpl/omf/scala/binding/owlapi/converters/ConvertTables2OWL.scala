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

import gov.nasa.jpl.imce.oml.tables.{ClosedWorldDesignations, OMLSpecificationTables, OpenWorldDefinitions}
import gov.nasa.jpl.imce.oml.resolver._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{ImmutableTerminologyBox, MutableTerminologyBox, MutableTerminologyGraph}
import gov.nasa.jpl.omf.scala.core.{OMFError, RelationshipCharacteristics, TerminologyKind}
import org.apache.xml.resolver.CatalogManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.apibinding.OWLManager

import scala.{Array, Either, Int, Left, None, Option, Right, Some, StringContext, Tuple2, Unit}
import scala.Predef.{ArrowAssoc, String, genericWrapArray}
import scala.util.{Failure, Success, Try}
import scala.collection.immutable._
import scala.util.control.Exception._
import scalaz._
import Scalaz._

object  ConvertTables2OWL {

  case class Config
  ( catalogFile: Option[Path] = None,
    tablesFile: Option[Path] = None,
    outputDir: Option[Path] = None ) {

    def validate: Seq[String] \/ ValidConfig
    = {

      val c = catalogFile.fold[Seq[String] \/ Path](-\/(Seq("Missing catalogFile argument")))(\/-(_))
      val t = tablesFile.fold[Seq[String] \/ Path](-\/(Seq("Missing tablesFile argument")))(\/-(_))
      val d = outputDir.fold[Seq[String] \/ Path](-\/(Seq("Missing outputDir argument")))(\/-(_))

      val s =
        c.swap.toOption.getOrElse(Seq.empty) ++
          t.swap.toOption.getOrElse(Seq.empty) ++
          d.swap.toOption.getOrElse(Seq.empty)

      if (s.nonEmpty)
        -\/(s)
      else
        for {
          _c <- c
          _t <- t
          _d <- d
          _r = ValidConfig(_c, _t, _d)
        } yield _r
    }
  }

  case class ValidConfig
  ( catalogFile: Path,
    tablesFile: Path,
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

  val configParser = new scopt.OptionParser[Config]("convertTables2Ontologies") {
    head("convert OMF tables to Ontologies")

    opt[Either[String,Path]]('c', "catalog")(scopt.Read.reads { parseExistingFilePath })
      .validate(x => x.map(_ => ()))
      .action( (x, c) => x.fold(
        fa = (_: String) => c,
        fb = (p: Path) => c.copy(catalogFile = Some(p))
      ))
      .text("catalog is a required Path property to an existing catalog file")

    opt[Either[String, Path]]('t', "tables")(scopt.Read.reads { parseExistingFilePath })
      .validate(x => x.map(_ => ()))
      .action( (x, c) => x.fold(
        fa = (_: String) => c,
        fb = (p: Path) => c.copy(tablesFile = Some(p))
      ))
      .text("tables is a required Path property to an existing *.json.zip archive of oml.tables")

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

  implicit def either2tryUnit[X]
  (x: Set[java.lang.Throwable] \/ X)
  : Try[Unit]
  = x match {
    case -\/(errors) =>
      Failure(OMFError.omfException(s"error: " + errors.head.getMessage, errors))
    case \/-(_) =>
      Success(())
  }

  def runMain(vc: ValidConfig)
  : Unit
  = {
    if (!vc.outputDir.toFile.exists())
      Files.createDirectories(vc.outputDir)

    val saveCatalog = vc.outputDir.resolve(vc.catalogFile.toFile.getName)
    if (!saveCatalog.toFile.exists())
      Files.copy(vc.catalogFile, saveCatalog)

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

    omfStore.catalogIRIMapper.parseCatalog(saveCatalog.toUri).valueOr { (errors: Set[java.lang.Throwable]) =>
      val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
      throw new scala.IllegalArgumentException(message)
    }

    import OMLOps._

    val result
    : Try[Unit]
    = for {
      tables <- OMLSpecificationTables.loadOMLSpecificationTables(vc.tablesFile.toFile)
      factory = impl.OMLResolvedFactoryImpl()
      resolver <- OMLTablesResolver.resolve(tables, factory)
      _ <- if (resolver.queue.isEmpty)
        Success(())
      else
        Failure(OMFError.omfError(
          s"Failed to resolve oml.tables from ${vc.tablesFile}:\n+${resolver.queue}"))

      tboxes <- resolver.context.topologicalOrder().map(_.to[Seq].reverse)

      // Terminologies
      tbox2ont <- tboxes.foldLeft[Try[Seq[(api.TerminologyBox, MutableTerminologyBox)]]](Success(Seq.empty)) {
        case (acc, tbox: api.TerminologyBox) =>
          acc.flatMap { t2mt =>
            val k: TerminologyKind = tbox.kind match {
              case OpenWorldDefinitions =>
                TerminologyKind.isDefinition
              case ClosedWorldDesignations =>
                TerminologyKind.isDesignation
            }
            val i = IRI.create(tbox.iri)
            val m = tbox match {
              case tgraph: api.TerminologyGraph =>
                ops.makeTerminologyGraph(tgraph.uuid, tgraph.name, i, k)
              case tbundle: api.Bundle =>
                ops.makeBundle(tbundle.uuid, tbundle.name, i, k)
            }
            m match {
              case -\/(errors) =>
                Failure(OMFError.omfException("error", errors))
              case \/-(mg) =>
                Success(t2mt :+ (tbox -> mg))
            }
          }
        case (acc, _) =>
          acc
      }

      // Atomic terms
      _ <- tbox2ont.foldLeft[Try[Unit]](Success(())) { case (acc, (tbox, mg)) =>
        for {
          _ <- acc
          _ <- tbox.aspects().foldLeft[Try[Unit]](Success(())) { case (acc1, a) =>
            acc1.flatMap { _ =>
              ops.addAspect(mg, a.name).flatMap { a1 =>
                if (a.uuid == ops.getTermUUID(a1))
                  a1.right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table aspect $a conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(a1)}")).left
              }
            }
          }
          _ <- tbox.concepts().foldLeft[Try[Unit]](Success(())) { case (acc1, c) =>
            acc1.flatMap { _ =>
              ops.addConcept(mg, c.name).flatMap { c1 =>
                if (c.uuid == ops.getTermUUID(c1))
                  c1.right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table concept $c conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(c1)}")).left
              }
            }
          }
          _ <- tbox.dataranges().foldLeft[Try[Unit]](Success(())) { case (acc1, dr) =>

            // TODO add cases for all the different kinds of data ranges
            acc1.flatMap { _ =>
              ops.addScalarDataType(mg, dr.name).flatMap { sc1 =>
                if (dr.uuid == ops.getTermUUID(sc1))
                  sc1.right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table scalar $dr conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(sc1)}")).left
              }
            }
          }
          _ <- tbox.structures().foldLeft[Try[Unit]](Success(())) { case (acc1, st) =>
            acc1.flatMap { _ =>
              ops.addStructuredDataType(mg, st.name).flatMap { st1 =>
                if (st.uuid == ops.getTermUUID(st1))
                  st1.right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table structure $st conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(st1)}")).left
              }
            }
          }
        } yield ()
      }

      tbox2ontMap = tbox2ont.toMap

      // TerminologyAxiom relationships(extensions)
      _ <- resolver.context.g.toOuterEdges.foldLeft[Try[Unit]](Success(())) {
        case (acc,
        TerminologyEdge(
        (es: api.TerminologyBox, et: api.TerminologyBox),
        tx: api.TerminologyExtensionAxiom)) =>
          for {
            _ <- acc
            s <- tbox2ontMap.get(es)
              .fold[Try[MutableTerminologyBox]](
              Failure(OMFError.omfError(s"invalid edge source: $es")))(Success(_))
            t <- tbox2ontMap.get(et)
              .fold[Try[MutableTerminologyBox]](
              Failure(OMFError.omfError(s"invalid edge target: $et")))(Success(_))
            _ <- either2tryUnit(ops.addTerminologyExtension(extendingG = s, extendedG = t).flatMap { ax =>
              if (tx.uuid == ops.getTerminologyAxiomUUID(ax))
                ax.right
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"OMF Schema table TerminologyExtensionAxiom $tx conversion " +
                    s"results in UUID mismatch: ${ops.getTerminologyAxiomUUID(ax)}")).left
            })
          } yield ()
        case (acc, _) =>
          acc
      }

      // TerminologyAxiom relationships(nestings)
      _ <- resolver.context.g.toOuterEdges.foldLeft[Try[Unit]](Success(())) {
        case (acc,
        TerminologyEdge(
        (es: api.TerminologyBox, et: api.TerminologyBox),
        tx: api.TerminologyNestingAxiom)) =>
          for {
            _ <- acc
            s <- tbox2ontMap.get(es)
              .fold[Try[MutableTerminologyGraph]](
              Failure(OMFError.omfError(s"invalid edge source: $es"))) {
              case mg: MutableTerminologyGraph =>
                Success(mg)
              case _ =>
                Failure(OMFError.omfError(s"Invalid edge source: $es is not a graph!"))
            }
            t <- tbox2ontMap.get(et)
              .fold[Try[MutableTerminologyBox]](
              Failure(OMFError.omfError(s"invalid edge target: $et")))(Success(_))
            _ <- ops.lookupConcept(t, IRI.create(tx.nestingContext.iri), recursively = false)
              .fold[Try[Unit]](Failure(OMFError.omfError(
              s"Invalid terminology nesting axiom: " +
                s"Unresolved nesting context: ${tx.nestingContext}"))
            ) { nc =>
              ops.addNestedTerminology(nestedGraph = s, nestingGraph = t, nestingContext = nc).flatMap { ax =>
                if (tx.uuid == ops.getTerminologyAxiomUUID(ax))
                  ax.right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table TerminologyNestingAxiom $tx conversion " +
                      s"results in UUID mismatch: ${ops.getTerminologyAxiomUUID(ax)}")).left
              }
            }
          } yield ()
        case (acc, _) =>
          acc
      }

      // TerminologyAxiom relationships(designations)
      _ <- resolver.context.g.toOuterEdges.foldLeft[Try[Unit]](Success(())) {
        case (acc,
        TerminologyEdge(
        (es: api.TerminologyBox, et: api.TerminologyBox),
        tx: api.ConceptDesignationTerminologyAxiom)) =>
          for {
            _ <- acc
            s <- tbox2ontMap.get(es)
              .fold[Try[MutableTerminologyGraph]](
              Failure(OMFError.omfError(s"invalid edge source: $es"))) {
              case mg: MutableTerminologyGraph =>
                Success(mg)
              case _ =>
                Failure(OMFError.omfError(s"Invalid edge source: $es is not a graph!"))
            }
            t <- tbox2ontMap.get(et)
              .fold[Try[MutableTerminologyBox]](
              Failure(OMFError.omfError(s"invalid edge target: $et")))(Success(_))
            _ <- ops.lookupConcept(t, IRI.create(tx.designatedConcept.iri), recursively = false)
              .fold[Try[Unit]](Failure(OMFError.omfError(
              s"Invalid concept designation terminology axiom: " +
                s"Unresolved designated concept: ${tx.designatedConcept}"))) { nc =>
              ops.addEntityConceptDesignationTerminologyAxiom(
                graph = s, entityConceptDesignation = nc, designationTerminologyGraph = t)
                .flatMap { ax =>
                  if (tx.uuid == ops.getTerminologyAxiomUUID(ax))
                    ax.right
                  else
                    Set[java.lang.Throwable](OMFError.omfError(
                      s"OMF Schema table ConceptDesignationTerminologyAxiom $tx conversion " +
                        s"results in UUID mismatch: ${ops.getTerminologyAxiomUUID(ax)}")).left
                }
            }
          } yield ()
        case (acc, _) =>
          acc
      }

      // Relational terms
      _ <- tbox2ont.foldLeft[Try[Unit]](Success(())) { case (acc, (tbox, mg)) =>
        for {
          _ <- acc
          _ = System.out.println(s"... Relational terms: ${tbox.iri}")
          _ <- convertDataRanges(tbox, mg, tbox.dataranges)
          _ <- convertReifiedRelationships(tbox, mg, tbox.reifiedRelationships)
          _ <- tbox2ont.foldLeft[Try[Unit]](Success(())) { case (acc, (tbox, mg)) =>
            for {
              _ <- acc
              _ <- tbox.unreifiedRelationships.foldLeft[Try[Unit]](Success(())) { case (acc1, ur) =>
                acc1.flatMap { _ =>
                  (ops.lookupEntity(mg, IRI.create(ur.source.iri), recursively = true),
                    ops.lookupEntity(mg, IRI.create(ur.target.iri), recursively = true)) match {
                    case (Some(s), Some(t)) =>
                      either2tryUnit(ops
                        .addUnreifiedRelationship(mg, s, t,
                          Iterable() ++
                            (if (ur.isAsymmetric) Iterable(RelationshipCharacteristics.isAsymmetric) else Iterable()) ++
                            (if (ur.isEssential) Iterable(RelationshipCharacteristics.isEssential) else Iterable()) ++
                            (if (ur.isFunctional) Iterable(RelationshipCharacteristics.isFunctional) else Iterable()) ++
                            (if (ur.isInverseEssential) Iterable(RelationshipCharacteristics.isInverseEssential) else Iterable()) ++
                            (if (ur.isInverseFunctional) Iterable(RelationshipCharacteristics.isInverseFunctional) else Iterable()) ++
                            (if (ur.isIrreflexive) Iterable(RelationshipCharacteristics.isIrreflexive) else Iterable()) ++
                            (if (ur.isReflexive) Iterable(RelationshipCharacteristics.isReflexive) else Iterable()) ++
                            (if (ur.isSymmetric) Iterable(RelationshipCharacteristics.isSymmetric) else Iterable()) ++
                            (if (ur.isTransitive) Iterable(RelationshipCharacteristics.isTransitive) else Iterable()),
                          ur.name)
                        .flatMap { our =>
                          if (ur.uuid == ops.getTermUUID(our)) ().right
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"OMF Schema table UnreifiedRelationship $ur conversion " +
                                s"results in UUID mismatch: ${ops.getTermUUID(our)}")).left
                        })
                    case (_, _) =>
                      Failure(OMFError.omfError(s"Unresolved unreifiedRelationship: $ur"))
                  }
                }
              }
            } yield ()
          }
        } yield ()
      }

      // DataRelationships
      _ <- tbox2ont.foldLeft[Try[Unit]](Success(())) { case (acc, (tbox, mg)) =>
        for {
          _ <- acc
          _ = System.out.println(s"... DataRelationships: ${tbox.iri}")
          _ <- tbox.boxStatements.foldLeft[Try[Unit]](Success(())) {
            case (acc1, dr: api.EntityScalarDataProperty) =>
              acc1.flatMap { _ =>
                (ops.lookupEntity(mg, IRI.create(dr.source.iri), recursively = true),
                  ops.lookupDataRange(mg, IRI.create(dr.target.iri), recursively = true)) match {
                  case (Some(s), Some(t)) =>
                    either2tryUnit(ops
                      .addEntityScalarDataProperty(mg, s, t, dr.name, dr.isIdentityCriteria)
                      .flatMap { odr =>
                        if (dr.uuid == ops.getTermUUID(odr)) ().right
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"OMF Schema table EntityScalarDataProperty $dr conversion " +
                              s"results in UUID mismatch: ${ops.getTermUUID(odr)}")).left
                      })
                  case (_, _) =>
                    Failure(OMFError.omfError(s"Unresolved EntityScalarDataProperty: $dr"))
                }
              }
            case (acc1, dr: api.EntityStructuredDataProperty) =>
              acc1.flatMap { _ =>
                (ops.lookupEntity(mg, IRI.create(dr.source.iri), recursively = true),
                  ops.lookupStructure(mg, IRI.create(dr.target.iri), recursively = true)) match {
                  case (Some(s), Some(t)) =>
                    either2tryUnit(ops
                      .addEntityStructuredDataProperty(mg, s, t, dr.name, dr.isIdentityCriteria)
                      .flatMap { odr =>
                        if (dr.uuid == ops.getTermUUID(odr)) ().right
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"OMF Schema table EntityStructuredDataProperty $dr conversion " +
                              s"results in UUID mismatch: ${ops.getTermUUID(odr)}")).left
                      })
                  case (_, _) =>
                    Failure(OMFError.omfError(s"Unresolved EntityStructuredDataProperty: $dr"))
                }
              }
            case (acc1, dr: api.ScalarDataProperty) =>
              acc1.flatMap { _ =>
                (ops.lookupStructure(mg, IRI.create(dr.source.iri), recursively = true),
                  ops.lookupDataRange(mg, IRI.create(dr.target.iri), recursively = true)) match {
                  case (Some(s), Some(t)) =>
                    either2tryUnit(ops
                      .addScalarDataProperty(mg, s, t, dr.name)
                      .flatMap { odr =>
                        if (dr.uuid == ops.getTermUUID(odr)) ().right
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"OMF Schema table ScalarDataProperty $dr conversion " +
                              s"results in UUID mismatch: ${ops.getTermUUID(odr)}")).left
                      })
                  case (_, _) =>
                    Failure(OMFError.omfError(s"Unresolved ScalarDataProperty: $dr"))
                }
              }
            case (acc1, dr: api.StructuredDataProperty) =>
              acc1.flatMap { _ =>
                (ops.lookupStructure(mg, IRI.create(dr.source.iri), recursively = true),
                  ops.lookupStructure(mg, IRI.create(dr.target.iri), recursively = true)) match {
                  case (Some(s), Some(t)) =>
                    either2tryUnit(ops
                      .addStructuredDataProperty(mg, s, t, dr.name)
                      .flatMap { odr =>
                        if (dr.uuid == ops.getTermUUID(odr)) ().right
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"OMF Schema table StructuredDataProperty $dr conversion " +
                              s"results in UUID mismatch: ${ops.getTermUUID(odr)}")).left
                      })
                  case (_, _) =>
                    Failure(OMFError.omfError(s"Unresolved StructuredDataProperty: $dr"))
                }
              }
            case (acc1, _) =>
              acc1
          }
        } yield ()
      }

      // ScalarOneOfLiteralAxioms
      _ <- tbox2ont.foldLeft[Try[Unit]](Success(())) { case (acc, (tbox, mg)) =>
        for {
          _ <- acc
          _ = System.out.println(s"... ScalarOneOfLiteralAxioms: ${tbox.iri}")
          _ <- tbox.boxStatements.foldLeft[Try[Unit]](Success(())) {
            case (acc1, ax: api.ScalarOneOfLiteralAxiom) =>
              ops.lookupDataRange(mg, IRI.create(ax.axiom.iri), recursively = true)
                .fold[Try[Unit]](Success(())) {
                case dr: OWLAPIOMF#ScalarOneOfRestriction =>
                  ops
                    .addScalarOneOfLiteralAxiom(mg, dr, ax.value)
                    .flatMap { oax =>
                      if (ax.uuid == ops.getAxiomUUID(oax)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table ScalarOneOfLiteralAxiom $ax conversion " +
                            s"results in UUID mismatch: ${ops.getAxiomUUID(oax)}")).left
                    }
                case _ =>
                  Failure(OMFError.omfError(s"Unresolved ScalarOneOfLiteralAxiom: $ax"))
              }
            case (acc1, _) =>
              acc1
          }
        } yield ()
      }

      // - TermAxioms
      // -- EntityRestrictionAxioms
      _ <- tbox2ont.foldLeft[Try[Unit]](Success(())) { case (acc, (tbox, mg)) =>
        for {
          _ <- acc
          _ = System.out.println(s"... EntityRestrictionAxioms: ${tbox.iri}")
          _ <- tbox.boxStatements.foldLeft[Try[Unit]](Success(())) {
            case (acc1, ax: api.EntityExistentialRestrictionAxiom) =>
              (ops.lookupEntity(mg, IRI.create(ax.restrictedDomain.iri), recursively = true),
                ops.lookupEntity(mg, IRI.create(ax.restrictedRange.iri), recursively = true),
                ops.lookupReifiedRelationship(mg, IRI.create(ax.restrictedRelation.iri), recursively = true)) match {
                case (Some(s), Some(t), Some(rr)) =>
                  ops
                    .addEntityExistentialRestrictionAxiom(mg, sub = s, rel = rr, range = t)
                    .flatMap { oax =>
                      if (ax.uuid == ops.getAxiomUUID(oax)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table EntityExistentialRestrictionAxiom $ax conversion " +
                            s"results in UUID mismatch: ${ops.getAxiomUUID(oax)}")).left
                    }
                case (_, _, _) =>
                  Failure(OMFError.omfError(s"Unresolved EntityExistentialRestrictionAxiom: $ax"))
              }
            case (acc1, ax: api.EntityUniversalRestrictionAxiom) =>
              (ops.lookupEntity(mg, IRI.create(ax.restrictedDomain.iri), recursively = true),
                ops.lookupEntity(mg, IRI.create(ax.restrictedRange.iri), recursively = true),
                ops.lookupReifiedRelationship(mg, IRI.create(ax.restrictedRelation.iri), recursively = true)) match {
                case (Some(s), Some(t), Some(rr)) =>
                  ops
                    .addEntityUniversalRestrictionAxiom(mg, sub = s, rel = rr, range = t)
                    .flatMap { oax =>
                      if (ax.uuid == ops.getAxiomUUID(oax)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table EntityUniversalRestrictionAxiom $ax conversion " +
                            s"results in UUID mismatch: ${ops.getAxiomUUID(oax)}")).left
                    }
                case (_, _, _) =>
                  Failure(OMFError.omfError(s"Unresolved EntityUniversalRestrictionAxiom: $ax"))
              }
            case (acc1, _) =>
              acc1
          }
        } yield ()
      }

      // -- EntityScalarDataPropertyRestrictionAxioms
      _ <- tbox2ont.foldLeft[Try[Unit]](Success(())) { case (acc, (tbox, mg)) =>
        for {
          _ <- acc
          _ = System.out.println(s"... EntityScalarDataPropertyRestrictionAxioms: ${tbox.iri}")
          _ <- tbox.boxStatements.foldLeft[Try[Unit]](Success(())) {
            case (acc1, ax: api.EntityScalarDataPropertyExistentialRestrictionAxiom) =>
              (ops.lookupEntity(mg, IRI.create(ax.restrictedEntity.iri), recursively = true),
                ops.lookupDataRange(mg, IRI.create(ax.scalarRestriction.iri), recursively = true),
                ops.lookupEntityScalarDataProperty(mg, IRI.create(ax.scalarProperty.iri), recursively = true)) match {
                case (Some(s), Some(t), Some(dp)) =>
                  ops
                    .addEntityScalarDataPropertyExistentialRestrictionAxiom(mg, restrictedEntity = s, scalarProperty = dp, range = t)
                    .flatMap { oax =>
                      if (ax.uuid == ops.getAxiomUUID(oax)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table EntityScalarDataPropertyExistentialRestrictionAxiom $ax conversion " +
                            s"results in UUID mismatch: ${ops.getAxiomUUID(oax)}")).left
                    }
                case (_, _, _) =>
                  Failure(OMFError.omfError(s"Unresolved EntityScalarDataPropertyExistentialRestrictionAxiom: $ax"))
              }
            case (acc1, ax: api.EntityScalarDataPropertyUniversalRestrictionAxiom) =>
              (ops.lookupEntity(mg, IRI.create(ax.restrictedEntity.iri), recursively = true),
                ops.lookupDataRange(mg, IRI.create(ax.scalarRestriction.iri), recursively = true),
                ops.lookupEntityScalarDataProperty(mg, IRI.create(ax.scalarProperty.iri), recursively = true)) match {
                case (Some(s), Some(t), Some(dp)) =>
                  ops
                    .addEntityScalarDataPropertyUniversalRestrictionAxiom(mg, restrictedEntity = s, scalarProperty = dp, range = t)
                    .flatMap { oax =>
                      if (ax.uuid == ops.getAxiomUUID(oax)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table EntityScalarDataPropertyUniversalRestrictionAxiom $ax conversion " +
                            s"results in UUID mismatch: ${ops.getAxiomUUID(oax)}")).left
                    }
                case (_, _, _) =>
                  Failure(OMFError.omfError(s"Unresolved EntityScalarDataPropertyUniversalRestrictionAxiom: $ax"))
              }
            case (acc1, ax: api.EntityScalarDataPropertyParticularRestrictionAxiom) =>
              (ops.lookupEntity(mg, IRI.create(ax.restrictedEntity.iri), recursively = true),
                ops.lookupEntityScalarDataProperty(mg, IRI.create(ax.scalarProperty.iri), recursively = true)) match {
                case (Some(s), Some(dp)) =>
                  ops
                    .addEntityScalarDataPropertyParticularRestrictionAxiom(mg, restrictedEntity = s, scalarProperty = dp, literalValue = ax.literalValue)
                    .flatMap { oax =>
                      if (ax.uuid == ops.getAxiomUUID(oax)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table EntityScalarDataPropertyParticularRestrictionAxiom $ax conversion " +
                            s"results in UUID mismatch: ${ops.getAxiomUUID(oax)}")).left
                    }
                case (_, _) =>
                  Failure(OMFError.omfError(s"Unresolved EntityScalarDataPropertyParticularRestrictionAxiom: $ax"))
              }
            case (acc1, _) =>
              acc1
          }
        } yield ()
      }

      // -- SpecializationAxiom
      _ <- tbox2ont.foldLeft[Try[Unit]](Success(())) { case (acc, (tbox, mg)) =>
        for {
          _ <- acc
          _ = System.out.println(s"... SpecializationAxiom: ${tbox.iri}")
          _ <- tbox.boxStatements.foldLeft[Try[Unit]](Success(())) {
            case (acc1, ax: api.AspectSpecializationAxiom) =>
              (ops.lookupEntity(mg, IRI.create(ax.subEntity.iri), recursively = true),
                ops.lookupAspect(mg, IRI.create(ax.superAspect.iri), recursively = true)) match {
                case (Some(sub), Some(sup)) =>
                  ops
                    .addAspectSpecializationAxiom(mg, sub, sup)
                    .flatMap { oax =>
                      if (ax.uuid == ops.getAxiomUUID(oax)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table AspectSpecializationAxiom $ax conversion " +
                            s"results in UUID mismatch: ${ops.getAxiomUUID(oax)}")).left
                    }
                case (_, _) =>
                  Failure(OMFError.omfError(s"Unresolved AspectSpecializationAxiom: $ax"))
              }
            case (acc1, ax: api.ConceptSpecializationAxiom) =>
              (ops.lookupConcept(mg, IRI.create(ax.subConcept.iri), recursively = true),
                ops.lookupConcept(mg, IRI.create(ax.superConcept.iri), recursively = true)) match {
                case (Some(sub), Some(sup)) =>
                  ops
                    .addConceptSpecializationAxiom(mg, sub, sup)
                    .flatMap { oax =>
                      if (ax.uuid == ops.getAxiomUUID(oax)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table ConceptSpecializationAxiom $ax conversion " +
                            s"results in UUID mismatch: ${ops.getAxiomUUID(oax)}")).left
                    }
                case (_, _) =>
                  Failure(OMFError.omfError(s"Unresolved ConceptSpecializationAxiom: $ax"))
              }
            case (acc1, ax: api.ReifiedRelationshipSpecializationAxiom) =>
              (ops.lookupReifiedRelationship(mg, IRI.create(ax.subRelationship.iri), recursively = true),
                ops.lookupReifiedRelationship(mg, IRI.create(ax.superRelationship.iri), recursively = true)) match {
                case (Some(sub), Some(sup)) =>
                  ops
                    .addReifiedRelationshipSpecializationAxiom(mg, sub, sup)
                    .flatMap { oax =>
                      if (ax.uuid == ops.getAxiomUUID(oax)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table ReifiedRelationshipSpecializationAxiom $ax conversion " +
                            s"results in UUID mismatch: ${ops.getAxiomUUID(oax)}")).left
                    }
                case (_, _) =>
                  Failure(OMFError.omfError(s"Unresolved ReifiedRelationshipSpecializationAxiom: $ax"))
              }
            case (acc1, _) =>
              acc1
          }
        } yield ()
      }

      tbox2iont <- tbox2ont
        .foldLeft[Try[(Seq[(api.TerminologyBox, ImmutableTerminologyBox)], OWLAPIOMF#Mutable2ImmutableTerminologyMap)]](
        Success(Tuple2(Seq.empty, Map.empty))
      ) { case (acc, (tbox, mg)) =>
          for {
            pair <- acc
            (convs, m2i) = pair
            _ = System.out.println(s"... Converting terminology ${mg.kind}: ${mg.ont.getOntologyID}")
            c <- ops.asImmutableTerminology(m2i, mg) match {
              case -\/(errors) =>
                Failure(OMFError.omfException("asImmutableTerminology error", errors))
              case \/-(converted) =>
                Success(converted)
            }
            (conv, m2iWithConversion) = c
          } yield Tuple2(convs :+ (tbox -> conv), m2iWithConversion)
      }

      _ <- tbox2iont._1.foldLeft[Try[Unit]](Success(())) { case (acc, (tbox, i)) =>
        acc.flatMap { _ =>
          System.out.println(s"... Saving terminology ${i.ont.getOntologyID}")
          val next = ops.saveTerminology(i)
          next
        }
      }

    } yield ()

    result match {
      case Failure(t) =>
        throw t
      case Success(_) =>
        System.out.println("... Done!")
    }
  }

  final protected def convertDataRanges
  (tbox: api.TerminologyBox,
   mg: MutableTerminologyBox,
   drs: SortedSet[api.DataRange],
   queue: SortedSet[api.DataRange]=TreeSet.empty,
   progress: Int=0)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Try[Unit]
  = {
    if (drs.isEmpty) {
      if (queue.isEmpty)
        Success(())
      else if (0 == progress)
        Failure(OMFError.omfError("No-progress in convertDataRanges!"))
      else
        convertDataRanges(tbox, mg, queue)
    } else
      drs.head match {
        case rdr: api.RestrictedDataRange =>
          ops.lookupDataRange(mg, IRI.create(rdr.restrictedRange.iri), recursively = true) match {
            case None =>
              convertDataRanges(tbox, mg, drs.tail, queue + drs.head, progress)
            case Some(r) =>
              val mr: Set[java.lang.Throwable] \/ Unit = rdr match {
                case sr: api.BinaryScalarRestriction =>
                  ops
                    .addBinaryScalarRestriction(
                      mg, sr.name, r,
                      sr.length, sr.minLength, sr.maxLength)
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table BinaryScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.IRIScalarRestriction =>
                  ops
                    .addIRIScalarRestriction(
                      mg, sr.name, r,
                      sr.length, sr.minLength, sr.maxLength, sr.pattern)
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table IRIScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.NumericScalarRestriction =>
                  ops
                    .addNumericScalarRestriction(
                      mg, sr.name, r,
                      sr.minInclusive, sr.maxInclusive, sr.minExclusive, sr.maxExclusive)
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table NumericScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.PlainLiteralScalarRestriction =>
                  ops
                    .addPlainLiteralScalarRestriction(
                      mg, sr.name, r,
                      sr.length, sr.minLength, sr.maxLength, sr.pattern)
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table PlainLiteralScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.ScalarOneOfRestriction =>
                  ops
                    .addScalarOneOfRestriction(
                      mg, sr.name, r)
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table ScalarOneOfRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.StringScalarRestriction =>
                  ops
                    .addStringScalarRestriction(
                      mg, sr.name, r,
                      sr.length, sr.minLength, sr.maxLength, sr.pattern)
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table StringScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.TimeScalarRestriction =>
                  ops
                    .addTimeScalarRestriction(
                      mg, sr.name, r,
                      sr.minInclusive, sr.maxInclusive, sr.minExclusive, sr.maxExclusive)
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table TimeScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
              }
              mr match {
                case -\/(errors) =>
                  Failure(OMFError.omfException("Errors in convertDataRanges", errors))
                case \/-(_) =>
                  convertDataRanges(tbox, mg, drs.tail, queue, 1+progress)
              }
          }
        case _ =>
          convertDataRanges(tbox, mg, drs.tail, queue, progress)
      }
  }

  final protected def convertReifiedRelationships
  (tbox: api.TerminologyBox,
   mg: MutableTerminologyBox,
   rrs: SortedSet[api.ReifiedRelationship],
   queue: SortedSet[api.ReifiedRelationship]=TreeSet.empty,
   progress: Int=0)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Try[Unit]
  = {
    if (rrs.isEmpty) {
      if (queue.isEmpty)
        Success(())
      else if (0 == progress)
        Failure(OMFError.omfError("No progress in convertReifiedRelationships!"))
      else
        convertReifiedRelationships(tbox, mg, queue)
    } else {
      val rr = rrs.head
      (ops.lookupEntity(mg, IRI.create(rr.source.iri), recursively = true),
        ops.lookupEntity(mg, IRI.create(rr.target.iri), recursively = true)) match {
        case (Some(s), Some(t)) =>
          val mr: Set[java.lang.Throwable] \/ Unit = ops
            .addReifiedRelationship(
              mg, s, t,
              Iterable() ++
                (if (rr.isAsymmetric) Iterable(RelationshipCharacteristics.isAsymmetric) else Iterable()) ++
                (if (rr.isEssential) Iterable(RelationshipCharacteristics.isEssential) else Iterable()) ++
                (if (rr.isFunctional) Iterable(RelationshipCharacteristics.isFunctional) else Iterable()) ++
                (if (rr.isInverseEssential) Iterable(RelationshipCharacteristics.isInverseEssential) else Iterable()) ++
                (if (rr.isInverseFunctional) Iterable(RelationshipCharacteristics.isInverseFunctional) else Iterable()) ++
                (if (rr.isIrreflexive) Iterable(RelationshipCharacteristics.isIrreflexive) else Iterable()) ++
                (if (rr.isReflexive) Iterable(RelationshipCharacteristics.isReflexive) else Iterable()) ++
                (if (rr.isSymmetric) Iterable(RelationshipCharacteristics.isSymmetric) else Iterable()) ++
                (if (rr.isTransitive) Iterable(RelationshipCharacteristics.isTransitive) else Iterable()),
              rr.name,
              rr.unreifiedPropertyName,
              rr.unreifiedInversePropertyName)
            .flatMap { orr =>
              if (rr.uuid == ops.getTermUUID(orr)) ().right
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"OMF Schema table ReifiedRelationship $rr conversion " +
                    s"results in UUID mismatch: ${ops.getTermUUID(orr)}")).left
            }
          mr match {
            case -\/(errors) =>
              Failure(OMFError.omfException("Errors in convertReifiedRelationships", errors))
            case \/-(_) =>
              convertReifiedRelationships(tbox, mg, rrs.tail, queue, 1+progress)
          }
        case (_, _) =>
          convertReifiedRelationships(tbox, mg, rrs.tail, queue + rrs.head, progress)
      }
    }
  }

}
