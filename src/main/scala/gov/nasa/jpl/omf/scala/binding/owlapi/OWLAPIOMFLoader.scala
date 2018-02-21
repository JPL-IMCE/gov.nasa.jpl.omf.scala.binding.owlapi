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

package gov.nasa.jpl.omf.scala.binding.owlapi

import java.lang.{IllegalArgumentException, System}

import gov.nasa.jpl.imce.oml.resolver.GraphUtilities
import gov.nasa.jpl.omf.scala.binding.owlapi.BackboneDeclaractions.BackboneDeclaractions
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{ImmutableModule, Module, MutableModule}
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.MutableDescriptionBox
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{MutableBundle, MutableTerminologyGraph}
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scalax.collection.config._
import scalax.collection.mutable.ArraySet.Hints
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scala.collection.immutable._
import scala.compat.java8.OptionConverters.RichOptionalGeneric
import scala.compat.java8.StreamConverters._
import scala.util.control.Exception.nonFatalCatch
import scala.{Int, None, Option, Ordering, Some, StringContext, annotation}
import scala.Predef.{ArrowAssoc, assert, require}
import scalaz._
import Scalaz._

object OWLAPIOMFLoader {

  /**
    * Get the classes directly declared in an ontology.
    *
    * @param ont      an ontology
    * @param backbone whether to include or exclude backbone declarations
    * @return the classes that are declared in `ont`, optionally with the backbone classes if backbone=included.
    */
  def getOntologyDirectlyDeclaredClasses
  (ont: OWLOntology,
   backbone: BackboneDeclaractions)
  (implicit ops: OWLAPIOMFOps)
  : Set[OWLClass]
  = ont
    .axioms[OWLDeclarationAxiom](AxiomType.DECLARATION, Imports.EXCLUDED)
    .toScala[Set]
    .map(_.getEntity)
    .flatMap {
      case c: OWLClass =>
        if (BackboneDeclaractions.exclude == backbone && ops.isBackboneIRI(c.getIRI))
          None
        else
          Some(c)
      case _ =>
        None
    }

  case class NestedOntologyToNestingContextIRI
  (nestingC: IRI,
   nestedG: IRI)

  def getNestedOntologyToNestingContextIRIIfAny
  (ont: OWLOntology,
   ontIRI: IRI)
  (implicit ops: OWLAPIOMFOps)
  : Option[NestedOntologyToNestingContextIRI]
  = getOntologyParentContextIfAny(ont)
    .map { iri => NestedOntologyToNestingContextIRI(nestingC = iri, nestedG = ontIRI) }

  def getOntologyParentContextIfAny
  (ont: OWLOntology)
  (implicit ops: OWLAPIOMFOps)
  : Option[IRI]
  = ont
    .annotations
    .toScala[Set]
    .find(ops.AnnotationHasContext == _.getProperty.getIRI)
    .flatMap { a =>
      a.getValue match {
        case iri: IRI =>
          Some(iri)
        case _ =>
          require(false, s"$ont has an annotation $a with a non-IRI value!")
          None
      }
    }

  /*
   * Deferring WIP https://github.com/JPL-IMCE/gov.nasa.jpl.omf.scala.binding.owlapi/issues/7
   */
  def getOntologyDirectlyImportedDocuments
  (ont: OWLOntology)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[IRI]
  = {
    val iris = ont.directImportsDocuments.toScala[Set]
    // Removed the filtering to pass the current OMF unit tests.
    //val filtered = iris.filterNot(store.isBuiltInIRI)
    //filtered
    iris
  }

  case class ExtendingOntologyToExtendedModuleIRI
  (extending: IRI,
   extended: IRI)

  def getOntologyDirectlyExtendedModules
  (ont: OWLOntology,
   ontIRI: IRI,
   om: OntologyMapping)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Set[ExtendingOntologyToExtendedModuleIRI]
  = {
    val explicit = getOntologyDirectlyImportedDocuments(ont)
      .map(iri => ExtendingOntologyToExtendedModuleIRI(extending = ontIRI, extended = iri))
    if (ontIRI != omlIRI) {
      explicit
    } else {
      val builtin = om.drc.builtInDatatypeModules.map { m =>
        val mIRI = ops.getModuleIRI(m)
        ExtendingOntologyToExtendedModuleIRI(extending = omlIRI, extended = mIRI)
      }
      explicit ++ builtin
    }
  }

  /**
    * TODO distinguish between:
    * - BundledTerminologyAxioms
    * - TerminologyExtensionAxioms
    *
    * Both are owl imports.
    *
    * @param ontologies
    * @param extensions
    * @param nested2context
    */
  case class OntologyLoadedState
  (ontologies: Map[IRI, OWLOntology],
   extensions: Set[ExtendingOntologyToExtendedModuleIRI],
   nested2context: Set[NestedOntologyToNestingContextIRI],
   om: OntologyMapping) {}

  object OntologyLoadedState {

    def loadAllOntologies
    (iri: IRI,
     om: OntologyMapping)
    (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
    : Throwables \/ OntologyLoadedState
    = Internal.loadOntologiesRecursively(
      Internal.OntologyLoaderState(
        ontologies = om.drc.builtInDatatypeModules.map { m => m.iri -> m.ont }.toMap,
        om = om,
        queue = Set(iri)))

    private object Internal {

      case class OntologyLoaderState
      (ontologies: Map[IRI, OWLOntology] = Map.empty,
       extensions: Set[ExtendingOntologyToExtendedModuleIRI] = Set.empty,
       nested2context: Set[NestedOntologyToNestingContextIRI] = Set.empty,
       om: OntologyMapping,
       queue: Set[IRI] = Set.empty) {}

      @annotation.tailrec
      final def loadOntologiesRecursively
      (s: OntologyLoaderState)
      (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
      : Throwables \/ OntologyLoadedState
      = if (s.queue.isEmpty)
        OntologyLoadedState(
          ontologies = s.ontologies,
          extensions = s.extensions,
          nested2context = s.nested2context,
          om = s.om).right
      else
        loadOneOntology(s, s.queue.head) match {
          case -\/(nels) =>
            -\/(nels)
          case \/-(next) =>
            loadOntologiesRecursively(next)
        }

      def loadOneOntology
      (s: OntologyLoaderState,
       iri: IRI)
      (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
      : Throwables \/ OntologyLoaderState
      = s.om.drc.lookupBuiltInModule(iri)
        .fold[Throwables \/ OntologyLoaderState] {
        s
          .om
          .lookupValue(iri) match {
          case Some(im) =>
            System.out.println(s"! loadOneOntology($iri) : already loaded!")
            val ont = im.ont

            val es = getOntologyDirectlyExtendedModules(ont, iri, s.om)
            val nc = getNestedOntologyToNestingContextIRIIfAny(ont, iri)

            // in principle, we should exclude the known ontology IRIs
            // i.e., -- s.ontologies.keySet
            // but this would be more expensive than tail recursive calls
            val newIRIs = es.map(_.extended)

            OntologyLoaderState(
              s.ontologies + (iri -> ont),
              s.extensions ++ es,
              s.nested2context ++ nc,
              s.om,
              s.queue - iri ++ newIRIs).right[Throwables]
          case None =>
            s
              .ontologies
              .get(iri)
              .fold[Throwables \/ OntologyLoaderState] {
              for {
                ont <- nonFatalCatch[Throwables \/ OWLOntology]
                  .withApply {
                    case t: OWLOntologyAlreadyExistsException =>
                      t.getOntologyID.getOntologyIRI.asScala.fold[Throwables \/ OWLOntology] {
                        Set[java.lang.Throwable](new IllegalArgumentException(
                          s"loadOneOntology failed for: $iri"
                        )).left
                      } { iri =>
                        System.out.println(s"!!! OWLOntologyAlreadyExistsException => $iri")
                        val ont = store.ontManager.getOntology(iri)
                        assert(null != ont)
                        ont.right[Throwables]
                      }

                    case cause: java.lang.Throwable =>
                      Set[java.lang.Throwable](OMFError.omfException(
                        s"loadTerminologyGraph($iri) failed: ${cause.getMessage}",
                        cause)).left
                  }
                  .apply {
                    val ont: OWLOntology =
                      if (store.ontManager.contains(iri))
                        store.ontManager.getOntology(iri)
                      else
                        store.ontManager.loadOntology(iri)
                    assert(null != ont)
                    ont.right[Throwables]
                  }
                result <- nonFatalCatch[Throwables \/ OntologyLoaderState]
                  .withApply {
                    cause: java.lang.Throwable =>
                      Set[java.lang.Throwable](OMFError.omfException(
                        s"loadTerminologyGraph($iri) failed: ${cause.getMessage}",
                        cause)).left
                  }
                  .apply {

                    val es = getOntologyDirectlyExtendedModules(ont, iri, s.om)
                    val nc = getNestedOntologyToNestingContextIRIIfAny(ont, iri)

                    // in principle, we should exclude the known ontology IRIs
                    // i.e., -- s.ontologies.keySet
                    // but this would be more expensive than tail recursive calls
                    val newIRIs = es.map(_.extended)

                    OntologyLoaderState(
                      s.ontologies + (iri -> ont),
                      s.extensions ++ es,
                      s.nested2context ++ nc,
                      s.om,
                      s.queue - iri ++ newIRIs).right[Throwables]

                  }
              } yield result
            } { _: OWLOntology =>
              s.copy(
                queue = s.queue - iri
              ).right[Throwables]
            }
        }
      } { m =>
        s.copy(
          ontologies = s.ontologies + (iri -> m.ont),
          queue = s.queue - iri
        ).right[Throwables]
      }

    }

  }

  /**
    * The type of a directed graph of OMF documents.
    *
    * A node is an OMF document is specified by its OWL Ontology document IRI.
    * A directed edge corresponds to a relationship among OMF documents (extension or nesting).
    */
  type OMFDocumentGraph = Graph[IRI, DiEdge]

  /**
    * The type of a node in an OMF document graph.
    */
  type OMFDocumentNode = OMFDocumentGraph#NodeT

  /**
    * The type of a topological ordering of OMF document graph nodes.
    */
  type OMFDocumentTopologicalOrder = OMFDocumentGraph#TopologicalOrder[Graph[IRI, DiEdge]#NodeT]

  val omlIRI = IRI.create("http://imce.jpl.nasa.gov/oml/oml")

  /**
    * Called from OWLAPIOMFGraphStore.loadModule
    *
    * @param iri
    * @param m2i
    * @param ops
    * @param store
    * @return
    */
  def loadModule
  (iri: IRI,
   m2i: OntologyMapping)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Throwables \/ (Module, OntologyMapping)
  = m2i.lookupValue(iri) match {
    case Some(im) =>
      (im -> m2i).right
    case None =>
      implicit val iriOrdering: Ordering[IRI] = new Ordering[IRI] {
        override def compare(x: IRI, y: IRI): Int = x.toString.compareTo(y.toString)
      }
      implicit val graphConfig: CoreConfig = CoreConfig(orderHint = 5000, Hints(64, 0, 64, 75))

      val builtInEdges = m2i.drc.builtInDatatypeModules.flatMap { fM =>
        val fIRI = ops.getModuleIRI(fM)
        fM.sig.importedTerminologies.map { tIRI =>
          fIRI -> tIRI
        }
      }

      for {
        s1 <- OntologyLoadedState.loadAllOntologies(iri, m2i)

        // Add nodes for every OMF document ontology document IRI loaded.
        g0 = Graph[IRI, DiEdge](s1.ontologies.keys.toSeq: _*)

        // Add edges for every OMF document extension relationship
        g1 = (g0 /: s1.extensions) { (gi, e) =>
          gi + e.extending ~> e.extended
        }

        g2 = (g1 /: builtInEdges) { case (gi, (fIRI, tIRI)) =>
          gi + fIRI ~> tIRI
        }

        lorder <- GraphUtilities.hierarchicalTopologicalSort(Seq(g2)).map(_.reverse)

        _ = {
          java.lang.System.out.println(s"loadModule(iri=$iri) ordered ${lorder.size} modules:")
          lorder.foreach { iri =>
            java.lang.System.out.println(s"=> load: $iri")
          }
          java.lang.System.out.println()
        }

        createPair <- createModules(lorder, s1)
        (mutableModules, s2) = createPair

        resultPair <- loadAllModuleContentsFromOntologies(mutableModules, s2).flatMap { loadedOM =>
          loadedOM.lookupModule(iri) match {
            case Some(m) =>
              (m -> loadedOM).right
            case _ =>
              Set(OMFError.omfError(
                s"BUG: There should have been an OML Module for $iri")).left
          }
        }

        (i, resultOM) = resultPair

      } yield i -> resultOM
  }

  def createModules
  (queue: Seq[IRI],
   s: OntologyLoadedState)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Throwables \/ (Seq[Module], OntologyLoadedState)
  = queue
    .foldLeft[Throwables \/ (Seq[Module], OntologyLoadedState)] {
    \/-(Seq.empty[Module] -> s)
  }{
    case (\/-((ms, si)), iri) =>
      createModule(iri, ms, si)
    case (-\/(errors), _) =>
      -\/(errors)
  }

  def createModule
  (iri: IRI,
   ms: Seq[Module],
   s: OntologyLoadedState)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Throwables \/ (Seq[Module], OntologyLoadedState)
  = s.ontologies.get(iri) match {
    case Some(ont) =>
      s.om.lookupModule(iri) match {
        case Some(m) =>
          \/-((ms :+ m) -> s)

        case None =>

          val ontOps = new OWLOntologyOps(ont)
          if (ontOps.isBundleOntology) {
            // Bundle
            val kind =
              if (ontOps.isOpenWorldDefinitionTerminologyBoxOntology)
                TerminologyKind.isOpenWorld
              else
                TerminologyKind.isClosedWorld

            store
              .createOMFBundle(ont.getOntologyID.getOntologyIRI.get, ont, kind)
              .flatMap { g: MutableBundle =>
                s.om.addMutableModule(g).map { omg =>
                  (ms :+ g) -> s.copy(om = omg)
                }
              }
          } else if (ontOps.isDescriptionBoxOntology) {
            // DescriptionBox
            val kind = if (ontOps.isFinalDescriptionBoxOntology)
              DescriptionKind.isPartial
            else
              DescriptionKind.isFinal

            store
              .createOMFDescriptionBox(ont.getOntologyID.getOntologyIRI.get, ont, kind)
              .flatMap { g: MutableDescriptionBox =>
                s.om.addMutableModule(g).map { omg =>
                  (ms :+ g) -> s.copy(om = omg)
                }
              }
          } else {
            // TerminologyGraph
            val kind =
              if (ontOps.isOpenWorldDefinitionTerminologyBoxOntology)
                TerminologyKind.isOpenWorld
              else
                TerminologyKind.isClosedWorld

            store
              .createOMFTerminologyGraph(ont.getOntologyID.getOntologyIRI.get, ont, kind)
              .flatMap { g: MutableTerminologyGraph =>
                s.om.addMutableModule(g).map { omg =>
                  (ms :+ g) -> s.copy(om = omg)
                }
              }
          }
      }
    case None =>
      Set(OMFError.omfError(
        s"BUG: createModule(iri=$iri) -- Unrecognized OWL Ontology pattern for an OML Module.")
      ).left
  }

  def loadAllModuleContentsFromOntologies
  (queue: Seq[Module],
   s: OntologyLoadedState)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Throwables \/ OntologyMapping
  = queue
    .foldLeft[Throwables \/ OntologyMapping]{
    s.om.right
  } {
    case (\/-(om), mm: MutableModule) =>
      if (om.get(mm).isDefined)
        om.right
      else
        store.convertModuleFromOntologyDocument(s, om, mm).map(_._2)
    case (\/-(om), im) =>
      require(im.isInstanceOf[ImmutableModule])
      require(om.lookupModule(im.iri).contains(im))
      om.right
    case (-\/(errors), _) =>
      -\/(errors)
  }

}