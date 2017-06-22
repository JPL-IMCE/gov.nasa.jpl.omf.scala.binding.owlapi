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

import gov.nasa.jpl.omf.scala.binding.owlapi.BackboneDeclaractions.BackboneDeclaractions
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core._

import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scalax.collection.config._
import scalax.collection.connectivity.GraphComponents
import scalax.collection.mutable.ArraySet.Hints
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._

import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.reflect.ClassTag
import scala.util.control.Exception._
import scala.{None, Option, Some, StringContext, annotation}
import scala.Predef.{Map => _, Set => _, _}

import scalaz._
import Scalaz._

object OWLAPIOMFLoader {

  @scala.annotation.tailrec
  final def hierarchicalTopologicalSort[N: ClassTag, E[M] <: DiEdge[M]]
  (queue: Seq[Graph[N, E]], before: Seq[N] = Seq.empty, after: Seq[N] = Seq.empty)
  : Throwables \/ Seq[N]
  = queue match {
    case Nil =>
      (before ++ after).right
    case g :: gs =>

      // Workaround
      // https://github.com/scala-graph/scala-graph/issues/75
      Option.apply(
        org.apache.log4j.LogManager.getLogger("scalax.collection.connectivity.GraphComponents")
      ) match {
        case Some(logger) =>
          logger.setLevel(org.apache.log4j.Level.OFF)
        case None =>
          ()
      }

      // TODO detect disconnected components!
      val dag = GraphComponents.graphToComponents(g).stronglyConnectedComponentsDag

      dag.topologicalSortByComponent().toList match {
        case Nil =>
          (before ++ after).right[Throwables]

        case n :: ns =>
          if (n.isLeft) {
            val n1 = n.left.get
            val ns: Seq[N] = n1.toOuter.toOuterNodes.to[Seq]
            hierarchicalTopologicalSort(gs, before ++ ns, after)
          } else if (n.isRight) {
            val cycle = n.right.get
            val ns: Seq[N] = cycle.toOuterNodes.to[Seq].flatMap(_.toOuterNodes.to[Seq])
            hierarchicalTopologicalSort(gs, before ++ ns, after)
          } else
            hierarchicalTopologicalSort(gs, before, after)

      }
  }

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
   ontIRI: IRI)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore, drc: BuiltInDatatypeMap)
  : Set[ExtendingOntologyToExtendedModuleIRI]
  = {
    val explicit = getOntologyDirectlyImportedDocuments(ont)
      .map(iri => ExtendingOntologyToExtendedModuleIRI(extending = ontIRI, extended = iri))
    if (ontIRI != omlIRI) {
      explicit
    } else {
      val builtin = drc.builtInDatatypeModules.map { m =>
        val mIRI = ops.getModuleIRI(m)
        ExtendingOntologyToExtendedModuleIRI(extending = omlIRI, extended = mIRI)
      }
      explicit ++ builtin
    }
  }

  case class OntologyLoadedState
  (ontologies: Map[IRI, OWLOntology],
   extensions: Set[ExtendingOntologyToExtendedModuleIRI],
   nested2context: Set[NestedOntologyToNestingContextIRI]) {}

  object OntologyLoadedState {

    def loadAllOntologies
    (iri: IRI,
     drc: BuiltInDatatypeMap)
    (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
    : Throwables \/ OntologyLoadedState
    = Internal.loadOntologiesRecursively(
      Internal.OntologyLoaderState(queue = Set(iri)))(ops, store, drc)

    private object Internal {

      case class OntologyLoaderState
      (ontologies: Map[IRI, OWLOntology] = Map.empty,
       extensions: Set[ExtendingOntologyToExtendedModuleIRI] = Set.empty,
       nested2context: Set[NestedOntologyToNestingContextIRI] = Set.empty,
       queue: Set[IRI] = Set.empty) {}

      @annotation.tailrec
      def loadOntologiesRecursively
      (s: OntologyLoaderState)
      (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore, drc: BuiltInDatatypeMap)
      : Throwables \/ OntologyLoadedState
      = if (s.queue.isEmpty)
          OntologyLoadedState(
            ontologies = s.ontologies,
            extensions = s.extensions,
            nested2context = s.nested2context).right
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
      (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore, drc: BuiltInDatatypeMap)
      : Throwables \/ OntologyLoaderState
      = if (drc.isBuiltInModule(iri))
        \/-(s.copy(queue = s.queue - iri))
      else
        s
        .ontologies
        .get(iri)
        .fold[Throwables \/ OntologyLoaderState] {
        nonFatalCatch[OntologyLoaderState]
          .withApply {
            cause: java.lang.Throwable =>
              Set(OMFError.omfException(
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

            val es = getOntologyDirectlyExtendedModules(ont, iri)
            val nc = getNestedOntologyToNestingContextIRIIfAny(ont, iri)

            // in principle, we should exclude the known ontology IRIs
            // i.e., -- s.ontologies.keySet
            // but this would be more expensive than tail recursive calls
            val newIRIs = es.map(_.extended)

            \/-(OntologyLoaderState(
              s.ontologies + (iri -> ont),
              s.extensions ++ es,
              s.nested2context ++ nc,
              s.queue - iri ++ newIRIs))

          }
      } { _: OWLOntology =>

        \/-(s.copy(queue = s.queue - iri))

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
    * Need to perform a 2-pass load:
    * 1) Load Ontology => MutableGraph where nesting parent is loaded before nested children
    * because a nested children graph can have references to its nesting parent graph.
    * At the end of this pass, all TerminologyGraphs are mutable.
    * 2) Convert MutableGraphs => ImmutableGraphs such that a nested graph is converted
    * to an ImmutableGraph so that it can be added to its nesting parent MutableGraph.
    *
    * @param iri
    * @param m2i
    * @param drc
    * @param ops
    * @param store
    * @return
    */
  def loadModule
  (iri: IRI,
   m2i: Mutable2ImmutableModuleMap,
   drc: BuiltInDatatypeMap)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Throwables \/ ImmutableModuleConversionMap
  = m2i.lookupValue(iri) match {
    case Some(im) =>
      (im -> m2i).right
    case None =>
      implicit val graphConfig = CoreConfig(orderHint = 5000, Hints(64, 0, 64, 75))

      val builtInEdges = drc.builtInDatatypeModules.flatMap { fM =>
        val fIRI = ops.getModuleIRI(fM)
        fM.sig.importedTerminologies.map { tM =>
          val tIRI = ops.getModuleIRI(tM)
          fIRI -> tIRI
        }
      }

      for {
        s <- OntologyLoadedState.loadAllOntologies(iri, drc)
        om = OntologyMapping.initialize(m2i, drc)

        // Add nodes for every OMF document ontology document IRI loaded.
        g0 = Graph[IRI, DiEdge](s.ontologies.keys.toSeq: _*)

        // Add edges for every OMF document extension relationship
        g1 = (g0 /: s.extensions) { (gi, e) =>
          gi + e.extending ~> e.extended
        }

        g2 = (g1 /: builtInEdges) { case (gi, (fIRI, tIRI)) =>
          gi + fIRI ~> tIRI
        }

        lorder <- hierarchicalTopologicalSort(Seq(g2), Seq.empty).map(_.reverse)

        _ = {
          java.lang.System.out.println(s"loadModule(iri=$iri) ordered ${lorder.size} modules:")
          lorder.foreach { iri =>
            java.lang.System.out.println(s"=> load: $iri")
          }
          java.lang.System.out.println()
        }

        resultPair <- loadModules(lorder, s, om).flatMap { loadedOM =>
          loadedOM.m2i.lookupValue(iri) match {
            case Some(im) =>
              (im -> loadedOM).right
            case _ =>
              Set(OMFError.omfError(
                s"BUG: There should have been an OML Module for $iri")).left
          }
        }

        (i, resultOM) = resultPair

      } yield i -> resultOM.m2i
  }


  def loadModules
  (queue: Seq[IRI],
   s: OntologyLoadedState,
   currentM2I: OntologyMapping)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Throwables \/ OntologyMapping
  = queue
    .foldLeft[Throwables \/ OntologyMapping]{
    currentM2I.right
  } {
    loadModuleFromOntologyDocument(s)
  }

  def loadModuleFromOntologyDocument
  (s: OntologyLoadedState)
  (acc: Throwables \/ OntologyMapping,
   ontIRI: IRI)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Throwables \/ OntologyMapping
  = acc.flatMap { om =>
    s.ontologies.get(ontIRI) match {
      case Some(ont) =>
        if (om.ont2i.contains(ont))
          om.right
        else om.lookupImmutableModule(ontIRI) match {
          case None =>
            val result = store.convertModuleFromOntologyDocument(s, om, ontIRI, ont)
            result
          case Some(_) =>
            // Already converted
            om.right
        }
      case None =>
        om.lookupImmutableModule(ontIRI) match {
          case Some(_) =>
            // Already converted
            om.right
          case None =>
            Set(OMFError.omfError(
              s"BUG: loadModuleFromOntologyDocument(ontIRI=$ontIRI) -- Cannot convert unloaded ontology!")
            ).left
        }
    }
  }

}