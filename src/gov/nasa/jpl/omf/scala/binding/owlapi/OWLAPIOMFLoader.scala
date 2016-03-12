/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.binding.owlapi

import java.lang.System
import java.util.concurrent.TimeUnit

import gov.nasa.jpl.omf.scala.binding.owlapi.BackboneDeclaractions.BackboneDeclaractions
import gov.nasa.jpl.omf.scala.core.builtin.BuiltInDatatypeMaps
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.ResolverHelper
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util.PriorityCollection

import scalax.collection.config._
import scalax.collection.mutable.ArraySet.Hints
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._

import scala.collection.immutable._
import scala.collection.JavaConversions._
import scala.concurrent.duration.FiniteDuration
import scala.language.postfixOps
import scala.util.control.Exception._
import scala.{Boolean, Int, None, Option, Some, StringContext, Tuple2, Unit, annotation}
import scala.Predef.{Map => _, Set => _, _}
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
    .getAxioms[OWLDeclarationAxiom](AxiomType.DECLARATION, Imports.EXCLUDED)
    .to[Set]
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

  case class NestingOntologyAndContextToNestedGraphIRI
  (nestingG: IRI,
   nestingC: OWLClass,
   nestedG: IRI)

  def getOntologyContextClasses2NestedGraphIRIs
  (ont: OWLOntology,
   ontIRI: IRI)
  (implicit ops: OWLAPIOMFOps)
  : Set[NestingOntologyAndContextToNestedGraphIRI]
  = getOntologyDirectlyDeclaredClasses(ont, BackboneDeclaractions.exclude)
    .flatMap { c =>
      ont
        .getAnnotationAssertionAxioms(c.getIRI)
        .flatMap { a =>
          if (ops.AnnotationHasGraph == a.getProperty.getIRI)
            a.getValue match {
              case iri: IRI =>
                Some(NestingOntologyAndContextToNestedGraphIRI(nestingG = ontIRI, nestingC = c, nestedG = iri))
              case _ =>
                require(false, s"${c.getIRI} has an annotation $a with a non-IRI value!")
                None
            }
          else
            None
        }
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
    .getAnnotations
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

  def getOntologyDirectlyImportedDocuments
  (ont: OWLOntology)
  : Set[IRI]
  = ont.getDirectImportsDocuments.to[Set]

  case class ExtendingOntologyToExtendedGraphIRI
  (extendingG: IRI,
   extendedG: IRI)

  def getOntologyDirectlyExtendedGraphs
  (ont: OWLOntology,
   ontIRI: IRI)
  : Set[ExtendingOntologyToExtendedGraphIRI]
  = getOntologyDirectlyImportedDocuments(ont)
    .map(iri => ExtendingOntologyToExtendedGraphIRI(extendingG = ontIRI, extendedG = iri))

  case class OntologyLoaderState
  (ontologies: Map[IRI, OWLOntology],
   extensions: Set[ExtendingOntologyToExtendedGraphIRI],
   nesting2nested: Set[NestingOntologyAndContextToNestedGraphIRI],
   nested2nesting: Set[NestedOntologyToNestingContextIRI],
   queue: Set[IRI]) {}


  object OntologyLoaderState {

    def loadAllOntologies
    (iri: IRI)
    (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
    : Set[java.lang.Throwable] \/ OntologyLoaderState
    = Internal.loadOntologiesRecursively(
      OntologyLoaderState(
        ontologies = Map(),
        extensions = Set(),
        nesting2nested = Set(),
        nested2nesting = Set(),
        queue = Set(iri)))

    private object Internal {

      @annotation.tailrec
      def loadOntologiesRecursively
      (s: OntologyLoaderState)
      (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
      : Set[java.lang.Throwable] \/ OntologyLoaderState
      = if (s.queue.isEmpty)
        validateNestingRelationsStep(
          OntologyLoaderState(
            ontologies = Map(),
            extensions = Set(),
            nesting2nested = Set(),
            nested2nesting = Set(),
            queue = Set()),
          s)
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
      : Set[java.lang.Throwable] \/ OntologyLoaderState
      = s
        .ontologies
        .get(iri)
        .fold[Set[java.lang.Throwable] \/ OntologyLoaderState]({
        nonFatalCatch[OntologyLoaderState]
          .withApply {
            cause: java.lang.Throwable =>
              Set(OMFError.omfException(
                s"loadTerminologyGraph failed: ${cause.getMessage}",
                cause)).left
          }
          .apply {
            val ont: OWLOntology =
              if (store.ontManager.contains(iri))
                store.ontManager.getOntology(iri)
              else
                store.ontManager.loadOntology(iri)
            assert(null != ont)

            val es = getOntologyDirectlyExtendedGraphs(ont, iri)
            val ns = getOntologyContextClasses2NestedGraphIRIs(ont, iri)
            val nc = getNestedOntologyToNestingContextIRIIfAny(ont, iri)

            // in principle, we should exclude the known ontology IRIs
            // i.e., -- s.ontologies.keySet
            // but this would be more expensive than tail recursive calls
            val newIRIs = es.map(_.extendedG) ++ ns.map(_.nestedG)

            \/-(OntologyLoaderState(
              s.ontologies + (iri -> ont),
              s.extensions ++ es,
              s.nesting2nested ++ ns,
              s.nested2nesting ++ nc,
              s.queue - iri ++ newIRIs))

          }
      }) { _: OWLOntology =>

        \/-(s.copy(queue = s.queue - iri))

      }

      @annotation.tailrec
      def validateNestingRelationsStep
      (acc: OntologyLoaderState,
       s: OntologyLoaderState)
      : Set[java.lang.Throwable] \/ OntologyLoaderState
      = s
        .nesting2nested
        .headOption match {
        case None =>
          \/-(acc.copy(extensions = s.extensions))

        case Some(n2n) =>
          val ext = s.extensions.filter(e => e.extendedG == n2n.nestingG && e.extendingG == n2n.nestedG)
          val nst = s.nested2nesting.filter(n => n.nestingC == n2n.nestingC && n.nestedG == n2n.nestedG)
          if (ext.isDefined && nst.isDefined)
            validateNestingRelationsStep(
              acc.copy(extensions = acc.extensions ++ ext, nested2nesting = acc.nested2nesting ++ nst),
              s.copy(nesting2nested = s.nesting2nested - n2n))
          else
            -\/(Set(OMFError.omfError(s"Inconsistency detected\n$n2n\n$ext\n$nst")))
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

  def loadTerminologyGraph
  (iri: IRI)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap)
  = OntologyLoaderState
    .loadAllOntologies(iri)
    .flatMap { s =>

      implicit val graphConfig = CoreConfig(orderHint = 5000, Hints(64, 0, 64, 75))

      // Add nodes for every OMF document ontology document IRI loaded.
      val g0: OMFDocumentGraph = Graph[IRI, DiEdge](s.ontologies.keys.toSeq: _*)

      // Add edges for every OMF document extension relationship
      val g1: OMFDocumentGraph = (g0 /: s.extensions) { (gi, e) =>

        // load the extended parent graph before the extending child graph
        // because terms in the extending child graph can reference
        // terms in scope of the extended parent graph
        gi + e.extendedG ~> e.extendingG

      }

      // Add edges for every OMF document nesting relationship
      val g2: OMFDocumentGraph = (g1 /: s.nesting2nested) { (gj, n) =>

        // load the nesting parent graph before the nested child graph
        // because terms in the nested child graph can reference
        // terms in scope of the nesting parent graph
        gj + n.nestedG ~> n.nestingG

      }

      g2
        .topologicalSort()
        .fold(
          (cycleNode: OMFDocumentNode) =>
            -\/(Set(
              OMFError.omfError(
                s"Graph with ${g2.nodes.size} ontologies, ${g2.edges.size} edges has a cycle involving\n"+
                cycleNode.value
              )
            )),
          (order: OMFDocumentTopologicalOrder) =>
            loadTerminologyGraphs(order, s).flatMap { m2i =>
              store.lookupTerminologyGraph(iri) match {
                case Some(ig: types.ImmutableModelTerminologyGraph) =>
                  \/-((ig, m2i))
                case other =>
                  -\/(Set(OMFError.omfError(
                    s"BUG: There should have been an OMF graph for $iri, instead got $other")))
              }
            }
        )
    }

  def loadTerminologyGraphs
  (queue: OMFDocumentTopologicalOrder,
   s: OntologyLoaderState)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap
  = ( types.emptyMutable2ImmutableTerminologyMapNES /: queue.toLayered )( loadTerminologyGraphLayer(s) )

  def loadTerminologyGraphLayer
  (s: OntologyLoaderState)
  (acc: Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap,
   layer: (Int, scala.collection.Iterable[OMFDocumentNode]))
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap
  = ( acc /: layer._2 )( loadTerminologyGraphFromOntologyDocument(s, layer._1) )

  def loadTerminologyGraphFromOntologyDocument
  (s: OntologyLoaderState, layer: Int)
  (acc: Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap,
   node: OMFDocumentNode)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap
  = acc.flatMap { m2i =>
    val ontIRI = node.value
    assert(s.ontologies.contains(ontIRI))
    val ont = s.ontologies(ontIRI)

    store
      .lookupTerminologyGraph(ontIRI)
      .fold[Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap](
      store.convertTerminologyGraphFromOntologyDocument(s, m2i, ontIRI, ont)
    ) { _ =>
      \/-(m2i)
    }
  }

}