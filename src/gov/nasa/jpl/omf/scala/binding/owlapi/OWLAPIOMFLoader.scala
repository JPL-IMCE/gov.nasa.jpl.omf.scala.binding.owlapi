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
import scala.{Boolean, None, Option, Some, StringContext, Tuple2, Unit, annotation}
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
   nestings: Set[NestingOntologyAndContextToNestedGraphIRI],
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
        nestings = Set(),
        queue = Set(iri)))

    private object Internal {

      @annotation.tailrec
      def loadOntologiesRecursively
      (s: OntologyLoaderState)
      (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
      : Set[java.lang.Throwable] \/ OntologyLoaderState
      = if (s.queue.isEmpty)
        \/-(s)
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

            // in principle, we should exclude the known ontology IRIs
            // i.e., -- s.ontologies.keySet
            // but this would be more expensive than tail recursive calls
            val newIRIs = es.map(_.extendedG) ++ ns.map(_.nestedG)

            \/-(OntologyLoaderState(
              s.ontologies + (iri -> ont),
              s.extensions ++ es,
              s.nestings ++ ns,
              s.queue - iri ++ newIRIs))

          }
      }) { _: OWLOntology =>

        \/-(s.copy(queue = s.queue - iri))

      }

    }

  }

  def loadTerminologyGraph
  (iri: IRI)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap)
  = OntologyLoaderState
    .loadAllOntologies(iri)
    .flatMap { s =>
      implicit val graphConfig = CoreConfig(orderHint = 5000, Hints(64, 0, 64, 75))

      val g0 = Graph[IRI, DiEdge](s.ontologies.keys.toSeq: _*)
      val g1 = (g0 /: s.extensions) { (gi, e) =>
        gi + e.extendingG ~> e.extendedG
      }
      val g2 = (g1 /: s.nestings) { (gj, n) =>
        gj + n.nestedG ~> n.nestingG
      }

      scala.Predef.???
    }

}