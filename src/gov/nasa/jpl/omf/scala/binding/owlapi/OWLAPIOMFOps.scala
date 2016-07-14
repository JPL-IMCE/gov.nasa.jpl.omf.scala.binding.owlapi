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

import java.net.URI

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils
import org.semanticweb.owlapi.model._

import scala.util.control.Exception._
import scala.{AnyVal, Boolean, None, Option, Some, StringContext, Unit}
import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._

object OWLAPIIRIOps {

  def makeIRI
  (s: String)
  : Set[java.lang.Throwable] \/IRI =
    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set(
            OMFError.omfException(
              s"makeIR('$s') failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        org.semanticweb.owlapi.model.IRI.create(s).right
      })
}

trait OWLAPIIRIOps
  extends IRIOps[OWLAPIOMF] {

  def iri2hash(prefix: String, iri: IRI)
  : String =
    Hex.encodeHexString(DigestUtils.sha(prefix + iri.toString))

  // IRI

  override def makeIRI
  (s: String)
  : Set[java.lang.Throwable] \/IRI =
    OWLAPIIRIOps.makeIRI(s)

  override def withFragment
  (iri: IRI, fragment: String)
  : Set[java.lang.Throwable] \/ IRI = {
    val uriConfig = com.netaporter.uri.config.UriConfig.conservative
    val safeFragment = uriConfig.fragmentEncoder.encode(fragment, uriConfig.charset)
    val u = iri.toURI
    Option.apply(u.getFragment)
    .fold[Set[java.lang.Throwable] \/ IRI](
      org.semanticweb.owlapi.model.IRI.create(u.resolve("#" + safeFragment)).right
    ){ f =>
      Set(
        OMFError
        .omfException(
          s"Cannot add fragment '$fragment' to IRI: $iri",
          IRIFragmentException(iri)
        )
      ).left
    }
  }

  override def splitIRI
  (iri: IRI) = {
    val u = iri.toURI
    u.getFragment match {
      case f: String if f.nonEmpty =>
        (org.semanticweb.owlapi.model.IRI.create(new URI(u.getScheme, u.getSchemeSpecificPart, null)),
          Some(f))
      case _                       =>
        (iri,
          None)
    }
  }

  override def toAbbreviatedName
  (iri: IRI, lowercaseFragmentInitial: Boolean) =
    splitIRI(iri) match {
      case (_, None)           => None
      case (i, Some(fragment)) =>
        val path = i.toURI.getSchemeSpecificPart
        val slash = path.lastIndexOf('/')
        val last = path.substring(slash + 1)
        val fragmentInitial = if (lowercaseFragmentInitial) fragment.head.toLower else fragment.head
        val fragmentTail = fragment.tail
        Some(last + ":" + fragmentInitial + fragmentTail)
    }

  override def fromIRI
  (iri: IRI) = iri.toString

  override def isBackboneIRI
  (iri: IRI) = {
    val u = iri.toURI
    import u._
    getHost == "imce.jpl.nasa.gov" && getPath.startsWith("/backbone")
  }

  override def toBackboneIRI
  (iri: IRI) = {
    val u = iri.toURI
    import u._
    org.semanticweb.owlapi.model.IRI.create(new URI(
                                                     getScheme, getUserInfo, "imce.jpl.nasa.gov", getPort, "/backbone/" + getHost + getPath, getQuery, getFragment))
  }

  override def toSourceIRI
  (iri: IRI) =
    splitIRI(iri) match {
      case (iri, Some(f)) =>
        val fragment = s"has${f}Source"
        org.semanticweb.owlapi.model.IRI.create(iri.toURI.resolve("#" + fragment))
      case (iri, None)    =>
        throw IRISourcePropertyException(iri)
    }

  override def toTargetIRI
  (iri: IRI) =
    splitIRI(iri) match {
      case (iri, Some(f)) =>
        val fragment = s"has${f}Target"
        org.semanticweb.owlapi.model.IRI.create(iri.toURI.resolve("#" + fragment))
      case (iri, None)    =>
        throw IRIargetPropertyException(iri)
    }

}

trait OWLAPIStoreOps
  extends OMFStoreOps[OWLAPIOMF] {
  self: OWLAPIOMFOps =>

  override def lookupTerminologyGraph
  (iri: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelTerminologyGraph]
  = store.lookupTerminologyGraph(iri)

  override def loadBuiltinDatatypeMap
  ()
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/
    (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap) =
    store.loadBuiltinDatatypeMap

  override def loadTerminologyGraph
  (iri: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap) =
    store.loadTerminologyGraph(iri)(this)

  override def isTerminologyGraphMutable
  ( graph: types.ModelTerminologyGraph )
  ( implicit store: OWLAPIOMFGraphStore )
  : Boolean
  = graph match {
    case _: types.MutableModelTerminologyGraph =>
      true
    case _ =>
      false
  }

  override def asMutableTerminologyGraph
  ( graph: types.ModelTerminologyGraph )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[types.MutableModelTerminologyGraph]
  = graph match {
    case g: types.MutableModelTerminologyGraph =>
      Some(g)
    case _ =>
      None
  }

  override def isTerminologyGraphImmutable
  ( graph: types.ModelTerminologyGraph )
  ( implicit store: OWLAPIOMFGraphStore )
  : Boolean
  = graph match {
    case _: types.ImmutableModelTerminologyGraph =>
      true
    case _ =>
      false
  }

  override def asImmutableTerminologyGraph
  ( graph: types.ModelTerminologyGraph )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[types.ImmutableModelTerminologyGraph]
  = graph match {
    case g: types.ImmutableModelTerminologyGraph =>
      Some(g)
    case _ =>
      None
  }

  override def fromTerminologyGraph
  (graph: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : OWLAPITerminologyGraphSignature =
    store.fromTerminologyGraph(graph)

  override def getDirectlyExtendingGraphsOfExtendedParentGraph
  (extendedParentG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Iterable[types.TerminologyGraphDirectExtensionAxiom] =
    store.getDirectlyExtendingGraphsOfExtendedParentGraph(extendedParentG)

  def getDirectlyExtendedGraphsOfExtendingChildGraph
  (extendingChildG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Iterable[types.TerminologyGraphDirectExtensionAxiom] =
    store.getDirectlyExtendedGraphsOfExtendingChildGraph(extendingChildG)

  def isTerminologyGraphDirectNestingAxiom
  ( axiom: types.TerminologyGraphAxiom)
  ( implicit store: OWLAPIOMFGraphStore )
  : Boolean
  = axiom match {
    case _: types.TerminologyGraphDirectNestingAxiom =>
      true
    case _ =>
      false
  }

  /**
    * Find the axiom TerminologyGraphDirectNestingAxiom(nestedChild==nestedG), if any.
    */
  override def lookupNestingAxiomForNestedChildIfAny
  (nestedG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.TerminologyGraphDirectNestingAxiom]
  = store.lookupNestingAxiomForNestedChildIfAny(nestedG)

  /**
    * Find the axioms TerminologyGraphDirectNestingAxiom(nestingContext=nestingC).
    */
  override def lookupNestingAxiomsForNestingContext
  (nestingC: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[types.TerminologyGraphDirectNestingAxiom]
  = store.lookupNestingAxiomsForNestingContext(nestingC)

  /**
    * Find the axioms TerminologyGraphDirectNestingAxiom(nestingParent=nestingG)
    */
  override def lookupNestingAxiomsForNestingParent
  (nestingG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[types.TerminologyGraphDirectNestingAxiom]
  = store.lookupNestingAxiomsForNestingParent(nestingG)

  override def getNestingGraph
  (nestedG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelTerminologyGraph]
  = store.getNestingGraph(nestedG)

  override def getNestedGraphs
  (nestingG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Iterable[types.ModelTerminologyGraph]
  = store.getNestedGraphs(nestingG)

  override def getNestingParentGraphOfAxiom
  (axiom: types.TerminologyGraphDirectNestingAxiom)
  (implicit store: OWLAPIOMFGraphStore)
  : types.ModelTerminologyGraph
  = store.getNestingParentGraphOfAxiom(axiom)

  override def getNestingContextConceptOfAxiom
  (axiom: types.TerminologyGraphDirectNestingAxiom)
  (implicit store: OWLAPIOMFGraphStore)
  : types.ModelEntityConcept
  = store.getNestingContextConceptOfAxiom(axiom)

  override def getNestedChildGraphOfAxiom
  (axiom: types.TerminologyGraphDirectNestingAxiom)
  (implicit store: OWLAPIOMFGraphStore)
  : types.ModelTerminologyGraph
  = store.getNestedChildGraphOfAxiom(axiom)

  def makeTerminologyGraphWithPath
  (iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind,
   extraProvenanceMetadata: OTI2OMFModelTerminologyGraphProvenance)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.MutableModelTerminologyGraph =
    store.makeTerminologyGraph(iri, relativeIRIPath, relativeIRIHashPrefix, kind, extraProvenanceMetadata.some)(this)

  override def makeTerminologyGraph
  (iri: IRI,
   kind: TerminologyKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.MutableModelTerminologyGraph =
    store.makeTerminologyGraph(
      iri,
      relativeIRIPath=Option.empty[String],
      relativeIRIHashPrefix=Option.empty[String],
      kind,
      extraProvenanceMetadata=Option.empty[OTI2OMFModelTerminologyGraphProvenance])(this)

  override def saveTerminologyGraph
  (g: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore) =
    store.saveTerminologyGraph(g)(this)

  override def saveTerminologyGraph
  (g: types.ModelTerminologyGraph,
   os: java.io.OutputStream)
  (implicit store: OWLAPIOMFGraphStore) =
    store.saveTerminologyGraph(g, os)(this)

  override def asImmutableTerminologyGraph
  (g: types.MutableModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore) =
    store.asImmutableTerminologyGraph(Map(), g)

  def createOMFTerminologyGraph
  (o: OWLOntology,
   ont: OWLOntology,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind.TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.MutableModelTerminologyGraph =
    store.createOMFModelTerminologyGraph(
      o, ont.getOntologyID.getOntologyIRI.get,
      relativeIRIPath, relativeIRIHashPrefix, ont, kind, extraProvenanceMetadata)

  override def loadInstanceGraph
  (iri: IRI)
  (implicit store: OWLAPIOMFGraphStore):
  Set[java.lang.Throwable] \/ instances.ImmutableModelInstanceGraph =
    store.loadInstanceGraph(iri)

  override def fromInstanceGraph
  (graph: instances.ModelInstanceGraph) =
    graph.fromInstanceGraph

  override def asImmutableInstanceGraph
  (g: instances.MutableModelInstanceGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ImmutableModelInstanceGraph =
    store.asImmutableInstanceGraph(g)

  override def makeInstanceGraph
  (iri: IRI,
   instantiatedTGraphs: Iterable[types.ImmutableModelTerminologyGraph],
   extendedIGraphs: Iterable[instances.ImmutableModelInstanceGraph])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.MutableModelInstanceGraph =
    store.makeInstanceGraph(iri, instantiatedTGraphs, extendedIGraphs)

  override def saveInstanceGraph
  (g: instances.ModelInstanceGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit =
    g.save

  override def saveInstanceGraph
  (g: instances.ModelInstanceGraph, os: java.io.OutputStream)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit =
    g.save(os)

}

trait OWLAPIImmutableTerminologyGraphOps
  extends ImmutableTerminologyGraphOps[OWLAPIOMF] {
  self: OWLAPIOMFOps =>

  override def getTerminologyGraphIRI
  (graph: types.ModelTerminologyGraph) =
    graph.iri

  def getTerminologyGraphShortName
  (graph: types.ModelTerminologyGraph) =
    graph.getTerminologyGraphShortName

  def getTerminologyGraphUUID
  (graph: types.ModelTerminologyGraph) =
    graph.getTerminologyGraphUUID

  override def getTerminologyGraphKind
  (graph: types.ModelTerminologyGraph) =
    graph.kind

  override def lookupTypeTerm
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    graph.lookupTypeTerm(iri, recursively)

  override def lookupEntityDefinition
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelEntityDefinition) => Some(t)
      case _                                    => None
    }

  override def lookupEntityAspect
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelEntityAspect) => Some(t)
      case _                                => None
    }

  override def lookupEntityConcept
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelEntityConcept) => Some(t)
      case _                                 => None
    }

  override def lookupEntityReifiedRelationship
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelEntityReifiedRelationship) => Some(t)
      case _                                             => None
    }

  override def lookupEntityUnreifiedRelationship
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelEntityUnreifiedRelationship) => Some(t)
      case _                                               => None
    }

  override def lookupScalarDataType
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelScalarDataType) => Some(t)
      case _                                  => None
    }

  override def lookupStructuredDataType
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelStructuredDataType) => Some(t)
      case _                                      => None
    }

  override def lookupEntityDataRelationshipFromEntityToScalar
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelDataRelationshipFromEntityToScalar) => Some(t)
      case _                                                      => None
    }

  override def lookupEntityDataRelationshipFromEntityToStructure
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelDataRelationshipFromEntityToStructure) => Some(t)
      case _                                                         => None
    }

  override def lookupEntityDataRelationshipFromStructureToScalar
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelDataRelationshipFromStructureToScalar) => Some(t)
      case _                                                         => None
    }

  override def lookupEntityDataRelationshipFromStructureToStructure
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore) =
    lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelDataRelationshipFromStructureToStructure) => Some(t)
      case _                                                            => None
    }

  override def getTermAxioms
  (graph: types.ModelTerminologyGraph)
  : ( IRI, Iterable[types.ModelTermAxiom] ) =
    graph.getTermAxioms

  override def getTypeTerms
  (graph: types.ModelTerminologyGraph)
  : ( IRI, Iterable[types.ModelTypeTerm] ) =
    graph.getTypeTerms

  override def lookupNestingAxiomForNestedChildIfAny
  (nestedG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.TerminologyGraphDirectNestingAxiom]
  = store.lookupNestingAxiomForNestedChildIfAny(nestedG)

  override def lookupNestingAxiomsForNestingContext
  (nestingC: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[types.TerminologyGraphDirectNestingAxiom]
  = store.lookupNestingAxiomsForNestingContext(nestingC)

  override def lookupNestingAxiomsForNestingParent
  (nestingG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[types.TerminologyGraphDirectNestingAxiom]
  = store.lookupNestingAxiomsForNestingParent(nestingG)

  override def getNestingGraph
  (graph: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelTerminologyGraph] =
    store.getNestingGraph(graph)

  override def getNestedGraphs
  (graph: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Iterable[types.ModelTerminologyGraph] =
    store.getNestedGraphs(graph)

  def foldTerm[T]
  (t: types.ModelTypeTerm)
  (funEntityConcept: types.ModelEntityConcept => T,
   funEntityReifiedRelationship: types.ModelEntityReifiedRelationship => T,
   funEntityUnreifiedRelationship: types.ModelEntityUnreifiedRelationship => T,
   funScalarDataType: types.ModelScalarDataType => T,
   funStructuredDataType: types.ModelStructuredDataType => T,
   funDataRelationshipFromEntityToScalar: types.ModelDataRelationshipFromEntityToScalar => T,
   funDataRelationshipFromEntityToStructure: types.ModelDataRelationshipFromEntityToStructure => T,
   funDataRelationshipFromStructureToScalar: types.ModelDataRelationshipFromStructureToScalar => T,
   funDataRelationshipFromStructureToStructure: types.ModelDataRelationshipFromStructureToStructure => T)
  : T = t match {
    case et: types.ModelEntityConcept                             => funEntityConcept(et)
    case et: types.ModelEntityReifiedRelationship                 => funEntityReifiedRelationship(et)
    case et: types.ModelEntityUnreifiedRelationship               => funEntityUnreifiedRelationship(et)
    case ed: types.ModelScalarDataType                            => funScalarDataType(ed)
    case ed: types.ModelStructuredDataType                        => funStructuredDataType(ed)
    case esc: types.ModelDataRelationshipFromEntityToScalar       => funDataRelationshipFromEntityToScalar(esc)
    case est: types.ModelDataRelationshipFromEntityToStructure    => funDataRelationshipFromEntityToStructure(est)
    case ssc: types.ModelDataRelationshipFromStructureToScalar    => funDataRelationshipFromStructureToScalar(ssc)
    case sst: types.ModelDataRelationshipFromStructureToStructure => funDataRelationshipFromStructureToStructure(sst)
  }

  override def getTermShortName
  (graph: types.ModelTerminologyGraph,
   term: types.ModelTypeTerm)
  : Option[String]
  = graph
    .getTermShortNameAnnotationAssertionAxiom(term)
    .flatMap { a =>
      a.getValue match {
        case l: OWLLiteral =>
          Some(l.getLiteral)
        case _ =>
          None
      }
    }

  override def getTermShortUUID
  (graph: types.ModelTerminologyGraph,
   term: types.ModelTypeTerm)
  : Option[String]
  = graph
    .getTermUUIDAnnotationAssertionAxiom(term)
    .flatMap { a =>
      a.getValue match {
        case l: OWLLiteral =>
          Some(l.getLiteral)
        case _ =>
          None
      }
    }

  override def fromEntityDefinition
  (e: types.ModelEntityDefinition) =
    e match {
      case ec: types.ModelEntityConcept             => fromEntityConcept(ec).iri
      case er: types.ModelEntityReifiedRelationship => fromEntityReifiedRelationship(er).iri
    }

  // entity facet

  override def fromEntityAspect
  (f: types.ModelEntityAspect)
  : IRI = f.iri

  // entity concept

  override def fromEntityConcept
  (c: types.ModelEntityConcept) =
    OWLAPIEntityConceptSignature(c.iri, c.isAbstract)

  // entity relationship

  override def fromEntityReifiedRelationship
  (r: types.ModelEntityReifiedRelationship) =
    OWLAPIEntityReifiedRelationshipSignature(r.iri, r.source, r.target, r.characteristics, r.isAbstract)


  override def fromEntityUnreifiedRelationship
  (r: types.ModelEntityUnreifiedRelationship) =
    OWLAPIEntityUnreifiedRelationshipSignature(r.iri, r.source, r.target, r.characteristics)


  // datatype definition

  override def fromDataTypeDefinition
  (dt: types.ModelDataTypeDefinition)
  : IRI = dt match {
    case d: types.ModelScalarDataType     => fromScalarDataType(d)
    case d: types.ModelStructuredDataType => fromStructuredDataType(d)
  }

  // scalar datatype

  override def fromScalarDataType
  (dt: types.ModelScalarDataType) = dt.iri

  // structured datatype

  override def fromStructuredDataType
  (dt: types.ModelStructuredDataType)
  : IRI = dt.iri

  // data relationship from entity to scalar

  override def fromDataRelationshipFromEntityToScalar
  (esc: types.ModelDataRelationshipFromEntityToScalar) = {
    import esc._
    (iri, source, target)
  }

  // data relationship from entity to structure

  override def fromDataRelationshipFromEntityToStructure
  (est: types.ModelDataRelationshipFromEntityToStructure) = {
    import est._
    (iri, source, target)
  }

  // data relationship from structure to scalar

  override def fromDataRelationshipFromStructureToScalar
  (ssc: types.ModelDataRelationshipFromStructureToScalar) = {
    import ssc._
    (iri, source, target)
  }

  // data relationship from structure to structure

  override def fromDataRelationshipFromStructureToStructure
  (sst: types.ModelDataRelationshipFromStructureToStructure) = {
    import sst._
    (iri, source, target)
  }

  // model term axioms

  override def foldTermAxiom[T]
  (t: types.ModelTermAxiom)
  (funEntityDefinitionAspectSubClassAxiom
   : types.EntityDefinitionAspectSubClassAxiom => T,
   funEntityConceptDesignationTerminologyGraphAxiom
   : types.EntityConceptDesignationTerminologyGraphAxiom => T,
   funEntityConceptSubClassAxiom
   : types.EntityConceptSubClassAxiom => T,
   funEntityConceptRestrictionAxiom
   : types.EntityConceptRestrictionAxiom => T,
   funEntityReifiedRelationshipSubClassAxiom
   : types.EntityReifiedRelationshipSubClassAxiom => T,
   funEntityReifiedRelationshipContextualizationAxiom
   : types.EntityReifiedRelationshipContextualizationAxiom => T,
   funEntityReifiedRelationshipRestrictionAxiom
   : types.EntityReifiedRelationshipRestrictionAxiom => T,
   funScalarDataTypeFacetRestriction
   : types.ScalarDataTypeFacetRestrictionAxiom => T,
   funModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
   : types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral => T)
  : T = t match {
    case ax: types.EntityDefinitionAspectSubClassAxiom =>
      funEntityDefinitionAspectSubClassAxiom(ax)
    case ax: types.EntityConceptDesignationTerminologyGraphAxiom =>
      funEntityConceptDesignationTerminologyGraphAxiom(ax)
    case ax: types.EntityConceptSubClassAxiom =>
      funEntityConceptSubClassAxiom(ax)
    case ax: types.EntityConceptRestrictionAxiom =>
      funEntityConceptRestrictionAxiom(ax)
    case ax: types.EntityReifiedRelationshipSubClassAxiom =>
      funEntityReifiedRelationshipSubClassAxiom(ax)
    case ax: types.EntityReifiedRelationshipRestrictionAxiom =>
      funEntityReifiedRelationshipRestrictionAxiom(ax)
    case ax: types.ScalarDataTypeFacetRestrictionAxiom =>
      funScalarDataTypeFacetRestriction(ax)
    case ax: types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral =>
      funModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(ax)
  }

  // scalar data relationship restriction from entity to literal axiom

  override def fromModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  ( ax: types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral )
  : ( types.ModelEntityDefinition, types.ModelDataRelationshipFromEntityToScalar, String )
  = ( ax.restrictedEntity, ax.restrictingDataProperty, ax.literalRestriction )

  // entity definition aspect subclass axiom

  override def fromEntityDefinitionAspectSubClassAxiom
  (ax: types.EntityDefinitionAspectSubClassAxiom)
  : (types.ModelEntityDefinition, types.ModelEntityAspect) = {
    import ax._
    (sub, sup)
  }


  // entity concept designation terminology graph axiom

  override def fromEntityConceptDesignationTerminologyGraphAxiom
  (ax: types.EntityConceptDesignationTerminologyGraphAxiom)
  : (types.ModelEntityConcept, types.ModelTerminologyGraph) = {
    import ax._
    (entityConceptDesignation, designationTerminologyGraph)
  }

  // entity concept subclass axiom

  override def fromEntityConceptSubClassAxiom
  (ax: types.EntityConceptSubClassAxiom)
  : (types.ModelEntityConcept, types.ModelEntityConcept) = {
    import ax._
    (sub, sup)
  }

  // entity concept restriction axiom

  override def fromEntityConceptRestrictionAxiom
  (ax: types.EntityConceptRestrictionAxiom)
  : (types.ModelEntityConcept, types.ModelEntityReifiedRelationship, types.ModelEntityDefinition) = {
    import ax._
    (sub, rel, range)
  }

  // entity relationship subclass axiom

  override def fromEntityReifiedRelationshipSubClassAxiom
  (ax: types.EntityReifiedRelationshipSubClassAxiom)
  : (types.ModelEntityReifiedRelationship, types.ModelEntityReifiedRelationship) = {
    import ax._
    (sub, sup)
  }

  override def fromEntityReifiedRelationshipContextualizationAxiom
  (ax: types.EntityReifiedRelationshipContextualizationAxiom)
  : (types.ModelEntityDefinition, types.ModelEntityReifiedRelationship, String, types.ModelEntityDefinition)
  = (ax.domain, ax.rel, ax.contextName, ax.range)

  override def fromEntityReifiedRelationshipRestrictionAxiom
  (ax: types.EntityReifiedRelationshipRestrictionAxiom)
  : (types.ModelEntityDefinition, types.ModelEntityReifiedRelationship, types.ModelEntityDefinition, RestrictionKind) = {
    import ax._
    ax match {
      case _ : types.EntityReifiedRelationshipExistentialRestrictionAxiom =>
        (domain, rel, range, ExistentialRestrictionKind)
      case _ : types.EntityReifiedRelationshipUniversalRestrictionAxiom =>
        (domain, rel, range, UniversalRestrictionKind)

    }

  }

  // scalar datatype facet restriction axiom

  override def fromScalarDataTypeFacetRestrictionAxiom
  (ax: types.ScalarDataTypeFacetRestrictionAxiom)
  : (types.ModelScalarDataType,
     types.ModelScalarDataType,
     Iterable[FundamentalFacet],
     Iterable[ConstrainingFacet]) = {
    import ax._
    (sub, sup, fundamentalFacets, constrainingFacets)
  }
}

trait OWLAPIMutableTerminologyGraphOps
  extends MutableTerminologyGraphOps[OWLAPIOMF]
          with OWLAPIImmutableTerminologyGraphOps {
  self: OWLAPIOMFOps =>

  override def setTerminologyGraphShortName
  (graph: types.MutableModelTerminologyGraph,
   name: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit =
    graph.setTerminologyGraphShortName(name)

  override def setTerminologyGraphUUID
  (graph: types.MutableModelTerminologyGraph,
   uuid: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit =
    graph.setTerminologyGraphUUID(uuid)

  override def setTermShortName
  (graph: types.MutableModelTerminologyGraph,
   term: types.ModelTypeTerm,
   name: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit =
    graph.setTermShortName(term, name)

  override def setTermUUID
  (graph: types.MutableModelTerminologyGraph,
   term: types.ModelTypeTerm,
   uuid: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit =
    graph.setTermUUID(term, uuid)

  // entity facet

  override def addEntityAspect
  (graph: types.MutableModelTerminologyGraph,
   aspectName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityAspect =
    for {
      aspectIRI <- withFragment(graph.iri, aspectName)
      result <- graph.addEntityAspect(aspectIRI)
      _ <- store.registerOMFModelEntityAspectInstance(graph, result)
    } yield result

  // entity concept

  override def addEntityConcept
  (graph: types.MutableModelTerminologyGraph,
   conceptName: String,
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityConcept =
    for {
      conceptIRI <- withFragment(graph.iri, conceptName)
      result <- graph.addEntityConcept(conceptIRI, isAbstract)
      _ <- store.registerOMFModelEntityConceptInstance(graph, result)
    } yield result

  // entity relationship

  override def addEntityReifiedRelationship
  (graph: types.MutableModelTerminologyGraph,
   source: types.ModelEntityDefinition,
   target: types.ModelEntityDefinition,
   characteristics: Iterable[RelationshipCharacteristics],
   reifiedRelationshipName: String,
   unreifiedRelationshipName: String,
   unreifiedInverseRelationshipName: Option[String],
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityReifiedRelationship =
    for {
      rIRI <- withFragment(graph.iri, reifiedRelationshipName)
      rIRISource = toSourceIRI(rIRI)
      rIRITarget = toTargetIRI(rIRI)
      uIRI <- withFragment(graph.iri, unreifiedRelationshipName)
      uiIRI <- unreifiedInverseRelationshipName.fold[Set[java.lang.Throwable] \/ Option[IRI]](\/-(None)) { uName =>
        withFragment(graph.iri, uName).map(Some(_))
      }
      result <- graph.addEntityReifiedRelationship(
                                                    rIRI,
                                                    rIRISource, rIRITarget,
                                                    uIRI, uiIRI,
                                                    source, target,
                                                    characteristics, isAbstract)
      _ <- store.registerOMFModelEntityReifiedRelationshipInstance(graph, result)
    } yield result

  // scalar datatype

  override def addScalarDataType
  (graph: types.MutableModelTerminologyGraph,
   scalarName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelScalarDataType =
    for {
      scalarIRI <- withFragment(graph.iri, scalarName)
      result <- graph.addScalarDataType(scalarIRI)
      _ <- store.registerOMFModelScalarDataTypeInstance(graph, result)
    } yield result

  // structured datatype

  override def addStructuredDataType
  (graph: types.MutableModelTerminologyGraph,
   structureName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelStructuredDataType =
    for {
      structureIRI <- withFragment(graph.iri, structureName)
      result <- graph.addStructuredDataType(structureIRI)
      _ <- store.registerOMFModelStructuredDataTypeInstance(graph, result)
    } yield result

  // data relationship from entity to scalar

  override def addDataRelationshipFromEntityToScalar
  (graph: types.MutableModelTerminologyGraph,
   source: types.ModelEntityDefinition,
   target: types.ModelScalarDataType,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore) =
    for {
      dIRI <- withFragment(graph.iri, dataRelationshipName)
      d <- graph.addDataRelationshipFromEntityToScalar(dIRI, source, target)
    } yield d

  override def addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  ( graph: types.MutableModelTerminologyGraph,
    entityDomain: types.ModelEntityDefinition,
    scalaDataProperty: types.ModelDataRelationshipFromEntityToScalar,
    literalRange: String )
  ( implicit store: OWLAPIOMFGraphStore )
  : Set[java.lang.Throwable] \/ types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  = graph.addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral( entityDomain, scalaDataProperty, literalRange )

  // data relationship from entity to structure

  override def addDataRelationshipFromEntityToStructure
  (graph: types.MutableModelTerminologyGraph,
   source: types.ModelEntityDefinition,
   target: types.ModelStructuredDataType,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore) =
    for {
      dIRI <- withFragment(graph.iri, dataRelationshipName)
      d <- graph.addDataRelationshipFromEntityToStructure(dIRI, source, target)
    } yield d

  // data relationship from structure to scalar

  override def addDataRelationshipFromStructureToScalar
  (graph: types.MutableModelTerminologyGraph,
   source: types.ModelStructuredDataType,
   target: types.ModelScalarDataType,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore) =
    for {
      dIRI <- withFragment(graph.iri, dataRelationshipName)
      d <- graph.addDataRelationshipFromStructureToScalar(dIRI, source, target)
    } yield d

  // data relationship from structure to structure

  override def addDataRelationshipFromStructureToStructure
  (graph: types.MutableModelTerminologyGraph,
   source: types.ModelStructuredDataType,
   target: types.ModelStructuredDataType,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore) =
    for {
      dIRI <- withFragment(graph.iri, dataRelationshipName)
      d <- graph.addDataRelationshipFromStructureToStructure(dIRI, source, target)
    } yield d

  // model term axioms

  // entity definition aspect subclass axiom

  override def addEntityDefinitionAspectSubClassAxiom
  (graph: types.MutableModelTerminologyGraph,
   sub: types.ModelEntityDefinition,
   sup: types.ModelEntityAspect)
  (implicit store: OWLAPIOMFGraphStore) =
    graph.addEntityDefinitionAspectSubClassAxiom(sub, sup)

  // entity concept subclass axiom

  override def addEntityConceptDesignationTerminologyGraphAxiom
  (graph: types.MutableModelTerminologyGraph,
   entityConceptDesignation: types.ModelEntityConcept,
   designationTerminologyGraph: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityConceptDesignationTerminologyGraphAxiom =
    graph.addEntityConceptDesignationTerminologyGraphAxiom(entityConceptDesignation, designationTerminologyGraph)

  override def addEntityConceptSubClassAxiom
  (graph: types.MutableModelTerminologyGraph,
   sub: types.ModelEntityConcept,
   sup: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityConceptSubClassAxiom =
    graph.addEntityConceptSubClassAxiom(sub, sup)

  // entity concept restriction axioms

  override def addEntityConceptUniversalRestrictionAxiom
  (graph: types.MutableModelTerminologyGraph,
   sub: types.ModelEntityConcept,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityConceptUniversalRestrictionAxiom =
    graph.addEntityConceptUniversalRestrictionAxiom(sub, rel, range)

  override def addEntityConceptExistentialRestrictionAxiom
  (graph: types.MutableModelTerminologyGraph,
   sub: types.ModelEntityConcept,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityConceptExistentialRestrictionAxiom =
    graph.addEntityConceptExistentialRestrictionAxiom(sub, rel, range)

  // entity relationship subclass axiom

  override def addEntityReifiedRelationshipSubClassAxiom
  (graph: types.MutableModelTerminologyGraph,
   sub: types.ModelEntityReifiedRelationship,
   sup: types.ModelEntityReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore) =
    graph.addEntityReifiedRelationshipSubClassAxiom(sub, sup)

  override def addEntityReifiedRelationshipContextualizationAxiom
  (graph: types.MutableModelTerminologyGraph,
   domain: types.ModelEntityDefinition,
   rel: types.ModelEntityReifiedRelationship,
   contextName: String,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  = graph.addEntityReifiedRelationshipContextualizationAxiom(domain, rel, contextName, range)

  override def addEntityReifiedRelationshipExistentialRestrictionAxiom
  (graph: types.MutableModelTerminologyGraph,
   domain: types.ModelEntityDefinition,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  = graph.addEntityReifiedRelationshipExistentialRestrictionAxiom(domain, rel, range)

  override def addEntityReifiedRelationshipUniversalRestrictionAxiom
  (graph: types.MutableModelTerminologyGraph,
   domain: types.ModelEntityDefinition,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  = graph.addEntityReifiedRelationshipUniversalRestrictionAxiom(domain, rel, range)

  // scalar datatype facet restriction axiom

  override def addScalarDataTypeFacetRestrictionAxiom
  (graph: types.MutableModelTerminologyGraph,
   sub: types.ModelScalarDataType,
   sup: types.ModelScalarDataType,
   fundamentalFacets: Iterable[FundamentalFacet],
   constrainingFacets: Iterable[ConstrainingFacet] )
  (implicit store: OWLAPIOMFGraphStore) =
    graph.addScalarDataTypeFacetRestrictionAxiom(sub, sup, fundamentalFacets, constrainingFacets)

  override def addTerminologyGraphExtension
  (extendingG: types.MutableModelTerminologyGraph,
   extendedG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom
  = extendingG.addTerminologyGraphExtension(extendedG)

  override def addNestedTerminologyGraph
  (nestingParent: types.MutableModelTerminologyGraph,
   nestingContext: types.ModelEntityConcept,
   nestedChild: types.ModelTerminologyGraph )
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectNestingAxiom
  = nestingParent.addNestedTerminologyGraph(nestingContext, nestedChild)

}

trait OWLAPIImmutableInstanceGraphOps
  extends ImmutableInstanceGraphOps[OWLAPIOMF] {

  override def getInstanceGraphIRI
  (graph: instances.ModelInstanceGraph) =
    graph.iri

  // instance object

  override def fromInstanceObject
  (o: instances.ModelInstanceObject) = {
    import o._
    (iri, conceptType)
  }

  // instance relation

  override def fromInstanceRelation
  (r: instances.ModelInstanceRelation) = {
    import r._
    (iri, relationshipType, source, target)
  }

  // data literal

  override def fromDataLiteral
  (dl: instances.ModelInstanceDataLiteral) = {
    import dl._
    (lexicalForm, datatype)
  }

  // data structure

  override def fromDataStructure
  (ds: instances.ModelInstanceDataStructure) = {
    import ds._
    (iri, datatype)
  }

  // data property from entity to scalar

  override def fromInstanceDataRelationshipFromEntityToScalar
  (e2sc: instances.ModelInstanceDataRelationshipFromEntityToScalar) = {
    import e2sc._
    (ei, dataRelationship, value)
  }

  // data property from entity to structure

  override def fromInstanceDataRelationshipFromEntityToStructure
  (e2st: instances.ModelInstanceDataRelationshipFromEntityToStructure) = {
    import e2st._
    (ei, dataRelationship, value)
  }

  // data property from structure to scalar

  override def fromInstanceDataRelationshipFromStructureToScalar
  (s2sc: instances.ModelInstanceDataRelationshipFromStructureToScalar) = {
    import s2sc._
    (di, dataRelationship, value)
  }

  // data property from structure to structure

  override def fromInstanceDataRelationshipFromStructureToStructure
  (s2st: instances.ModelInstanceDataRelationshipFromStructureToStructure) = {
    import s2st._
    (di, dataRelationship, value)
  }
}

trait OWLAPIMutableInstanceGraphOps
  extends MutableInstanceGraphOps[OWLAPIOMF]
          with OWLAPIImmutableInstanceGraphOps {

  // instance object

  override def addInstanceObject
  (graph: instances.MutableModelInstanceGraph,
   conceptType: types.ModelEntityConcept,
   fragment: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ModelInstanceObject =
    ???

  // instance relation

  override def addInstanceRelation
  (graph: instances.MutableModelInstanceGraph,
   relationshipType: types.ModelEntityReifiedRelationship,
   source: instances.ModelEntityInstance,
   target: instances.ModelEntityInstance,
   fragment: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ModelInstanceRelation =
    ???

  // data literal

  override def addDataLiteral
  (graph: instances.MutableModelInstanceGraph,
   datatype: types.ModelScalarDataType,
   lexicalForm: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ModelInstanceDataLiteral =
    ???

  // data structure

  override def addDataStructure
  (graph: instances.MutableModelInstanceGraph,
   datatype: types.ModelStructuredDataType,
   fragment: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ModelInstanceDataStructure =
    ???

  // data property from entity to scalar

  override def addInstanceDataRelationshipFromEntityToScalar
  (graph: instances.MutableModelInstanceGraph,
   ei: instances.ModelEntityInstance,
   e2sc: types.ModelDataRelationshipFromEntityToScalar,
   value: instances.ModelInstanceDataLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ModelInstanceDataRelationshipFromEntityToScalar =
    ???

  // data property from entity to structure

  override def addInstanceDataRelationshipFromEntityToStructure
  (graph: instances.MutableModelInstanceGraph,
   ei: instances.ModelEntityInstance,
   e2st: types.ModelDataRelationshipFromEntityToStructure,
   value: instances.ModelInstanceDataStructure)
  (implicit store: OWLAPIOMFGraphStore) =
    ???

  // data property from structure to scalar

  override def addInstanceDataRelationshipFromStructureToScalar
  (graph: instances.MutableModelInstanceGraph,
   di: instances.ModelInstanceDataStructure,
   e2sc: types.ModelDataRelationshipFromStructureToScalar,
   value: instances.ModelInstanceDataLiteral)
  (implicit store: OWLAPIOMFGraphStore) =
    ???

  // data property from structure to structure

  override def addInstanceDataRelationshipFromStructureToStructure
  (graph: instances.MutableModelInstanceGraph,
   di: instances.ModelInstanceDataStructure,
   e2st: types.ModelDataRelationshipFromStructureToStructure,
   value: instances.ModelInstanceDataStructure)
  (implicit store: OWLAPIOMFGraphStore) =
    ???
}

class OWLAPIOMFOps
( val rdfs_label: IRI,
  val AnnotationHasUUID: IRI,
  val AnnotationHasRelativeIRI: IRI,
  val AnnotationHasIRIHashPrefix: IRI,
  val AnnotationHasIRIHashSuffix: IRI,
  val AnnotationIsAbstract: IRI,
  val AnnotationIsDerived: IRI,
  val AnnotationIsDefinition: IRI,
  val AnnotationIsDesignation: IRI,
  val AnnotationIsToplevel: IRI,
  val AnnotationHasContext: IRI,
  val AnnotationHasGraph: IRI,
  val AnnotationHasRestrictedSourceProperty: IRI,
  val AnnotationHasRestrictedTargetProperty: IRI)
  extends OMFOps[OWLAPIOMF]
          with OWLAPIIRIOps
          with OWLAPIMutableTerminologyGraphOps
          with OWLAPIMutableInstanceGraphOps
          with OWLAPIStoreOps {

}

final class OWLOntologyOps
(val ont: OWLOntology)
(implicit val ops: OWLAPIOMFOps) {

  def isOntologyTBoxToplevel: Boolean = {
    for {
      aaa <- ont.getAnnotations
      if aaa.getProperty.getIRI == ops.AnnotationIsToplevel
    } {
      aaa.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          return l.parseBoolean
        case _ =>
          ()
      }
    }

    false
  }

  def isOntologyTBoxDefinition: Boolean = {
    for {
      aaa <- ont.getAnnotations
      if aaa.getProperty.getIRI == ops.AnnotationIsDefinition
    } {
      aaa.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          return l.parseBoolean
        case _ =>
          ()
      }
    }

    true
  }

  def isOntologyTBoxDesignation: Boolean = {
    for {
      aaa <- ont.getAnnotations
      if aaa.getProperty.getIRI == ops.AnnotationIsDesignation
    } {
      aaa.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          return l.parseBoolean
        case _ =>
          ()
      }
    }

    false
  }

}

sealed abstract class IRIArgumentException(val message: String)
  extends java.lang.IllegalArgumentException(message) {
  require(null != message)
}

case class IRIFragmentException(val iri: IRI)
  extends IRIArgumentException(s"withFragment(iri=${iri}) -- the IRI already has a fragment") {
  require(null != iri)
}

case class IRIObjectPropertyException(val iri: IRI)
  extends IRIArgumentException(s"toObjectProperty(iri=${iri}) -- the IRI must have a fragment") {
  require(null != iri)
}

case class IRISourcePropertyException(val iri: IRI)
  extends IRIArgumentException(s"toSourceIRI(iri=${iri}) -- the IRI must have a fragment") {
  require(null != iri)
}

case class IRIargetPropertyException(val iri: IRI)
  extends IRIArgumentException(s"toTargetIRI(iri=${iri}) -- the IRI must have a fragment") {
  require(null != iri)
}