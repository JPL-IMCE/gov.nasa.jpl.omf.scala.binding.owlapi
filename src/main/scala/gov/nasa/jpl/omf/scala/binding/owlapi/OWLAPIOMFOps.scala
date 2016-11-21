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

import java.net.URI
import java.util.UUID

import gov.nasa.jpl.imce.omf.schema.tables.LocalName
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils
import org.semanticweb.owlapi.model._

import scala.{Boolean, None, Option, Some, StringContext, Tuple3, Unit}
import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.util.control.Exception._
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
      .apply {
        org.semanticweb.owlapi.model.IRI.create(s).right
      }
}

trait OWLAPIIRIOps
  extends IRIOps[OWLAPIOMF] {

  def iri2hash(prefix: String, iri: IRI)
  : String
  = Hex.encodeHexString(DigestUtils.sha1(prefix + iri.toString))

  // IRI

  override def makeIRI
  (s: String)
  : Set[java.lang.Throwable] \/IRI
  = OWLAPIIRIOps.makeIRI(s)

  override def withFragment
  (iri: IRI, fragment: String)
  : Set[java.lang.Throwable] \/ IRI
  = {
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
  (iri: IRI, lowercaseFragmentInitial: Boolean)
  = splitIRI(iri) match {
      case (_, None)           => None
      case (i, Some(fragment)) =>
        val path = i.toURI.getSchemeSpecificPart
        val slash = path.lastIndexOf('/')
        val last = path.substring(slash + 1)
        val fragmentInitial = if (lowercaseFragmentInitial) fragment.head.toLower else fragment.head
        val fragmentTail = fragment.tail
        Some(last + ":" + fragmentInitial + fragmentTail)
    }

  def getFragment(iri: IRI)
  : Set[java.lang.Throwable] \/ String
  = Option.apply(iri.toURI.getFragment) match {
    case None =>
      Set(OMFError.omfBindingError(s"getFragment($iri): error: there should be a fragment!")).left
    case Some(f) =>
      f.right
  }

  def lastSegment(iri: IRI)
  : Set[java.lang.Throwable] \/ LocalName
  = if (Option.apply(iri.toURI.getFragment).isDefined)
      Set(OMFError.omfBindingError(s"lastSegment($iri): error: there should not be a fragment!")).left
    else
      \/-(iri.getShortForm)

  override def fromIRI
  (iri: IRI) = iri.toString

  override def isBackboneIRI
  (iri: IRI)
  = {
    val u = iri.toURI
    u.getHost == "imce.jpl.nasa.gov" && u.getPath.startsWith("/backbone")
  }

  override def toBackboneIRI
  (iri: IRI)
  = {
    val u = iri.toURI
    IRI.create(
      new URI(
        u.getScheme, u.getUserInfo, "imce.jpl.nasa.gov", u.getPort,
        "/backbone/" + u.getHost + u.getPath, u.getQuery, u.getFragment))
  }

  override def toSourceIRI
  (iri: IRI)
  = splitIRI(iri) match {
      case (iri, Some(f)) =>
        val fragment = s"has${f}Source"
        org.semanticweb.owlapi.model.IRI.create(iri.toURI.resolve("#" + fragment))
      case (iri, None)    =>
        throw IRISourcePropertyException(iri)
    }

  override def toTargetIRI
  (iri: IRI)
  = splitIRI(iri) match {
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

  override def lookupTerminologyGraph
  (uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelTerminologyGraph]
  = store.lookupTerminologyGraph(uuid)

  override def loadBuiltinDatatypeMap
  ()
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/
    (types.ImmutableModelTerminologyGraph, types.Mutable2ImmutableTerminologyMap) =
    store.loadBuiltinDatatypeMap

  override def loadTerminologyGraph
  (iri: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2ImmutableTerminologyMap)
  = store.loadTerminologyGraph(iri)

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
  : OWLAPITerminologyGraphSignature
  = store.fromTerminologyGraph(graph)

  def getTerminologyGraphAxiomUUID
  (ax: types.TerminologyGraphAxiom)
  (implicit store: OWLAPIOMFGraphStore)
  : UUID
  = ax.uuid

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

  override def getNestingContextConceptOfAxiom
  (axiom: types.TerminologyGraphDirectNestingAxiom)
  (implicit store: OWLAPIOMFGraphStore)
  : types.ModelEntityConcept
  = store.getNestingContextConceptOfAxiom(axiom)

  override def getDirectlyExtendedGraphsOfExtendingChildGraph
  (extendingChildG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[types.TerminologyGraphDirectExtensionAxiom]
  = store.getDirectlyExtendedGraphsOfExtendingChildGraph(extendingChildG)

  def makeTerminologyGraphWithPath
  (uuid: UUID,
   name: LocalName,
   iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.MutableModelTerminologyGraph
  = store.makeTerminologyGraph(
    uuid, name, iri, relativeIRIPath, relativeIRIHashPrefix, kind, extraProvenanceMetadata)(this)

  override def makeTerminologyGraph
  (uuid: UUID,
   name: LocalName,
   iri: IRI,
   kind: TerminologyKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.MutableModelTerminologyGraph
  = makeTerminologyGraphWithPath(
    uuid, name, iri,
    relativeIRIPath=Option.empty[String],
    relativeIRIHashPrefix=Option.empty[String],
    kind,
    extraProvenanceMetadata=Option.empty[OTI2OMFModelTerminologyGraphProvenance])(store)

  override def saveTerminologyGraph
  (g: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  = store.saveTerminologyGraph(g)(this)

  override def saveTerminologyGraph
  (g: types.ModelTerminologyGraph,
   os: java.io.OutputStream)
  (implicit store: OWLAPIOMFGraphStore)
  = store.saveTerminologyGraph(g, os)(this)

  override def asImmutableTerminologyGraph
  (g: types.MutableModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  = store.asImmutableTerminologyGraph(Map(), g)

  override def asImmutableTerminologyGraph
  (m2i: types.Mutable2ImmutableTerminologyMap,
   g: types.MutableModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  = store.asImmutableTerminologyGraph(m2i, g)

  def createOMFTerminologyGraph
  (o: OWLOntology,
   ont: OWLOntology,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind.TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.MutableModelTerminologyGraph
  = store.createOMFModelTerminologyGraph(
      o, ont.getOntologyID.getOntologyIRI.get,
      relativeIRIPath, relativeIRIHashPrefix, ont, kind, extraProvenanceMetadata)

  override def loadInstanceGraph
  (iri: IRI)
  (implicit store: OWLAPIOMFGraphStore):
  Set[java.lang.Throwable] \/ instances.ImmutableModelInstanceGraph
  = store.loadInstanceGraph(iri)

  override def fromInstanceGraph
  (graph: instances.ModelInstanceGraph)
  = graph.fromInstanceGraph

  override def asImmutableInstanceGraph
  (g: instances.MutableModelInstanceGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ImmutableModelInstanceGraph
  = store.asImmutableInstanceGraph(g)

  override def makeInstanceGraph
  (iri: IRI,
   instantiatedTGraphs: Iterable[types.ImmutableModelTerminologyGraph],
   extendedIGraphs: Iterable[instances.ImmutableModelInstanceGraph])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.MutableModelInstanceGraph
  = store.makeInstanceGraph(iri, instantiatedTGraphs, extendedIGraphs)

  override def saveInstanceGraph
  (g: instances.ModelInstanceGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit
  = g.save

  override def saveInstanceGraph
  (g: instances.ModelInstanceGraph, os: java.io.OutputStream)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit
  = g.save(os)

}

trait OWLAPIImmutableTerminologyGraphOps
  extends ImmutableTerminologyGraphOps[OWLAPIOMF] {
  self: OWLAPIOMFOps =>

  override def getTerminologyGraphIRI
  (graph: types.ModelTerminologyGraph)
  = graph.iri

  def getTerminologyGraphLocalName
  (graph: types.ModelTerminologyGraph)
  : LocalName
  = graph.name

  def getTerminologyGraphUUID
  (graph: types.ModelTerminologyGraph)
  : UUID
  = graph.uuid

  override def getTerminologyGraphKind
  (graph: types.ModelTerminologyGraph)
  : TerminologyKind
  = graph.kind

  override def lookupTypeTerm
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelTypeTerm]
  = graph.lookupTypeTerm(iri, recursively)

  override def lookupEntityDefinition
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelEntityDefinition]
  = lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelEntityDefinition) => Some(t)
      case _                                    => None
    }

  override def lookupEntityAspect
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelEntityAspect]
  = lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelEntityAspect) => Some(t)
      case _                                => None
    }

  override def lookupEntityConcept
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelEntityConcept]
  = lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelEntityConcept) => Some(t)
      case _                                 => None
    }

  override def lookupEntityReifiedRelationship
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelEntityReifiedRelationship]
  = lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelEntityReifiedRelationship) => Some(t)
      case _                                             => None
    }

  override def lookupEntityUnreifiedRelationship
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelEntityUnreifiedRelationship]
  = lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelEntityUnreifiedRelationship) => Some(t)
      case _                                               => None
    }

  override def lookupScalarDataType
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelScalarDataType]
  = lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelScalarDataType) => Some(t)
      case _                                  => None
    }

  override def lookupStructuredDataType
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelStructuredDataType]
  = lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelStructuredDataType) => Some(t)
      case _                                      => None
    }

  override def lookupEntityDataRelationshipFromEntityToScalar
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelDataRelationshipFromEntityToScalar]
  = lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelDataRelationshipFromEntityToScalar) => Some(t)
      case _                                                      => None
    }

  override def lookupEntityDataRelationshipFromEntityToStructure
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelDataRelationshipFromEntityToStructure]
  = lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelDataRelationshipFromEntityToStructure) => Some(t)
      case _                                                         => None
    }

  override def lookupEntityDataRelationshipFromStructureToScalar
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelDataRelationshipFromStructureToScalar]
  = lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelDataRelationshipFromStructureToScalar) => Some(t)
      case _                                                         => None
    }

  override def lookupEntityDataRelationshipFromStructureToStructure
  (graph: types.ModelTerminologyGraph, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[types.ModelDataRelationshipFromStructureToStructure]
  = lookupTypeTerm(graph, iri, recursively) match {
      case Some(t: types.ModelDataRelationshipFromStructureToStructure) => Some(t)
      case _                                                            => None
    }

  override def getTermAxiomUUID
  (graph: types.ModelTerminologyGraph,
   ax: types.ModelTermAxiom)
  : UUID
  = ax.uuid

  override def getTermAxioms
  (graph: types.ModelTerminologyGraph)
  : ( IRI, Iterable[types.ModelTermAxiom] )
  = graph.getTermAxioms

  override def getTypeTerms
  (graph: types.ModelTerminologyGraph)
  : ( IRI, Iterable[types.ModelTypeTerm] )
  = graph.getTypeTerms

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

  def foldTerm[T]
  (t: types.ModelTypeTerm)
  (funEntityAspect: types.ModelEntityAspect => T,
   funEntityConcept: types.ModelEntityConcept => T,
   funEntityReifiedRelationship: types.ModelEntityReifiedRelationship => T,
   funEntityUnreifiedRelationship: types.ModelEntityUnreifiedRelationship => T,
   funScalarDataType: types.ModelScalarDataType => T,
   funStructuredDataType: types.ModelStructuredDataType => T,
   funDataRelationshipFromEntityToScalar: types.ModelDataRelationshipFromEntityToScalar => T,
   funDataRelationshipFromEntityToStructure: types.ModelDataRelationshipFromEntityToStructure => T,
   funDataRelationshipFromStructureToScalar: types.ModelDataRelationshipFromStructureToScalar => T,
   funDataRelationshipFromStructureToStructure: types.ModelDataRelationshipFromStructureToStructure => T)
  : T = t match {
    case et: types.ModelEntityAspect                              => funEntityAspect(et)
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

  override def getTermLocalName
  (graph: types.ModelTerminologyGraph,
   term: types.ModelTypeTerm)
  : LocalName
  = graph
    .getTermLocalNameAnnotationAssertionAxiom(term)
    .flatMap { a =>
      a.getValue match {
        case l: OWLLiteral =>
          Some(l.getLiteral)
        case _ =>
          None
      }
    }
    .fold[LocalName]({
    throw OMFError.omfBindingError(s"Missing LocalName annotation on OMF term: ${term.iri}")
  })(identity)

  override def getTermUUID
  (graph: types.ModelTerminologyGraph,
   term: types.ModelTypeTerm)
  : UUID
  = term.uuid

  override def fromEntityDefinition
  (e: types.ModelEntityDefinition)
  : IRI
  = e match {
    case ea: types.ModelEntityAspect =>
      fromEntityAspect(ea)
    case ec: types.ModelEntityConcept =>
      fromEntityConcept(ec).iri
    case er: types.ModelEntityReifiedRelationship =>
      fromEntityReifiedRelationship(er).iri
  }

  // entity facet

  override def fromEntityAspect
  (f: types.ModelEntityAspect)
  : IRI = f.iri

  // entity concept

  override def fromEntityConcept
  (c: types.ModelEntityConcept)
  : OWLAPIEntityConceptSignature
  = OWLAPIEntityConceptSignature(c.uuid, c.name, c.iri, c.isAbstract)

  // entity relationship

  override def fromEntityReifiedRelationship
  (r: types.ModelEntityReifiedRelationship)
  : OWLAPIEntityReifiedRelationshipSignature
  = OWLAPIEntityReifiedRelationshipSignature(r.uuid, r.name, r.iri, r.source, r.target, r.characteristics, r.isAbstract)


  override def fromEntityUnreifiedRelationship
  (r: types.ModelEntityUnreifiedRelationship)
  : OWLAPIEntityUnreifiedRelationshipSignature
  = OWLAPIEntityUnreifiedRelationshipSignature(r.uuid, r.name, r.iri, r.source, r.target, r.characteristics)


  // datatype definition

  override def fromDataTypeDefinition
  (dt: types.ModelDataTypeDefinition)
  : (UUID, LocalName, IRI)
  = dt match {
    case d: types.ModelScalarDataType =>
      fromScalarDataType(d)
    case d: types.ModelStructuredDataType =>
      fromStructuredDataType(d)
  }

  // scalar datatype

  override def fromScalarDataType
  (dt: types.ModelScalarDataType)
  : (UUID, LocalName, IRI)
  = Tuple3(dt.uuid, dt.name, dt.iri)

  // structured datatype

  override def fromStructuredDataType
  (dt: types.ModelStructuredDataType)
  : (UUID, LocalName, IRI)
  = Tuple3(dt.uuid, dt.name, dt.iri)

  // data relationship from entity to scalar

  override def fromDataRelationshipFromEntityToScalar
  (esc: types.ModelDataRelationshipFromEntityToScalar)
  : (UUID, LocalName, IRI, types.ModelEntityDefinition, types.ModelScalarDataType)
  = {
    import esc._
    (uuid, name, iri, source, target)
  }

  // data relationship from entity to structure

  override def fromDataRelationshipFromEntityToStructure
  (est: types.ModelDataRelationshipFromEntityToStructure)
  : (UUID, LocalName, IRI, types.ModelEntityDefinition, types.ModelStructuredDataType)
  = {
    import est._
    (uuid, name, iri, source, target)
  }

  // data relationship from structure to scalar

  override def fromDataRelationshipFromStructureToScalar
  (ssc: types.ModelDataRelationshipFromStructureToScalar)
  : (UUID, LocalName, IRI, types.ModelStructuredDataType, types.ModelScalarDataType)
  = {
    import ssc._
    (uuid, name, iri, source, target)
  }

  // data relationship from structure to structure

  override def fromDataRelationshipFromStructureToStructure
  (sst: types.ModelDataRelationshipFromStructureToStructure)
  : (UUID, LocalName, IRI, types.ModelStructuredDataType, types.ModelStructuredDataType)
  = {
    import sst._
    (uuid, name, iri, source, target)
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
   funEntityDefinitionRestrictionAxiom
   : types.EntityDefinitionRestrictionAxiom => T,
   funEntityReifiedRelationshipSubClassAxiom
   : types.EntityReifiedRelationshipSubClassAxiom => T,
   funScalarDataTypeFacetRestriction
   : types.ScalarDataTypeFacetRestrictionAxiom => T,
   funModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
   : types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral => T)
  : T
  = t match {
    case ax: types.EntityDefinitionAspectSubClassAxiom =>
      funEntityDefinitionAspectSubClassAxiom(ax)
    case ax: types.EntityConceptDesignationTerminologyGraphAxiom =>
      funEntityConceptDesignationTerminologyGraphAxiom(ax)
    case ax: types.EntityConceptSubClassAxiom =>
      funEntityConceptSubClassAxiom(ax)
    case ax: types.EntityDefinitionRestrictionAxiom =>
      funEntityDefinitionRestrictionAxiom(ax)
    case ax: types.EntityReifiedRelationshipSubClassAxiom =>
      funEntityReifiedRelationshipSubClassAxiom(ax)
    case ax: types.ScalarDataTypeFacetRestrictionAxiom =>
      funScalarDataTypeFacetRestriction(ax)
    case ax: types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral =>
      funModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(ax)
  }

  // scalar data relationship restriction from entity to literal axiom

  override def fromModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  ( ax: types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral )
  : ( UUID, types.ModelEntityDefinition, types.ModelDataRelationshipFromEntityToScalar, String )
  = ( ax.uuid, ax.restrictedEntity, ax.restrictingDataProperty, ax.literalRestriction )

  // entity definition aspect subclass axiom

  override def fromEntityDefinitionAspectSubClassAxiom
  (ax: types.EntityDefinitionAspectSubClassAxiom)
  : (UUID, types.ModelEntityDefinition, types.ModelEntityAspect)
  = (ax.uuid, ax.sub, ax.sup)

  // entity concept designation terminology graph axiom

  override def fromEntityConceptDesignationTerminologyGraphAxiom
  (ax: types.EntityConceptDesignationTerminologyGraphAxiom)
  : (UUID, types.ModelEntityConcept, types.ModelTerminologyGraph)
  = (ax.uuid, ax.entityConceptDesignation, ax.designationTerminologyGraph)

  // entity concept subclass axiom

  override def fromEntityConceptSubClassAxiom
  (ax: types.EntityConceptSubClassAxiom)
  : (UUID, types.ModelEntityConcept, types.ModelEntityConcept)
  = (ax.uuid, ax.sub, ax.sup)

  // entity concept restriction axiom

  override def fromEntityDefinitionRestrictionAxiom
  (ax: types.EntityDefinitionRestrictionAxiom)
  : (UUID, types.ModelEntityDefinition, types.ModelEntityReifiedRelationship, types.ModelEntityDefinition, RestrictionKind)
  = (ax.uuid,
    ax.sub,
     ax.rel,
     ax.range,
     ax match {
       case _: types.EntityDefinitionExistentialRestrictionAxiom =>
         ExistentialRestrictionKind
       case _: types.EntityDefinitionUniversalRestrictionAxiom =>
         UniversalRestrictionKind
     })

  // entity relationship subclass axiom

  override def fromEntityReifiedRelationshipSubClassAxiom
  (ax: types.EntityReifiedRelationshipSubClassAxiom)
  : (UUID, types.ModelEntityReifiedRelationship, types.ModelEntityReifiedRelationship)
  = {
    import ax._
    (uuid, sub, sup)
  }

  // scalar datatype facet restriction axiom

  override def fromScalarDataTypeFacetRestrictionAxiom
  (ax: types.ScalarDataTypeFacetRestrictionAxiom)
  : (UUID, types.ModelScalarDataType,
     types.ModelScalarDataType,
     Iterable[FundamentalFacet],
     Iterable[ConstrainingFacet])
  = {
    import ax._
    (uuid, sub, sup, fundamentalFacets, constrainingFacets)
  }
}

trait OWLAPIMutableTerminologyGraphOps
  extends MutableTerminologyGraphOps[OWLAPIOMF]
          with OWLAPIImmutableTerminologyGraphOps {
  self: OWLAPIOMFOps =>

  // entity facet

  override def addEntityAspect
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   aspectName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityAspect
  = for {
      aspectIRI <- withFragment(graph.iri, aspectName)
      result <- graph.addEntityAspect(aspectIRI, aspectName, uuid)
      _ <- store.registerOMFModelEntityAspectInstance(graph, result)
    } yield result

  // entity concept

  override def addEntityConcept
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   conceptName: String,
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityConcept
  = for {
      conceptIRI <- withFragment(graph.iri, conceptName)
      result <- graph.addEntityConcept(conceptIRI, conceptName, uuid, isAbstract)
      _ <- store.registerOMFModelEntityConceptInstance(graph, result)
    } yield result

  // entity relationship

  override def addEntityReifiedRelationship
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   source: types.ModelEntityDefinition,
   target: types.ModelEntityDefinition,
   characteristics: Iterable[RelationshipCharacteristics],
   reifiedRelationshipName: String,
   unreifiedRelationshipName: String,
   unreifiedInverseRelationshipName: Option[String],
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityReifiedRelationship
  = for {
      rIRI <- withFragment(graph.iri, reifiedRelationshipName)
      rIRISource = toSourceIRI(rIRI)
      rIRITarget = toTargetIRI(rIRI)
      uIRI <- withFragment(graph.iri, unreifiedRelationshipName)
      uiIRI <- unreifiedInverseRelationshipName.fold[Set[java.lang.Throwable] \/ Option[IRI]](\/-(None)) { uName =>
        withFragment(graph.iri, uName).map(Some(_))
      }
      result <- graph.addEntityReifiedRelationship(
        rIRI, reifiedRelationshipName, uuid,
        rIRISource, rIRITarget,
        uIRI, uiIRI,
        source, target,
        characteristics, isAbstract)
      _ <- store.registerOMFModelEntityReifiedRelationshipInstance(graph, result)
    } yield result

  // scalar datatype

  override def addScalarDataType
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   scalarName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelScalarDataType
  = for {
      scalarIRI <- withFragment(graph.iri, scalarName)
      result <- graph.addScalarDataType(scalarIRI, scalarName, uuid)
      _ <- store.registerOMFModelScalarDataTypeInstance(graph, result)
    } yield result

  // structured datatype

  override def addStructuredDataType
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   structureName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelStructuredDataType
  = for {
      structureIRI <- withFragment(graph.iri, structureName)
      result <- graph.addStructuredDataType(structureIRI, structureName, uuid)
      _ <- store.registerOMFModelStructuredDataTypeInstance(graph, result)
    } yield result

  // data relationship from entity to scalar

  override def addDataRelationshipFromEntityToScalar
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   source: types.ModelEntityDefinition,
   target: types.ModelScalarDataType,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore)
  = for {
      dIRI <- withFragment(graph.iri, dataRelationshipName)
      d <- graph.addDataRelationshipFromEntityToScalar(dIRI, dataRelationshipName, uuid, source, target)
    } yield d

  override def addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  ( graph: types.MutableModelTerminologyGraph,
    uuid: UUID,
    entityDomain: types.ModelEntityDefinition,
    scalaDataProperty: types.ModelDataRelationshipFromEntityToScalar,
    literalRange: String )
  ( implicit store: OWLAPIOMFGraphStore )
  : Set[java.lang.Throwable] \/ types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  = graph.addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(uuid, entityDomain, scalaDataProperty, literalRange )

  // data relationship from entity to structure

  override def addDataRelationshipFromEntityToStructure
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   source: types.ModelEntityDefinition,
   target: types.ModelStructuredDataType,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToStructure
  = for {
      dIRI <- withFragment(graph.iri, dataRelationshipName)
      d <- graph.addDataRelationshipFromEntityToStructure(dIRI, dataRelationshipName, uuid, source, target)
    } yield d

  // data relationship from structure to scalar

  override def addDataRelationshipFromStructureToScalar
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   source: types.ModelStructuredDataType,
   target: types.ModelScalarDataType,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromStructureToScalar
  = for {
      dIRI <- withFragment(graph.iri, dataRelationshipName)
      d <- graph.addDataRelationshipFromStructureToScalar(dIRI, dataRelationshipName, uuid, source, target)
    } yield d

  // data relationship from structure to structure

  override def addDataRelationshipFromStructureToStructure
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   source: types.ModelStructuredDataType,
   target: types.ModelStructuredDataType,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromStructureToStructure
  = for {
      dIRI <- withFragment(graph.iri, dataRelationshipName)
      d <- graph.addDataRelationshipFromStructureToStructure(dIRI, dataRelationshipName, uuid, source, target)
    } yield d

  // model term axioms

  // entity definition aspect subclass axiom

  override def addEntityDefinitionAspectSubClassAxiom
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   sub: types.ModelEntityDefinition,
   sup: types.ModelEntityAspect)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionAspectSubClassAxiom
  = graph.addEntityDefinitionAspectSubClassAxiom(uuid, sub, sup)

  // entity concept subclass axiom

  override def addEntityConceptDesignationTerminologyGraphAxiom
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   entityConceptDesignation: types.ModelEntityConcept,
   designationTerminologyGraph: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityConceptDesignationTerminologyGraphAxiom
  = graph.addEntityConceptDesignationTerminologyGraphAxiom(uuid, entityConceptDesignation, designationTerminologyGraph)

  override def addEntityConceptSubClassAxiom
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   sub: types.ModelEntityConcept,
   sup: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityConceptSubClassAxiom
  = graph.addEntityConceptSubClassAxiom(uuid, sub, sup)

  // entity concept restriction axioms

  override def addEntityDefinitionUniversalRestrictionAxiom
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   sub: types.ModelEntityDefinition,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionUniversalRestrictionAxiom
  = graph.addEntityDefinitionUniversalRestrictionAxiom(uuid, sub, rel, range)

  override def addEntityDefinitionExistentialRestrictionAxiom
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   sub: types.ModelEntityDefinition,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionExistentialRestrictionAxiom
  = graph.addEntityDefinitionExistentialRestrictionAxiom(uuid, sub, rel, range)

  // entity relationship subclass axiom

  override def addEntityReifiedRelationshipSubClassAxiom
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   sub: types.ModelEntityReifiedRelationship,
   sup: types.ModelEntityReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityReifiedRelationshipSubClassAxiom
  = graph.addEntityReifiedRelationshipSubClassAxiom(uuid, sub, sup)

  /*
  override def addEntityReifiedRelationshipExistentialRestrictionAxiom
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   domain: types.ModelEntityDefinition,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  = graph.addEntityReifiedRelationshipExistentialRestrictionAxiom(uuid, domain, rel, range)

  override def addEntityReifiedRelationshipUniversalRestrictionAxiom
  (graph: types.MutableModelTerminologyGraph,
   uuid: UUID,
   domain: types.ModelEntityDefinition,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  = graph.addEntityReifiedRelationshipUniversalRestrictionAxiom(uuid, domain, rel, range)
*/

  // scalar datatype facet restriction axiom

  override def addScalarDataTypeFacetRestrictionAxiom
  (uuid: UUID,
   graph: types.MutableModelTerminologyGraph,
   sub: types.ModelScalarDataType,
   sup: types.ModelScalarDataType,
   fundamentalFacets: Iterable[FundamentalFacet],
   constrainingFacets: Iterable[ConstrainingFacet] )
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ScalarDataTypeFacetRestrictionAxiom
  = graph.addScalarDataTypeFacetRestrictionAxiom(uuid, sub, sup, fundamentalFacets, constrainingFacets)

  override def addTerminologyGraphExtension
  (uuid: UUID,
   extendingG: types.MutableModelTerminologyGraph,
   extendedG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom
  = extendingG.addTerminologyGraphExtension(uuid, extendedG)

  override def addNestedTerminologyGraph
  (uuid: UUID,
   nestingGraph: types.ModelTerminologyGraph,
   nestingContext: types.ModelEntityConcept,
   nestedChild: types.MutableModelTerminologyGraph )
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectNestingAxiom
  = nestedChild.addNestedTerminologyGraph(uuid, nestingGraph, nestingContext)

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
  val AnnotationHasID: IRI,
  val AnnotationHasURL: IRI,
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

  def getTermLocalNameFromAssertionOrFromIRI
  (ont: OWLOntology,
   termIRI: IRI)
  : Set[java.lang.Throwable] \/ LocalName
  = findAnnotationAssertionAxiom(ont, termIRI, rdfs_label)
    .fold {
      getFragment(termIRI)
    }{ ax =>
      ax.getValue match {
        case l: OWLLiteral =>
          \/-(l.getLiteral)
        case _ =>
          -\/(Set[java.lang.Throwable](OMFError.omfBindingError(s"Missing LocalName annotation on OMF term: $termIRI")))
      }
    }

  def getTermUUIDFromAssertionOrFromIRI
  (ont: OWLOntology,
   termIRI: IRI)
  : Set[java.lang.Throwable] \/ UUID
  = findAnnotationAssertionAxiom(ont, termIRI, AnnotationHasUUID)
    .fold[Set[java.lang.Throwable] \/ UUID] {
    \/-(generateUUID(fromIRI(termIRI)))
  } { ax =>
    ax.getValue match {
      case l: OWLLiteral =>
        nonFatalCatch[Set[java.lang.Throwable] \/ UUID]
          .withApply { cause: java.lang.Throwable =>
            Set[java.lang.Throwable](cause).left
          }
          .apply {
            \/-(UUID.fromString(l.getLiteral))
          }
      case _ =>
        -\/(Set[java.lang.Throwable](OMFError.omfBindingError(s"Missing LocalName annotation on OMF term: $termIRI")))
    }
  }

  def isAnnotatedAbstract
  (ont: OWLOntology,
   termIRI: IRI)
  : Set[java.lang.Throwable] \/ Boolean
  = findAnnotationAssertionAxiom(ont, termIRI, AnnotationIsAbstract)
    .fold[Set[java.lang.Throwable] \/ Boolean] {
    \/-(false)
  } { ax =>
    ax.getValue match {
      case l: OWLLiteral if l.isBoolean =>
        \/-(l.parseBoolean)
      case _ =>
        -\/(Set[java.lang.Throwable](OMFError.omfBindingError(s"Invalid 'isAbstract' annotation on $termIRI")))
    }
  }

  def isAnnotatedDerived
  (ont: OWLOntology,
   termIRI: IRI)
  : Set[java.lang.Throwable] \/ Boolean
  = findAnnotationAssertionAxiom(ont, termIRI, AnnotationIsDerived)
    .fold[Set[java.lang.Throwable] \/ Boolean]{
    \/-(false)
  } { ax =>
    ax.getValue match {
      case l: OWLLiteral if l.isBoolean =>
        \/-(l.parseBoolean)
      case _ =>
        -\/(Set[java.lang.Throwable](OMFError.omfBindingError(s"Invalid 'isDerived' annotation on $termIRI")))
    }
  }
}

final class OWLOntologyOps
(val ont: OWLOntology)
(implicit val ops: OWLAPIOMFOps) {

  def isOntologyTBoxDefinition: Boolean = {
    for {
      aaa <- ont.annotations.toScala[Set]
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
      aaa <- ont.annotations.toScala[Set]
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