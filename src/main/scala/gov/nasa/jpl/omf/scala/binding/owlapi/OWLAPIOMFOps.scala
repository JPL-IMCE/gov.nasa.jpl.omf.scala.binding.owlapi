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

import java.io.File
import java.net.URI
import java.util.UUID

import gov.nasa.jpl.imce.omf.schema.tables.{Annotation, AnnotationProperty, LexicalValue, LocalName}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.ImmutableTerminologyConversionMap
import gov.nasa.jpl.omf.scala.binding.owlapi.types.bundleStatements.ConceptTreeDisjunction
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms.{BundledTerminologyAxiom, TerminologyAxiom, TerminologyBoxAxiom, TerminologyExtensionAxiom, TerminologyNestingAxiom}
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind
import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils
import org.semanticweb.owlapi.model._

import scala.{Boolean, Int, None, Option, Some, StringContext, Unit}
import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.util.control.Exception._
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._

object OWLAPIIRIOps {

  def makeIRI
  (s: String)
  : Set[java.lang.Throwable] \/IRI
  = nonFatalCatch[Unit]
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

  def getFragment(iri: IRI)
  : Set[java.lang.Throwable] \/ String
  = Option.apply(iri.toURI.getFragment) match {
    case None =>
      Set(OMFError.omfBindingError(s"getFragment($iri): error: there should be a fragment!")).left
    case Some(f) =>
      f.right
  }

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
  (iri: IRI)
  : (IRI, Option[String])
  = {
    val u = iri.toURI
    u.getFragment match {
      case f: String if f.nonEmpty =>
        (org.semanticweb.owlapi.model.IRI.create(new URI(u.getScheme, u.getSchemeSpecificPart, null)),
          Some(f))
      case _ =>
        (iri,
          None)
    }
  }

  override def toAbbreviatedName
  (iri: IRI, lowercaseFragmentInitial: Boolean)
  : Option[String]
  = splitIRI(iri) match {
    case (_, None) => None
    case (i, Some(fragment)) =>
      val path = i.toURI.getSchemeSpecificPart
      val slash = path.lastIndexOf('/')
      val last = path.substring(slash + 1)
      val fragmentInitial = if (lowercaseFragmentInitial) fragment.head.toLower else fragment.head
      val fragmentTail = fragment.tail
      Some(last + ":" + fragmentInitial + fragmentTail)
  }

  def lastSegment(iri: IRI)
  : Set[java.lang.Throwable] \/ LocalName
  = if (Option.apply(iri.toURI.getFragment).isDefined)
      Set(OMFError.omfBindingError(s"lastSegment($iri): error: there should not be a fragment!")).left
    else
      \/-(iri.getShortForm)

  override def fromIRI
  (iri: IRI)
  : String
  = iri.toString

  override def isBackboneIRI
  (iri: IRI)
  : Boolean
  = {
    val u = iri.toURI
    u.getHost == "imce.jpl.nasa.gov" && u.getPath.startsWith("/backbone")
  }

  override def toBackboneIRI
  (iri: IRI)
  : IRI
  = {
    val u = iri.toURI
    IRI.create(
      new URI(
        u.getScheme, u.getUserInfo, "imce.jpl.nasa.gov", u.getPort,
        "/backbone/" + u.getHost + u.getPath, u.getQuery, u.getFragment))
  }

  override def toSourceIRI
  (iri: IRI)
  : IRI
  = splitIRI(iri) match {
    case (iri, Some(f)) =>
      val fragment = s"has${f}Source"
      org.semanticweb.owlapi.model.IRI.create(iri.toURI.resolve("#" + fragment))
    case (iri, None) =>
      throw IRISourcePropertyException(iri)
  }

  override def toTargetIRI
  (iri: IRI)
  : IRI
  = splitIRI(iri) match {
    case (iri, Some(f)) =>
      val fragment = s"has${f}Target"
      org.semanticweb.owlapi.model.IRI.create(iri.toURI.resolve("#" + fragment))
    case (iri, None) =>
      throw IRIargetPropertyException(iri)
  }

}

trait OWLAPIStoreOps
  extends OMFStoreOps[OWLAPIOMF] {
  self: OWLAPIOMFOps =>

  override def getTerminologyThingUUID
  (th: OWLAPIOMF#TerminologyThing)
  (implicit store: OWLAPIOMFGraphStore)
  : UUID
  = th.uuid

  override def annotationProperties
  ()
  (implicit store: OWLAPIOMFGraphStore)
  : Seq[AnnotationProperty]
  = store.annotationProperties()

  override def addAnnotationProperty
  (ap: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ AnnotationProperty
  = store.addAnnotationProperty(ap)

  override def foldTerminology[T]
  (funImmutableTerminologyGraph: OWLAPIOMF#ImmutableTerminologyGraph => T,
   funMutableTerminologyGraph: OWLAPIOMF#MutableTerminologyGraph => T,
   funImmutableTerminologyBundle: OWLAPIOMF#ImmutableBundle => T,
   funMutableTerminologyBundle: OWLAPIOMF#MutableBundle => T)
  (t: OWLAPIOMF#TerminologyBox)
  : T
  = t match {
    case g: OWLAPIOMF#ImmutableTerminologyGraph =>
      funImmutableTerminologyGraph(g)
    case g: OWLAPIOMF#MutableTerminologyGraph =>
      funMutableTerminologyGraph(g)
    case b: OWLAPIOMF#ImmutableBundle =>
      funImmutableTerminologyBundle(b)
    case b: OWLAPIOMF#MutableBundle =>
      funMutableTerminologyBundle(b)
  }

  override def lookupTerminology
  (iri: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[TerminologyBox]
  = store.lookupTerminology(iri)

  override def lookupTerminology
  (uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[TerminologyBox]
  = store.lookupTerminology(uuid)

  override def loadBuiltinDatatypeMap
  ()
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ ImmutableTerminologyConversionMap
  = store.loadBuiltinDatatypeMap()

  override def loadTerminology
  (iri: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ ImmutableTerminologyConversionMap
  = store.loadTerminologyGraph(iri)

  override def isTerminologyMutable
  ( graph: TerminologyBox )
  ( implicit store: OWLAPIOMFGraphStore )
  : Boolean
  = graph.isMutable

  override def toMutableTerminology
  ( graph: TerminologyBox )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[MutableTerminologyBox]
  = graph match {
    case g: MutableTerminologyBox =>
      Some(g)
    case _ =>
      None
  }

  override def isTerminologyImmutable
  ( graph: TerminologyBox )
  ( implicit store: OWLAPIOMFGraphStore )
  : Boolean
  = graph.isImmutable

  override def toImmutableTerminology
  ( graph: TerminologyBox )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ImmutableTerminologyBox]
  = graph match {
    case g: ImmutableTerminologyBox =>
      Some(g)
    case _ =>
      None
  }

  override def getTerminologyIRI
  (graph: OWLAPIOMF#TerminologyBox)
  : IRI
  = graph.iri

  override def fromTerminology
  (graph: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : OWLAPITerminologySignature
  = store.fromTerminology(graph)

  override def getTerminologyAxiomUUID
  (ax: TerminologyAxiom)
  (implicit store: OWLAPIOMFGraphStore)
  : UUID
  = ax.uuid

  /**
    * Find the axiom TerminologyGraphDirectNestingAxiom(nestedChild==nestedG), if any.
    */
  override def lookupNestingAxiomForNestedChildIfAny
  (nestedG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[TerminologyNestingAxiom]
  = store.lookupNestingAxiomForNestedChildIfAny(nestedG)

  /**
    * Find the axioms TerminologyGraphDirectNestingAxiom(nestingContext=nestingC).
    */
  override def lookupNestingAxiomsForNestingContext
  (nestingC: Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[TerminologyNestingAxiom]
  = store.lookupNestingAxiomsForNestingContext(nestingC)

  override def getExtensionAxioms
  (extendingChildG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[TerminologyExtensionAxiom]
  = store.getExtensionAxioms(extendingChildG)

  def makeTerminologyGraphWithPath
  (uuid: UUID,
   name: LocalName,
   iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ MutableTerminologyGraph
  = store.makeTerminologyGraph(
    uuid, name, iri, relativeIRIPath, relativeIRIHashPrefix, kind, extraProvenanceMetadata)(this)

  override def makeTerminologyGraph
  (uuid: UUID,
   name: LocalName,
   iri: IRI,
   kind: TerminologyKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ MutableTerminologyGraph
  = makeTerminologyGraphWithPath(
    uuid, name, iri,
    relativeIRIPath=Option.empty[String],
    relativeIRIHashPrefix=Option.empty[String],
    kind,
    extraProvenanceMetadata=Option.empty[OTI2OMFModelTerminologyGraphProvenance])(store)

  def makeBundleWithPath
  (uuid: UUID,
   name: LocalName,
   iri: IRI,
   kind: TerminologyKind,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ MutableBundle
  = store.makeBundle(
    uuid, name, iri, relativeIRIPath, relativeIRIHashPrefix, kind, extraProvenanceMetadata)(this)

  override def makeBundle
  (uuid: UUID,
   name: LocalName,
   iri: IRI,
   kind: TerminologyKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ MutableBundle
  = makeBundleWithPath(
    uuid, name, iri,
    relativeIRIPath=Option.empty[String],
    relativeIRIHashPrefix=Option.empty[String],
    kind=kind,
    extraProvenanceMetadata=Option.empty[OTI2OMFModelTerminologyGraphProvenance])(store)

  override def saveTerminology
  (g: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit
  = store.saveTerminology(g)(this)

  override def saveTerminology
  (g: TerminologyBox,
   os: java.io.OutputStream)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit
  = store.saveTerminology(g, os)(this)

  override def asImmutableTerminology
  (g: MutableTerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/
    (types.terminologies.ImmutableTerminologyBox, types.Mutable2ImmutableTerminologyMap)
  = store.asImmutableTerminology(Map(), g)

  override def asImmutableTerminology
  (m2i: types.Mutable2ImmutableTerminologyMap,
   g: MutableTerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/
    (types.terminologies.ImmutableTerminologyBox, types.Mutable2ImmutableTerminologyMap)
  = store.asImmutableTerminology(m2i, g)

  override def loadInstanceGraph
  (iri: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ImmutableModelInstanceGraph
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
   instantiatedTGraphs: Iterable[ImmutableTerminologyBox],
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

  override def resolveIRIAsLocalFile
  (iri: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ File
  = store.resolveIRIAsLocalFile(iri)
}

trait OWLAPIImmutableTerminologyGraphOps
  extends ImmutableTerminologyGraphOps[OWLAPIOMF] {
  self: OWLAPIOMFOps =>

  override def getAnnotations
  (graph: OWLAPIOMF#TerminologyBox)
  : Map[AnnotationProperty, Seq[Annotation]]
  = graph.getAnnotations()

  override def getTerminologyName
  (graph: TerminologyBox)
  : LocalName
  = graph.name

  override def getTerminologyUUID
  (graph: TerminologyBox)
  : UUID
  = graph.uuid

  override def getTerminologyKind
  (graph: OWLAPIOMF#TerminologyBox)
  : TerminologyKind
  = graph.kind

  override def lookupTerm
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Term]
  = graph.lookupTerm(iri, recursively)

  override def lookupEntity
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Entity]
  = lookupTerm(graph, iri, recursively) match {
    case Some(t: OWLAPIOMF#Entity) => Some(t)
    case _ => None
  }

  override def lookupAspect
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Aspect]
  = lookupTerm(graph, iri, recursively) match {
    case Some(t: OWLAPIOMF#Aspect) => Some(t)
    case _ => None
  }

  override def lookupConcept
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Concept]
  = lookupTerm(graph, iri, recursively) match {
    case Some(t: OWLAPIOMF#Concept) => Some(t)
    case _ => None
  }

  override def lookupReifiedRelationship
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#ReifiedRelationship]
  = lookupTerm(graph, iri, recursively) match {
    case Some(t: OWLAPIOMF#ReifiedRelationship) => Some(t)
    case _ => None
  }

  override def lookupUnreifiedRelationship
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#UnreifiedRelationship]
  = lookupTerm(graph, iri, recursively) match {
    case Some(t: OWLAPIOMF#UnreifiedRelationship) => Some(t)
    case _ => None
  }

  override def lookupDataRange
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#DataRange]
  = lookupTerm(graph, iri, recursively) match {
      case Some(t: OWLAPIOMF#DataRange) => Some(t)
      case _ => None
    }

  override def restrictedDataRangeOf
  (dr: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#DataRange]
  = dr match {
    case _: OWLAPIOMF#Scalar =>
      None
    case rdr: OWLAPIOMF#RestrictedDataRange =>
      Some(rdr.restrictedDataRange)
  }

  override def lookupStructure
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Structure]
  = lookupTerm(graph, iri, recursively) match {
      case Some(t: OWLAPIOMF#Structure) => Some(t)
      case _ => None
    }

  override def lookupEntityScalarDataProperty
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#EntityScalarDataProperty]
  = lookupTerm(graph, iri, recursively) match {
      case Some(t: OWLAPIOMF#EntityScalarDataProperty) => Some(t)
      case _ => None
    }

  override def lookupEntityStructuredDataProperty
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#EntityStructuredDataProperty]
  = lookupTerm(graph, iri, recursively) match {
      case Some(t: OWLAPIOMF#EntityStructuredDataProperty) => Some(t)
      case _ => None
    }

  override def lookupScalarDataProperty
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#ScalarDataProperty]
  = lookupTerm(graph, iri, recursively) match {
      case Some(t: OWLAPIOMF#ScalarDataProperty) => Some(t)
      case _ => None
    }

  override def lookupStructuredDataProperty
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#StructuredDataProperty]
  = lookupTerm(graph, iri, recursively) match {
      case Some(t: OWLAPIOMF#StructuredDataProperty) => Some(t)
      case _ => None
    }

  override def getAxiomUUID
  (ax: OWLAPIOMF#Axiom)
  : UUID
  = ax.uuid

  override def getAxioms
  (graph: OWLAPIOMF#TerminologyBox)
  : ( IRI, Iterable[OWLAPIOMF#Axiom] )
  = graph.getTermAxioms

  override def getTerms
  (graph: OWLAPIOMF#TerminologyBox)
  : ( IRI, Iterable[OWLAPIOMF#Term] )
  = graph.getTypeTerms

  override def getConceptTreeDisjunctionUUID
  (ctd: ConceptTreeDisjunction)
  : UUID
  = ctd.uuid

  override def lookupNestingAxiomForNestedChildIfAny
  (nestedG: OWLAPIOMF#TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#TerminologyNestingAxiom]
  = store.lookupNestingAxiomForNestedChildIfAny(nestedG)

  override def lookupNestingAxiomsForNestingContext
  (nestingC: OWLAPIOMF#Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[OWLAPIOMF#TerminologyNestingAxiom]
  = store.lookupNestingAxiomsForNestingContext(nestingC)

  def foldTerm[T]
  (funEntityAspect: OWLAPIOMF#Aspect => T,
   funEntityConcept: OWLAPIOMF#Concept => T,
   funEntityReifiedRelationship: OWLAPIOMF#ReifiedRelationship => T,
   funEntityUnreifiedRelationship: OWLAPIOMF#UnreifiedRelationship => T,
   funScalar: OWLAPIOMF#Scalar => T,
   funStructure: OWLAPIOMF#Structure => T,
   funScalarOneOfRestriction: OWLAPIOMF#ScalarOneOfRestriction => T,
   funBinaryScalarRestriction: OWLAPIOMF#BinaryScalarRestriction => T,
   funIRIScalarRestriction: OWLAPIOMF#IRIScalarRestriction => T,
   funPlainLiteralScalarRestriction: OWLAPIOMF#PlainLiteralScalarRestriction => T,
   funStringScalarRestriction: OWLAPIOMF#StringScalarRestriction => T,
   funTimeScalarRestriction: OWLAPIOMF#TimeScalarRestriction => T,
   funEntityScalarDataProperty: OWLAPIOMF#EntityScalarDataProperty => T,
   funEntityStructuredDataProperty: OWLAPIOMF#EntityStructuredDataProperty => T,
   funScalarDataProperty: OWLAPIOMF#ScalarDataProperty => T,
   funStructuredDataProperty: OWLAPIOMF#StructuredDataProperty => T)
  (t: types.Term)
  : T = t match {
    case et: OWLAPIOMF#Aspect =>
      funEntityAspect(et)
    case et: OWLAPIOMF#Concept =>
      funEntityConcept(et)
    case et: OWLAPIOMF#ReifiedRelationship =>
      funEntityReifiedRelationship(et)
    case et: OWLAPIOMF#UnreifiedRelationship =>
      funEntityUnreifiedRelationship(et)
    case ed: OWLAPIOMF#Scalar =>
      funScalar(ed)
    case ed: OWLAPIOMF#Structure =>
      funStructure(ed)
    case r: OWLAPIOMF#ScalarOneOfRestriction =>
      funScalarOneOfRestriction(r)
    case r: OWLAPIOMF#BinaryScalarRestriction =>
      funBinaryScalarRestriction(r)
    case r: OWLAPIOMF#IRIScalarRestriction =>
      funIRIScalarRestriction(r)
    case r: OWLAPIOMF#PlainLiteralScalarRestriction =>
      funPlainLiteralScalarRestriction(r)
    case r: OWLAPIOMF#StringScalarRestriction =>
      funStringScalarRestriction(r)
    case r: OWLAPIOMF#TimeScalarRestriction =>
      funTimeScalarRestriction(r)
    case p: OWLAPIOMF#EntityScalarDataProperty =>
      funEntityScalarDataProperty(p)
    case p: OWLAPIOMF#EntityStructuredDataProperty =>
      funEntityStructuredDataProperty(p)
    case p: OWLAPIOMF#ScalarDataProperty =>
      funScalarDataProperty(p)
    case p: OWLAPIOMF#StructuredDataProperty =>
      funStructuredDataProperty(p)
  }

  override def getTermName
  (term: OWLAPIOMF#Term)
  : LocalName
  = term.name

  override def getTermUUID
  (term: OWLAPIOMF#Term)
  : UUID
  = term.uuid

  override def getTermIRI
  (term: OWLAPIOMF#Term)
  : OWLAPIOMF#IRI
  = term.iri

  override def foldBundleStatement[T]
  (funAnonymousConceptTaxonomyAxiom: OWLAPIOMF#AnonymousConceptTaxonomyAxiom => T,
   funRootConceptTaxonomyAxiom: OWLAPIOMF#RootConceptTaxonomyAxiom => T,
   funSpecificDisjointConceptAxiom: OWLAPIOMF#SpecificDisjointConceptAxiom => T)
  (s: OWLAPIOMF#TerminologyBundleStatement)
  : T
  = s match {
    case ax: OWLAPIOMF#AnonymousConceptTaxonomyAxiom =>
      funAnonymousConceptTaxonomyAxiom(ax)
    case ax: OWLAPIOMF#RootConceptTaxonomyAxiom =>
      funRootConceptTaxonomyAxiom(ax)
    case ax: OWLAPIOMF#SpecificDisjointConceptAxiom =>
      funSpecificDisjointConceptAxiom(ax)
  }

  override def fromConcept
  (c: OWLAPIOMF#Concept)
  : OWLAPIEntityConceptSignature
  = OWLAPIEntityConceptSignature(c.uuid, c.name, c.iri, c.isAbstract)

  override def fromReifiedRelationship
  (r: OWLAPIOMF#ReifiedRelationship)
  : OWLAPIEntityReifiedRelationshipSignature
  = OWLAPIEntityReifiedRelationshipSignature(
    r.uuid, r.name, r.unreifiedPropertyName, r.inversePropertyName,
    r.iri, r.source, r.target, r.characteristics, r.isAbstract)

  override def fromUnreifiedRelationship
  (r: OWLAPIOMF#UnreifiedRelationship)
  : OWLAPIEntityUnreifiedRelationshipSignature
  = OWLAPIEntityUnreifiedRelationshipSignature(r.uuid, r.name, r.iri, r.source, r.target, r.characteristics)

  override def fromEntityScalarDataProperty
  (esc: OWLAPIOMF#EntityScalarDataProperty)
  : OWLAPIEntityScalarDataPropertySignature
  = OWLAPIEntityScalarDataPropertySignature(esc.uuid, esc.name, esc.iri, esc.domain, esc.range)


  override def fromEntityStructuredDataProperty
  (est: OWLAPIOMF#EntityStructuredDataProperty)
  : OWLAPIEntityStructuredDataPropertySignature
  = OWLAPIEntityStructuredDataPropertySignature(est.uuid, est.name, est.iri, est.domain, est.range)

  override def fromScalarDataProperty
  (ssc: OWLAPIOMF#ScalarDataProperty)
  : OWLAPIScalarDataPropertySignature
  = OWLAPIScalarDataPropertySignature(ssc.uuid, ssc.name, ssc.iri, ssc.domain, ssc.range)

  override def fromStructuredDataProperty
  (sst: OWLAPIOMF#StructuredDataProperty)
  : OWLAPIStructuredDataPropertySignature
  = OWLAPIStructuredDataPropertySignature(sst.uuid, sst.name, sst.iri, sst.domain, sst.range)

  override def foldAxiom[T]
  (funAspectSpecializationAxiom
   : OWLAPIOMF#AspectSpecializationAxiom => T,
   funConceptSpecializationAxiom
   : OWLAPIOMF#ConceptSpecializationAxiom => T,
   funEntityReifiedRelationshipSubClassAxiom
   : OWLAPIOMF#ReifiedRelationshipSpecializationAxiom => T,
   funEntityExistentialRestrictionAxiom
   : OWLAPIOMF#EntityExistentialRestrictionAxiom => T,
   funEntityUniversalRestrictionAxiom
   : OWLAPIOMF#EntityUniversalRestrictionAxiom => T,
   funEntityScalarDataPropertyExistentialRestrictionAxiom
   : OWLAPIOMF#EntityScalarDataPropertyExistentialRestrictionAxiom => T,
   funEntityScalarDataPropertyParticularRestrictionAxiom
   : OWLAPIOMF#EntityScalarDataPropertyParticularRestrictionAxiom => T,
   funEntityScalarDataPropertyUniversalRestrictionAxiom
   : OWLAPIOMF#EntityScalarDataPropertyUniversalRestrictionAxiom => T,
   funScalarOneOfLiteralAxiom
   : OWLAPIOMF#ScalarOneOfLiteralAxiom => T)
  (t: types.Axiom)
  : T
  = t match {
    case ax: OWLAPIOMF#AspectSpecializationAxiom =>
      funAspectSpecializationAxiom(ax)
    case ax: OWLAPIOMF#ConceptSpecializationAxiom =>
      funConceptSpecializationAxiom(ax)
    case ax: OWLAPIOMF#ReifiedRelationshipSpecializationAxiom =>
      funEntityReifiedRelationshipSubClassAxiom(ax)
    case ax: OWLAPIOMF#EntityExistentialRestrictionAxiom =>
      funEntityExistentialRestrictionAxiom(ax)
    case ax: OWLAPIOMF#EntityUniversalRestrictionAxiom =>
      funEntityUniversalRestrictionAxiom(ax)
    case ax: OWLAPIOMF#EntityScalarDataPropertyExistentialRestrictionAxiom =>
      funEntityScalarDataPropertyExistentialRestrictionAxiom(ax)
    case ax: OWLAPIOMF#EntityScalarDataPropertyParticularRestrictionAxiom =>
      funEntityScalarDataPropertyParticularRestrictionAxiom(ax)
    case ax: OWLAPIOMF#EntityScalarDataPropertyUniversalRestrictionAxiom =>
      funEntityScalarDataPropertyUniversalRestrictionAxiom(ax)
    case ax: OWLAPIOMF#ScalarOneOfLiteralAxiom =>
      funScalarOneOfLiteralAxiom(ax)
  }

  override def foldTerminologyBoxAxiom[T]
  (funConceptDesignationTerminologyAxiom
   : OWLAPIOMF#ConceptDesignationTerminologyAxiom => T,
   funTerminologyGraphDirectExtensionAxiom
   : OWLAPIOMF#TerminologyExtensionAxiom => T,
   funTerminologyGraphDirectNestingAxiom
   : OWLAPIOMF#TerminologyNestingAxiom => T)
  (t: TerminologyBoxAxiom)
  : T
  = t match {
    case gax: OWLAPIOMF#ConceptDesignationTerminologyAxiom =>
      funConceptDesignationTerminologyAxiom(gax)
    case gax: OWLAPIOMF#TerminologyExtensionAxiom =>
      funTerminologyGraphDirectExtensionAxiom(gax)
    case gax: OWLAPIOMF#TerminologyNestingAxiom =>
      funTerminologyGraphDirectNestingAxiom(gax)
  }

  override def foldTerminologyBundleAxiom[T]
  ( funBundledTerminologyAxiom
    : OWLAPIOMF#BundledTerminologyAxiom => T)
  (t: OWLAPIOMF#TerminologyBundleAxiom)
  : T
  = t match {
    case bx: OWLAPIOMF#BundledTerminologyAxiom =>
      funBundledTerminologyAxiom(bx)
  }

  override def fromAspectSubClassAxiom
  (ax: OWLAPIOMF#AspectSpecializationAxiom)
  : OWLAPIAspectSpecializationSignature
  = OWLAPIAspectSpecializationSignature(ax.uuid, ax.sub, ax.sup)

  override def fromConceptSpecializationAxiom
  (ax: OWLAPIOMF#ConceptSpecializationAxiom)
  : OWLAPIConceptSpecializationSignature
  = OWLAPIConceptSpecializationSignature(ax.uuid, ax.sub, ax.sup)

  override def fromReifiedRelationshipSpecializationAxiom
  (ax: OWLAPIOMF#ReifiedRelationshipSpecializationAxiom)
  : OWLAPIReifiedRelationshipSpecializationSignature
  = OWLAPIReifiedRelationshipSpecializationSignature(ax.uuid, ax.sub, ax.sup)

  override def fromEntityRestrictionAxiom
  (ax: OWLAPIOMF#EntityRestrictionAxiom)
  : OWLAPIEntityRestrictionSignature
  = OWLAPIEntityRestrictionSignature(ax.uuid, ax.restrictedDomain, ax.restrictedRelation, ax.restrictedRange)

  override def fromEntityScalarDataPropertyExistentialRestrictionAxiom
  (ax: OWLAPIOMF#EntityScalarDataPropertyExistentialRestrictionAxiom)
  : OWLAPIEntityScalarDataPropertyQuantifiedRestrictionSignature
  = OWLAPIEntityScalarDataPropertyQuantifiedRestrictionSignature(ax.uuid, ax.restrictedEntity, ax.scalarProperty, ax.scalarRestriction)

  override def fromEntityScalarDataPropertyParticularRestrictionAxiom
  (ax: OWLAPIOMF#EntityScalarDataPropertyParticularRestrictionAxiom)
  : OWLAPIEntityScalarDataPropertyParticularRestrictionSignature
  = OWLAPIEntityScalarDataPropertyParticularRestrictionSignature(ax.uuid, ax.restrictedEntity, ax.scalarProperty, ax.literalValue)

  override def fromEntityScalarDataPropertyUniversalRestrictionAxiom
  (ax: OWLAPIOMF#EntityScalarDataPropertyUniversalRestrictionAxiom)
  : OWLAPIEntityScalarDataPropertyQuantifiedRestrictionSignature
  = OWLAPIEntityScalarDataPropertyQuantifiedRestrictionSignature(ax.uuid, ax.restrictedEntity, ax.scalarProperty, ax.scalarRestriction)

  override def fromScalarOneOfLiteralAxiom
  (ax: OWLAPIOMF#ScalarOneOfLiteralAxiom)
  : OWLAPIScalarOneOfLiteralSignature
  = OWLAPIScalarOneOfLiteralSignature(ax.uuid, ax.axiom, ax.value)

  override def fromBinaryScalarRestriction
  (ax: OWLAPIOMF#BinaryScalarRestriction)
  : OWLAPIBinaryScalarRestrictionSignature
  = OWLAPIBinaryScalarRestrictionSignature(ax.uuid, ax.name, ax.iri, ax.length, ax.minLength, ax.maxLength, ax.restrictedDataRange)

  override def fromIRIScalarRestriction
  (ax: OWLAPIOMF#IRIScalarRestriction)
  : OWLAPIIRIScalarRestrictionSignature
  = OWLAPIIRIScalarRestrictionSignature(ax.uuid, ax.name, ax.iri, ax.length, ax.minLength, ax.maxLength, ax.pattern, ax.restrictedDataRange)

  override def fromNumericScalarRestriction
  (ax: OWLAPIOMF#NumericScalarRestriction)
  : OWLAPINumericScalarRestrictionSignature
  = OWLAPINumericScalarRestrictionSignature(ax.uuid, ax.name, ax.iri, ax.minInclusive, ax.maxInclusive, ax.minExclusive, ax.maxExclusive, ax.restrictedDataRange)

  override def fromPlainLiteralScalarRestriction
  (ax: OWLAPIOMF#PlainLiteralScalarRestriction)
  : OWLAPIPlainLiteralScalarRestrictionSignature
  = OWLAPIPlainLiteralScalarRestrictionSignature(ax.uuid, ax.name, ax.iri, ax.length, ax.minLength, ax.maxLength, ax.pattern, ax.language, ax.restrictedDataRange)

  override def fromScalarOneOfRestriction
  (ax: OWLAPIOMF#ScalarOneOfRestriction)
  : OWLAPIScalarOneOfRestrictionSignature
  = OWLAPIScalarOneOfRestrictionSignature(ax.uuid, ax.name, ax.iri, ax.restrictedDataRange)

  override def fromStringScalarRestriction
  (ax: OWLAPIOMF#StringScalarRestriction)
  : OWLAPIStringScalarRestrictionSignature
  = OWLAPIStringScalarRestrictionSignature(ax.uuid, ax.name, ax.iri, ax.length, ax.minLength, ax.maxLength, ax.pattern, ax.restrictedDataRange)

  override def fromSynonymScalarRestriction
  (ax: OWLAPIOMF#SynonymScalarRestriction)
  : OWLAPISynonymScalarRestrictionSignature
  = OWLAPISynonymScalarRestrictionSignature(ax.uuid, ax.name, ax.iri, ax.restrictedDataRange)

  override def fromTimeScalarRestriction
  (ax: OWLAPIOMF#TimeScalarRestriction)
  : OWLAPITimeScalarRestrictionSignature
  = OWLAPITimeScalarRestrictionSignature(ax.uuid, ax.name, ax.iri, ax.minInclusive, ax.maxInclusive, ax.minExclusive, ax.maxExclusive, ax.restrictedDataRange)

  override def fromConceptDesignationTerminologyAxiom
  (ax: OWLAPIOMF#ConceptDesignationTerminologyAxiom)
  : OWLAPIConceptDesignationTerminologySignature
  = OWLAPIConceptDesignationTerminologySignature(ax.uuid, ax.designatedConcept, ax.designatedTerminology)

  override def fromTerminologyExtensionAxiom
  (ax: OWLAPIOMF#TerminologyExtensionAxiom)
  : OWLAPITerminologyExtensionSignature
  = OWLAPITerminologyExtensionSignature(ax.uuid, ax.extendedTerminology)

  def fromTerminologyNestingAxiom
  (ax: OWLAPIOMF#TerminologyNestingAxiom)
  : OWLAPITerminologyNestingSignature
  = OWLAPITerminologyNestingSignature(ax.uuid, ax.nestingTerminology, ax.nestingContext)

  override def fromBundledTerminologyAxiom
  (ax: OWLAPIOMF#BundledTerminologyAxiom)
  : OWLAPIBundledTerminologySignature
  = OWLAPIBundledTerminologySignature(ax.uuid, ax.terminologyBundle, ax.bundledTerminology)

  override def fromAnonymousConceptTaxonomyAxiom
  (ax: OWLAPIOMF#AnonymousConceptTaxonomyAxiom)
  : OWLAPIAnonymousConceptTaxonomySignature
  = OWLAPIAnonymousConceptTaxonomySignature(ax.uuid, ax.terminologyBundle, ax.disjointTaxonomyParent)

  override def fromRootConceptTaxonomyAxiom
  (ax: OWLAPIOMF#RootConceptTaxonomyAxiom)
  : OWLAPIRootConceptTaxonomySignature
  = OWLAPIRootConceptTaxonomySignature(ax.uuid, ax.terminologyBundle, ax.root)

  override def fromSpecificDisjointConceptAxiom
  (ax: OWLAPIOMF#SpecificDisjointConceptAxiom)
  : OWLAPISpecificDisjointConceptSignature
  = OWLAPISpecificDisjointConceptSignature(ax.uuid, ax.terminologyBundle, ax.disjointTaxonomyParent, ax.disjointLeaf)
}

trait OWLAPIMutableTerminologyGraphOps
  extends MutableTerminologyGraphOps[OWLAPIOMF]
          with OWLAPIImmutableTerminologyGraphOps {
  self: OWLAPIOMFOps =>

  override def addAnnotation
  (graph: MutableTerminologyBox,
   subject: OWLAPIOMF#TerminologyThing,
   property: AnnotationProperty,
   value: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Annotation
  = for {
    _ <- addAnnotationProperty(property)
    a <- graph.addAnnotation(subject, property, value)
  } yield a

  def addAnnotation
  (graph: MutableTerminologyBox,
   subject: OWLAPIOMF#TerminologyThing,
   a: OWLAnnotation)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Annotation
  = for {
    ap <- store.lookupAnnotationProperty(a)
    v <- a.getValue match {
      case _: OWLAnonymousIndividual =>
        Set[java.lang.Throwable](OMFError.omfError(s"AnonymousIndividual cannot be an OMF annotation value: $a")).left
      case i: IRI =>
        i.getIRIString.right
      case l: OWLLiteral =>
        l.getLiteral.right
    }
    a <- addAnnotation(graph, subject, ap, v)
  } yield a

  override def removeAnnotations
  (graph: OWLAPIOMF#MutableTerminologyBox,
   subject: OWLAPIOMF#TerminologyThing,
   property: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Seq[Annotation]
  = graph.removeAnnotations(subject, property)

  def addAnnotationAssertions
  (graph: MutableTerminologyBox,
   subject: OWLAPIOMF#TerminologyThing,
   aas: Vector[OWLAnnotationAssertionAxiom])
  (implicit store: OWLAPIOMFGraphStore)
  : types.UnitNES
  = aas.foldLeft[types.UnitNES](types.rightUnitNES) { case (acc, aa) =>
    for {
      _ <- acc
      ap = getAnnotationPropertyFromOWLAnnotation(aa.getAnnotation)
      _ <- addAnnotationProperty(ap)
      _ <- addAnnotation(graph, subject, aa.getAnnotation)
    } yield ()
  }

  override protected def addAspect
  (graph: MutableTerminologyBox,
   uuid: UUID,
   aspectIRI: IRI,
   aspectName: LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Aspect
  = for {
    result <- graph.addEntityAspect(aspectIRI, aspectName, uuid)
    _ <- store.registerOMFModelEntityAspectInstance(graph, result)
  } yield result

  override protected def addConcept
  (graph: MutableTerminologyBox,
   uuid: UUID,
   conceptIRI: IRI,
   conceptName: LocalName,
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Concept
  = for {
    result <- graph.addEntityConcept(conceptIRI, conceptName, uuid, isAbstract)
    _ <- store.registerOMFModelEntityConceptInstance(graph, result)
  } yield result

  override protected def addReifiedRelationship
  (graph: MutableTerminologyBox,
   uuid: UUID,
   rIRI: IRI,
   source: Entity,
   target: Entity,
   characteristics: Iterable[RelationshipCharacteristics],
   reifiedRelationshipName: String,
   unreifiedRelationshipName: String,
   unreifiedInverseRelationshipName: Option[String],
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ ReifiedRelationship
  = for {
    uIRI <- withFragment(graph.iri, unreifiedRelationshipName)
    uiIRI <- unreifiedInverseRelationshipName.fold[Set[java.lang.Throwable] \/ Option[IRI]](\/-(None)) { uName =>
      withFragment(graph.iri, uName).map(Some(_))
    }
    rIRISource = toSourceIRI(rIRI)
    rIRITarget = toTargetIRI(rIRI)
    result <- graph.addEntityReifiedRelationship(
      rIRI, reifiedRelationshipName, unreifiedRelationshipName, unreifiedInverseRelationshipName, uuid,
      rIRISource, rIRITarget,
      uIRI, uiIRI,
      source, target,
      characteristics, isAbstract)
    _ <- store.registerOMFModelEntityReifiedRelationshipInstance(graph, result)
  } yield result

  override protected def addUnreifiedRelationship
  (graph: MutableTerminologyBox,
   uuid: UUID,
   rIRI: IRI,
   source: Entity,
   target: Entity,
   characteristics: Iterable[RelationshipCharacteristics],
   unreifiedRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ UnreifiedRelationship
  = for {
    result <- graph.addEntityUnreifiedRelationship(
      rIRI, unreifiedRelationshipName, uuid,
      source, target,
      characteristics)
    _ <- store.registerOMFModelEntityUnreifiedRelationshipInstance(graph, result)
  } yield result

  override protected def addScalarDataType
  (graph: MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: IRI,
   dataTypeName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Scalar
  = graph.addScalarDataType(dataTypeIRI, dataTypeName, dataTypeUUID)

  override protected def addStructuredDataType
  (graph: MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: IRI,
   dataTypeName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Structure
  = for {
    result <- graph.addStructuredDataType(dataTypeIRI, dataTypeName, dataTypeUUID)
    _ <- store.registerOMFModelStructuredDataTypeInstance(graph, result)
  } yield result

  override protected def addScalarOneOfRestriction
  (graph: MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: IRI,
   dataTypeName: String,
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ ScalarOneOfRestriction
  = graph.addScalarOneOfRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange)

  override protected def addBinaryScalarRestriction
  (graph: MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: IRI,
   dataTypeName: LocalName,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ BinaryScalarRestriction
  = graph.addBinaryScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    length, minLength, maxLength)
  
  override protected def addIRIScalarRestriction
  (graph: MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: IRI,
   dataTypeName: LocalName,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ IRIScalarRestriction
  = graph.addIRIScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    length, minLength, maxLength, pattern)

  override protected def addNumericScalarRestriction
  (graph: MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: IRI,
   dataTypeName: LocalName,
   minInclusive: Option[String],
   maxInclusive: Option[String],
   minExclusive: Option[String],
   maxExclusive: Option[String],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ NumericScalarRestriction
  = graph.addNumericScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    minInclusive, maxInclusive, minExclusive, maxExclusive)

  override protected def addPlainLiteralScalarRestriction
  (graph: MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: IRI,
   dataTypeName: LocalName,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String],
   language: Option[String],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ PlainLiteralScalarRestriction
  = graph.addPlainLiteralScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    length, minLength, maxLength, pattern, language)

  override protected def addStringScalarRestriction
  (graph: MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: IRI,
   dataTypeName: LocalName,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ StringScalarRestriction
  = graph.addStringScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    length, minLength, maxLength, pattern)

  override protected def addSynonymScalarRestriction
  (graph: MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: IRI,
   dataTypeName: LocalName,
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ SynonymScalarRestriction
  = graph.addSynonymScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange)

  override protected def addScalarOneOfLiteralAxiom
  (graph: MutableTerminologyBox,
   axiomUUID: UUID,
   scalarOneOfRestriction: ScalarOneOfRestriction,
   value: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ ScalarOneOfLiteralAxiom
  = graph.addScalarOneOfLiteralAxiom(axiomUUID, scalarOneOfRestriction, value)

  override protected def addTimeScalarRestriction
  (graph: MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: IRI,
   dataTypeName: LocalName,
   minInclusive: Option[String],
   maxInclusive: Option[String],
   minExclusive: Option[String],
   maxExclusive: Option[String],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ TimeScalarRestriction
  = graph.addTimeScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    minInclusive, maxInclusive, minExclusive, maxExclusive)

  override protected def addEntityScalarDataProperty
  (graph: MutableTerminologyBox,
   uuid: UUID,
   dataRelationshipIRI: IRI,
   source: Entity,
   target: DataRange,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore)
  = graph.addDataRelationshipFromEntityToScalar(dataRelationshipIRI, dataRelationshipName, uuid, source, target)

  override protected def addEntityStructuredDataProperty
  (graph: MutableTerminologyBox,
   uuid: UUID,
   dataRelationshipIRI: IRI,
   source: Entity,
   target: Structure,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ EntityStructuredDataProperty
  = graph.addDataRelationshipFromEntityToStructure(dataRelationshipIRI, dataRelationshipName, uuid, source, target)

  override protected def addScalarDataProperty
  (graph: MutableTerminologyBox,
   uuid: UUID,
   dataRelationshipIRI: IRI,
   source: Structure,
   target: DataRange,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ ScalarDataProperty
  = graph.addDataRelationshipFromStructureToScalar(dataRelationshipIRI, dataRelationshipName, uuid, source, target)

  override protected def addStructuredDataProperty
  (graph: MutableTerminologyBox,
   uuid: UUID,
   dataRelationshipIRI: IRI,
   source: Structure,
   target: Structure,
   dataRelationshipName: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ StructuredDataProperty
  = graph.addDataRelationshipFromStructureToStructure(dataRelationshipIRI, dataRelationshipName, uuid, source, target)

  override protected def addAspectSpecializationAxiom
  (graph: MutableTerminologyBox,
   uuid: UUID,
   sub: Entity,
   sup: Aspect)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ AspectSpecializationAxiom
  = graph.addEntityDefinitionAspectSubClassAxiom(uuid, sub, sup)

  override protected def addConceptSpecializationAxiom
  (graph: MutableTerminologyBox,
   uuid: UUID,
   sub: Concept,
   sup: Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ ConceptSpecializationAxiom
  = graph.addEntityConceptSubClassAxiom(uuid, sub, sup)

  override protected def addReifiedRelationshipSpecializationAxiom
  (graph: MutableTerminologyBox,
   uuid: UUID,
   sub: ReifiedRelationship,
   sup: ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ ReifiedRelationshipSpecializationAxiom
  = graph.addEntityReifiedRelationshipSubClassAxiom(uuid, sub, sup)

  override protected def addEntityUniversalRestrictionAxiom
  (graph: MutableTerminologyBox,
   uuid: UUID,
   sub: Entity,
   rel: ReifiedRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ EntityUniversalRestrictionAxiom
  = graph.addEntityDefinitionUniversalRestrictionAxiom(uuid, sub, rel, range)

  override protected def addEntityExistentialRestrictionAxiom
  (graph: MutableTerminologyBox,
   uuid: UUID,
   sub: Entity,
   rel: ReifiedRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ EntityExistentialRestrictionAxiom
  = graph.addEntityDefinitionExistentialRestrictionAxiom(uuid, sub, rel, range)
  
  override protected def addEntityScalarDataPropertyExistentialRestrictionAxiom
  (graph: MutableTerminologyBox,
   uuid: UUID,
   restrictedEntity: Entity,
   scalarProperty: EntityScalarDataProperty,
   range: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ EntityScalarDataPropertyExistentialRestrictionAxiom
  = graph.addEntityScalarDataPropertyExistentialRestrictionAxiom(uuid, restrictedEntity, scalarProperty, range)
  
  override protected def addEntityScalarDataPropertyUniversalRestrictionAxiom
  (graph: MutableTerminologyBox,
   uuid: UUID,
   restrictedEntity: Entity,
   scalarProperty: EntityScalarDataProperty,
   range: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ EntityScalarDataPropertyUniversalRestrictionAxiom
  = graph.addEntityScalarDataPropertyUniversalRestrictionAxiom(uuid, restrictedEntity, scalarProperty, range)
  
  override protected def addEntityScalarDataPropertyParticularRestrictionAxiom
  (graph: MutableTerminologyBox,
   uuid: UUID,
   restrictedEntity: Entity,
   scalarProperty: EntityScalarDataProperty,
   literalValue: LexicalValue)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ EntityScalarDataPropertyParticularRestrictionAxiom
  = graph.addEntityScalarDataPropertyParticularRestrictionAxiom(uuid, restrictedEntity, scalarProperty, literalValue)

  override protected def addBundledTerminologyAxiom
  (uuid: UUID,
   terminologyBundle: MutableBundle,
   bundledTerminology: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ BundledTerminologyAxiom
  = terminologyBundle.addBundledTerminologyAxiom(uuid, bundledTerminology)

  override protected def addTerminologyExtension
  (uuid: UUID,
   extendingTerminology: MutableTerminologyBox,
   extendedTerminology: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ TerminologyExtensionAxiom
  = extendingTerminology.addTerminologyGraphExtension(uuid, extendedTerminology)

  override protected def addNestedTerminology
  (uuid: UUID,
   nestingTerminology: TerminologyBox,
   nestingContext: Concept,
   nestedTerminology: MutableTerminologyGraph )
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ TerminologyNestingAxiom
  = nestedTerminology.addNestedTerminologyGraph(uuid, nestingTerminology, nestingContext)

  override protected def addEntityConceptDesignationTerminologyAxiom
  (graph: OWLAPIOMF#MutableTerminologyBox,
   uuid: UUID,
   designatedConcept: OWLAPIOMF#Concept,
   designatedTerminology: OWLAPIOMF#TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ConceptDesignationTerminologyAxiom
  = graph.addEntityConceptDesignationTerminologyGraphAxiom(uuid, designatedConcept, designatedTerminology)

  override protected def addAnonymousConceptTaxonomyAxiom
  (uuid: UUID,
   terminologyBundle: OWLAPIOMF#MutableBundle,
   disjointTerminologyParent: OWLAPIOMF#ConceptTreeDisjunction)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#AnonymousConceptTaxonomyAxiom
  = ???

  override protected def addRootConceptTaxonomyAxiom
  (uuid: UUID,
   terminologyBundle: OWLAPIOMF#MutableBundle,
   root: OWLAPIOMF#Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#RootConceptTaxonomyAxiom
  = ???

  override protected def addSpecificDisjointConceptAxiom
  (uuid: UUID,
   terminologyBundle: OWLAPIOMF#MutableBundle,
   disjointTerminologyParent: OWLAPIOMF#ConceptTreeDisjunction,
   disjointLeaf: OWLAPIOMF#Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#SpecificDisjointConceptAxiom
  = ???
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
   conceptType: Concept,
   fragment: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ModelInstanceObject =
    ???

  // instance relation

  override def addInstanceRelation
  (graph: instances.MutableModelInstanceGraph,
   relationshipType: ReifiedRelationship,
   source: instances.ModelEntityInstance,
   target: instances.ModelEntityInstance,
   fragment: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ModelInstanceRelation =
    ???

  // data literal

  override def addDataLiteral
  (graph: instances.MutableModelInstanceGraph,
   datatype: Scalar,
   lexicalForm: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ModelInstanceDataLiteral =
    ???

  // data structure

  override def addDataStructure
  (graph: instances.MutableModelInstanceGraph,
   datatype: Structure,
   fragment: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ModelInstanceDataStructure =
    ???

  // data property from entity to scalar

  override def addInstanceDataRelationshipFromEntityToScalar
  (graph: instances.MutableModelInstanceGraph,
   ei: instances.ModelEntityInstance,
   e2sc: EntityScalarDataProperty,
   value: instances.ModelInstanceDataLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ instances.ModelInstanceDataRelationshipFromEntityToScalar =
    ???

  // data property from entity to structure

  override def addInstanceDataRelationshipFromEntityToStructure
  (graph: instances.MutableModelInstanceGraph,
   ei: instances.ModelEntityInstance,
   e2st: EntityStructuredDataProperty,
   value: instances.ModelInstanceDataStructure)
  (implicit store: OWLAPIOMFGraphStore) =
    ???

  // data property from structure to scalar

  override def addInstanceDataRelationshipFromStructureToScalar
  (graph: instances.MutableModelInstanceGraph,
   di: instances.ModelInstanceDataStructure,
   e2sc: ScalarDataProperty,
   value: instances.ModelInstanceDataLiteral)
  (implicit store: OWLAPIOMFGraphStore) =
    ???

  // data property from structure to structure

  override def addInstanceDataRelationshipFromStructureToStructure
  (graph: instances.MutableModelInstanceGraph,
   di: instances.ModelInstanceDataStructure,
   e2st: StructuredDataProperty,
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
