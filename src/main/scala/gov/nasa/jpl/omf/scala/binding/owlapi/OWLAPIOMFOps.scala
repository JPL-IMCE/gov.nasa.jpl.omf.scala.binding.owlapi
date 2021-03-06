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

import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.resolver.api.taggedTypes.ReifiedRelationshipRestrictionUUID
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue, CardinalityRestrictionKind, LiteralValue}
import gov.nasa.jpl.imce.oml.uuid.{JVMUUIDGenerator, OMLUUIDGenerator}
import gov.nasa.jpl.omf.scala.binding.owlapi.common._
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.{DescriptionBox, ImmutableDescriptionBox, MutableDescriptionBox, SingletonInstanceStructuredDataPropertyContext}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{RestrictionScalarDataPropertyValue, RestrictionStructuredDataPropertyContext, RestrictionStructuredDataPropertyTuple}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.bundleStatements.ConceptTreeDisjunction
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms._
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind
import gov.nasa.jpl.omf.scala.core.builtin.BuiltInDatatypeMaps
import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils
import org.semanticweb.owlapi.model._

import scala.{Boolean, None, Option, Some, StringContext, Unit}
import scala.collection.immutable.{Iterable, Seq, Set, Vector}
import scala.compat.java8.StreamConverters._
import scala.util.control.Exception._
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._

import scala.reflect.ClassTag

object OWLAPIIRIOps {

  def makeIRI
  (s: String)
  : Throwables \/IRI
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
  : Throwables \/IRI
  = OWLAPIIRIOps.makeIRI(s)

  def getFragment(iri: IRI)
  : Throwables \/ tables.taggedTypes.LocalName
  = Option.apply(iri.toURI.getFragment) match {
    case None =>
      Set(OMFError.omfBindingError(s"getFragment($iri): error: there should be a fragment!")).left
    case Some(f) =>
      tables.taggedTypes.localName(f).right
  }

  override def withFragment
  (iri: IRI, fragment: tables.taggedTypes.LocalName)
  : Throwables \/ IRI
  = {
    val uriConfig = com.netaporter.uri.config.UriConfig.conservative
    val safeFragment = uriConfig.fragmentEncoder.encode(fragment, uriConfig.charset)
    val u = iri.toURI
    Option.apply(u.getFragment)
    .fold[Throwables \/ IRI](
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
  : (IRI, Option[tables.taggedTypes.LocalName])
  = {
    val u = iri.toURI
    u.getFragment match {
      case f: String if f.nonEmpty =>
        (org.semanticweb.owlapi.model.IRI.create(new URI(u.getScheme, u.getSchemeSpecificPart, null)),
          tables.taggedTypes.localName(f).some)
      case _ =>
        (iri,
          None)
    }
  }

  override def toAbbreviatedName
  (iri: IRI, lowercaseFragmentInitial: Boolean)
  : Option[tables.taggedTypes.AbbrevIRI]
  = splitIRI(iri) match {
    case (_, None) => None
    case (i, Some(fragment)) =>
      val path = i.toURI.getSchemeSpecificPart
      val slash = path.lastIndexOf('/')
      val last = path.substring(slash + 1)
      val fragmentInitial = if (lowercaseFragmentInitial) fragment.head.toLower else fragment.head
      val fragmentTail = fragment.tail
      tables.taggedTypes.abbrevIRI(last + ":" + fragmentInitial + fragmentTail).some
  }

  def lastSegment(iri: IRI)
  : Throwables \/ tables.taggedTypes.LocalName
  = if (Option.apply(iri.toURI.getFragment).isDefined)
      Set(OMFError.omfBindingError(s"lastSegment($iri): error: there should not be a fragment!")).left
    else
      \/-(tables.taggedTypes.localName(iri.getShortForm))

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

  implicit val itbTag = ClassTag[OWLAPIOMF#ImmutableTerminologyBox](classOf[OWLAPIOMF#ImmutableTerminologyBox])
  implicit val itgTag = ClassTag[OWLAPIOMF#ImmutableTerminologyGraph](classOf[OWLAPIOMF#ImmutableTerminologyGraph])
  implicit val bTag = ClassTag[OWLAPIOMF#ImmutableBundle](classOf[OWLAPIOMF#ImmutableBundle])
  implicit val dTag = ClassTag[OWLAPIOMF#ImmutableDescriptionBox](classOf[OWLAPIOMF#ImmutableDescriptionBox])

  override def terminologyBoxImportClosure
  (m: OWLAPIOMF#Module)
  (implicit store: OWLAPIOMF#Store)
  : Set[OWLAPIOMF#TerminologyBox]
  = store.terminologyClosureCache.get(m)

  override def descriptionBoxImportClosure
  (m: OWLAPIOMF#Module)
  (implicit store: OWLAPIOMF#Store)
  : Set[OWLAPIOMF#DescriptionBox]
  = store.descriptionClosureCache.get(m)

  override def lookupModule
  (iri: OWLAPIOMF#IRI)
  (implicit store: OWLAPIOMF#Store)
  : OMFError.Throwables \/ OWLAPIOMF#Module
  = store.lookupModule(iri)

  override def lookupTerminologyBox
  (iri: OWLAPIOMF#IRI)
  (implicit store: OWLAPIOMF#Store)
  : OMFError.Throwables \/ OWLAPIOMF#TerminologyBox
  = store.lookupTerminologyBox(iri)

  override def lookupDescriptionBox
  (iri: OWLAPIOMF#IRI)
  (implicit store: OWLAPIOMF#Store)
  : OMFError.Throwables \/ OWLAPIOMF#DescriptionBox
  = store.lookupDescriptionBox(iri)

  override def getLogicalElementUUID
  (e: OWLAPIOMF#LogicalElement)
  : api.taggedTypes.LogicalElementUUID
  = e.uuid

  override def getModuleIRI
  (m: OWLAPIOMF#Module)
  : IRI
  = m.iri
  
  override def getModuleName
  (m: Module)
  : tables.taggedTypes.LocalName
  = m.name

  override def getModuleUUID
  (m: Module)
  : api.taggedTypes.ModuleUUID
  = m.uuid

  override def annotationProperties
  (m: Module)
  (implicit store: OWLAPIOMFGraphStore)
  : Seq[AnnotationProperty]
  = m.sig.annotationProperties.to[Seq]

  def annotations
  (m: Module)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[AnnotationPropertyValue]
  = m.sig.annotationPropertyValues.to[Set]

  override def foldModule[T]
  (funImmutableTerminologyGraph: OWLAPIOMF#ImmutableTerminologyGraph => T,
   funMutableTerminologyGraph: OWLAPIOMF#MutableTerminologyGraph => T,
   funImmutableTerminologyBundle: OWLAPIOMF#ImmutableBundle => T,
   funMutableTerminologyBundle: OWLAPIOMF#MutableBundle => T,
   funImmutableDescriptionBox: OWLAPIOMF#ImmutableDescriptionBox => T,
   funMutableDescriptionBox: OWLAPIOMF#MutableDescriptionBox => T)
  (m: OWLAPIOMF#Module)
  : T
  = m match {
    case g: OWLAPIOMF#ImmutableTerminologyGraph =>
      funImmutableTerminologyGraph(g)
    case g: OWLAPIOMF#MutableTerminologyGraph =>
      funMutableTerminologyGraph(g)
    case b: OWLAPIOMF#ImmutableBundle =>
      funImmutableTerminologyBundle(b)
    case b: OWLAPIOMF#MutableBundle =>
      funMutableTerminologyBundle(b)
    case d: OWLAPIOMF#ImmutableDescriptionBox =>
      funImmutableDescriptionBox(d)
    case d: OWLAPIOMF#MutableDescriptionBox =>
      funMutableDescriptionBox(d)
  }

  override def foldImmutableModule[T]
  (funImmutableTerminologyGraph: OWLAPIOMF#ImmutableTerminologyGraph => T,
   funImmutableTerminologyBundle: OWLAPIOMF#ImmutableBundle => T,
   funImmutableDescriptionBox: OWLAPIOMF#ImmutableDescriptionBox => T)
  (m: OWLAPIOMF#ImmutableModule)
  : T
  = m match {
    case g: OWLAPIOMF#ImmutableTerminologyGraph =>
      funImmutableTerminologyGraph(g)
    case b: OWLAPIOMF#ImmutableBundle =>
      funImmutableTerminologyBundle(b)
    case d: OWLAPIOMF#ImmutableDescriptionBox =>
      funImmutableDescriptionBox(d)
  }

  override def foldMutableModule[T]
  (funMutableTerminologyGraph: OWLAPIOMF#MutableTerminologyGraph => T,
   funMutableTerminologyBundle: OWLAPIOMF#MutableBundle => T,
   funMutableDescriptionBox: OWLAPIOMF#MutableDescriptionBox => T)
  (m: OWLAPIOMF#MutableModule)
  : T
  = m match {
    case g: OWLAPIOMF#MutableTerminologyGraph =>
      funMutableTerminologyGraph(g)
    case b: OWLAPIOMF#MutableBundle =>
      funMutableTerminologyBundle(b)
    case d: OWLAPIOMF#MutableDescriptionBox =>
      funMutableDescriptionBox(d)
  }

  override def immutableTerminologyGraphSignature
  (ig: OWLAPIOMF#ImmutableTerminologyGraph)
  : ImmutableTerminologyBoxSignature[OWLAPIOMF]
  = ig.sig

  override def mutableTerminologyGraphSignature
  (mg: OWLAPIOMF#MutableTerminologyGraph)
  : MutableTerminologyBoxSignature[OWLAPIOMF]
  = mg.sig

  override def immutableBundleSignature
  (ib: OWLAPIOMF#ImmutableBundle)
  : ImmutableTerminologyBoxSignature[OWLAPIOMF]
  = ib.sig

  override def mutableBundleSignature
  (mb: OWLAPIOMF#MutableBundle)
  : MutableTerminologyBoxSignature[OWLAPIOMF]
  = mb.sig

  override def immutableDescriptionBoxSignature
  (id: OWLAPIOMF#ImmutableDescriptionBox)
  : ImmutableDescriptionBoxSignature[OWLAPIOMF]
  = id.sig

  override def mutableDescriptionBoxSignature
  (md: OWLAPIOMF#MutableDescriptionBox)
  : MutableDescriptionBoxSignature[OWLAPIOMF]
  = md.sig

  override def loadBuiltinDatatypeMap
  ()
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ BuiltInDatatypeMaps.DataRangeCategories[OWLAPIOMF]
  = store.loadBuiltinDatatypeMap()

  override def loadModule
  (m2i: OntologyMapping,
   iri: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ (Module, OntologyMapping)
  = OWLAPIOMFLoader.loadModule(iri, m2i)(this, store)

  override def isMutable
  ( m: Module )
  ( implicit store: OWLAPIOMFGraphStore )
  : Boolean
  = m match {
    case _: MutableModule =>
      true
    case _ =>
      false
  }

  override def asImmutableModule
  (m: MutableModule,
   m2i: OntologyMapping)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/
    (ImmutableModule, OntologyMapping)
  = store.asImmutableModule(m, m2i)

  override def toMutableModule
  ( m: Module )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[MutableModule]
  = m match {
    case mb: MutableModule =>
      Some(mb)
    case _ =>
      None
  }

  override def toTerminologyBox
  ( m: Module )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[TerminologyBox]
  = m match {
    case t: TerminologyBox =>
      Some(t)
    case _ =>
      None
  }

  override def toImmutableTerminologyBox
  ( m: Module )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ImmutableTerminologyBox]
  = m match {
    case t: ImmutableTerminologyBox =>
      Some(t)
    case _ =>
      None
  }

  override def toTerminologyGraph
  ( m: Module )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[TerminologyGraph]
  = m match {
    case t: TerminologyGraph =>
      Some(t)
    case _ =>
      None
  }

  override def toImmutableTerminologyGraph
  ( m: Module )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ImmutableTerminologyGraph]
  = m match {
    case t: ImmutableTerminologyGraph =>
      Some(t)
    case _ =>
      None
  }

  override def toBundle
  ( m: Module )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[Bundle]
  = m match {
    case b: Bundle =>
      Some(b)
    case _ =>
      None
  }

  override def toImmutableBundle
  ( m: Module )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ImmutableBundle]
  = m match {
    case b: ImmutableBundle =>
      Some(b)
    case _ =>
      None
  }

  override def toDescriptionBox
  ( m: Module )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[DescriptionBox]
  = m match {
    case d: DescriptionBox =>
      Some(d)
    case _ =>
      None
  }

  override def toImmutableDescriptionBox
  ( m: Module )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ImmutableDescriptionBox]
  = m match {
    case d: ImmutableDescriptionBox =>
      Some(d)
    case _ =>
      None
  }

  override def fromImmutableTerminology
  (tbox: ImmutableTerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : ImmutableTerminologyBoxSignature[OWLAPIOMF]
  = tbox.sig

  override def fromMutableTerminology
  (tbox: MutableTerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : MutableTerminologyBoxSignature[OWLAPIOMF]
  = tbox.sig

  override def fromImmutableDescriptionBox
  (dbox: ImmutableDescriptionBox)
  (implicit store: OWLAPIOMFGraphStore)
  : ImmutableDescriptionBoxSignature[OWLAPIOMF]
  = dbox.sig

  override def fromMutableDescriptionBox
  (dbox: MutableDescriptionBox)
  (implicit store: OWLAPIOMFGraphStore)
  : MutableDescriptionBoxSignature[OWLAPIOMF]
  = dbox.sig

  override def getTerminologyAxiomUUID
  (ax: TerminologyAxiom)
  (implicit store: OWLAPIOMFGraphStore)
  : api.taggedTypes.TerminologyAxiomUUID
  = ax.uuid

  override def lookupNestingAxiomForNestedChildIfAny
  (nestedG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[TerminologyNestingAxiom]
  = store.lookupNestingAxiomForNestedChildIfAny(nestedG)

  override def lookupNestingAxiomsForNestingContext
  (nestingC: ConceptKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[TerminologyNestingAxiom]
  = store.lookupNestingAxiomsForNestingContext(nestingC)

  override def getExtensionAxioms
  (extendingChildG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[TerminologyExtensionAxiom]
  = store.getExtensionAxioms(extendingChildG)

  override protected def makeTerminologyGraph
  (name: tables.taggedTypes.LocalName,
   iri: IRI,
   kind: TerminologyKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ MutableTerminologyGraph
  = store.makeTerminologyGraph(name, iri, kind)(this)

  override protected def makeBundle
  (name: tables.taggedTypes.LocalName,
   iri: IRI,
   kind: TerminologyKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ MutableBundle
  = store.makeBundle(name, iri, kind)(this)

  override def saveTerminology
  (g: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Unit
  = if (g.owlVocabularyNotToBeSerialized())
      ().right
    else
      store.saveTerminology(g)(this)

  override def saveTerminology
  (g: TerminologyBox,
   os: java.io.OutputStream)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Unit
  = if (g.owlVocabularyNotToBeSerialized())
    ().right
  else
    store.saveTerminology(g, os)(this)

  override def makeDescriptionBox
  (name: tables.taggedTypes.LocalName,
   iri: IRI,
   k: DescriptionKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.MutableDescriptionBox
  = store.makeDescriptionBox(name, iri, k)

  override def saveDescriptionBox
  (g: descriptions.DescriptionBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Unit
  = store.saveDescription(g)(this)

  override def saveDescriptionBox
  (g: descriptions.DescriptionBox, os: java.io.OutputStream)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Unit
  = store.saveDescription(g, os)(this)

  override def resolveIRIAsLocalFile
  (iri: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ File
  = store.resolveIRIAsLocalFile(iri)
}

trait OWLAPIImmutableTerminologyGraphOps
  extends ImmutableTerminologyGraphOps[OWLAPIOMF] {
  self: OWLAPIOMFOps =>

  override def getAnnotations
  (tbox: OWLAPIOMF#TerminologyBox)
  : Set[AnnotationPropertyValue]
  = tbox.sig.annotationPropertyValues.to[Set]

  def getTerminologyKind
  (tbox: TerminologyBox)
  : TerminologyKind
  = tbox.sig.kind

  override def lookupTerm
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Term]
  = tbox.lookupTerm(iri, recursively)

  def lookupTerm
  (tbox: OWLAPIOMF#TerminologyBox, iri: Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Term]
  = iri.map(IRI.create).flatMap(lookupTerm(tbox, _, recursively))

  override def getEntityUUID(term: OWLAPIOMF#Entity): api.taggedTypes.EntityUUID = term.uuid

  override def getConceptualEntityUUID(term: OWLAPIOMF#ConceptualEntity)
  : api.taggedTypes.ConceptualEntityUUID
  = term.uuid

  override def getConceptualRelationshipUUID(term: OWLAPIOMF#ConceptualRelationship)
  : api.taggedTypes.ConceptualRelationshipUUID
  = term.uuid

  override def lookupEntity
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Entity]
  = lookupTerm(tbox, iri, recursively) match {
    case Some(t: OWLAPIOMF#Entity) => Some(t)
    case _ => None
  }

  def lookupEntity
  (tbox: OWLAPIOMF#TerminologyBox, iri: Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Entity]
  = iri.map(IRI.create).flatMap(lookupEntity(tbox, _, recursively))

  override def getAspectKindUUID(term: AspectKind): api.taggedTypes.AspectKindUUID = term.uuid

  override def getAspectUUID(term: Aspect): api.taggedTypes.AspectUUID = term.uuid

  override def getCardinalityRestrictedAspectUUID
  (term: CardinalityRestrictedAspect)
  : api.taggedTypes.CardinalityRestrictedAspectUUID
  = term.uuid

  override def lookupAspectKind
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#AspectKind]
  = lookupTerm(tbox, iri, recursively) match {
    case Some(t: OWLAPIOMF#AspectKind) => Some(t)
    case _ => None
  }

  override def lookupAspect
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Aspect]
  = lookupTerm(tbox, iri, recursively) match {
    case Some(t: OWLAPIOMF#Aspect) => Some(t)
    case _ => None
  }

  def lookupAspect
  (tbox: OWLAPIOMF#TerminologyBox, iri: Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Aspect]
  = iri.map(IRI.create).flatMap(lookupAspect(tbox, _, recursively))

  override def lookupConceptKind
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#ConceptKind]
  = lookupTerm(tbox, iri, recursively) match {
    case Some(t: OWLAPIOMF#ConceptKind) => Some(t)
    case _ => None
  }

  override def lookupConcept
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Concept]
  = lookupTerm(tbox, iri, recursively) match {
    case Some(t: OWLAPIOMF#Concept) => Some(t)
    case _ => None
  }

  override def getConceptKindUUID(term: ConceptKind)
  : api.taggedTypes.ConceptKindUUID
  = term.uuid

  override def getConceptUUID(term: Concept)
  : api.taggedTypes.ConceptUUID
  = term.uuid

  override def getCardinalityRestrictedConceptUUID
  (term: CardinalityRestrictedConcept)
  : api.taggedTypes.CardinalityRestrictedConceptUUID
  = term.uuid

  override def getRestrictableRelationshipUUID
  (term: OWLAPIOMF#RestrictableRelationship)
  : api.taggedTypes.RestrictableRelationshipUUID
  = term.uuid

  def lookupConcept
  (graph: OWLAPIOMF#TerminologyBox, iri: Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Concept]
  = iri.map(IRI.create).flatMap(lookupConcept(graph, _, recursively))

  override def getReifiedRelationshipRestrictionUUID(term: ReifiedRelationshipRestriction): ReifiedRelationshipRestrictionUUID = term.uuid

  override def getEntityRelationshipUUID(term: EntityRelationship): api.taggedTypes.EntityRelationshipUUID = term.uuid

  override def getReifiedRelationshipUUID(term: ReifiedRelationship): api.taggedTypes.ReifiedRelationshipUUID = term.uuid

  override def getCardinalityRestrictedReifiedRelationshipUUID
  (term: CardinalityRestrictedReifiedRelationship)
  : api.taggedTypes.CardinalityRestrictedReifiedRelationshipUUID
  = term.uuid

  override def lookupConceptualRelationship
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#ConceptualRelationship]
  = lookupTerm(tbox, iri, recursively) match {
    case Some(t: OWLAPIOMF#ConceptualRelationship) => Some(t)
    case _ => None
  }

  override def lookupReifiedRelationshipRestriction
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#ReifiedRelationshipRestriction]
  = lookupTerm(tbox, iri, recursively) match {
    case Some(t: OWLAPIOMF#ReifiedRelationshipRestriction) => Some(t)
    case _ => None
  }

  override def lookupReifiedRelationship
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#ReifiedRelationship]
  = lookupTerm(tbox, iri, recursively) match {
    case Some(t: OWLAPIOMF#ReifiedRelationship) => Some(t)
    case _ => None
  }

  def lookupReifiedRelationship
  (tbox: OWLAPIOMF#TerminologyBox, iri: Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#ReifiedRelationship]
  = iri.map(IRI.create).flatMap(lookupReifiedRelationship(tbox, _, recursively))

  override def getUnreifiedRelationshipUUID(term: UnreifiedRelationship): api.taggedTypes.UnreifiedRelationshipUUID = term.uuid

  override def lookupUnreifiedRelationship
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#UnreifiedRelationship]
  = lookupTerm(tbox, iri, recursively) match {
    case Some(t: OWLAPIOMF#UnreifiedRelationship) => Some(t)
    case _ => None
  }
  
  override def getDataRangeUUID(term: DataRange): api.taggedTypes.DataRangeUUID = term.uuid

  override def getScalarUUID(term: OWLAPIOMF#Scalar): api.taggedTypes.ScalarUUID = term.uuid

  override def getScalarOneOfRestrictionUUID(term: OWLAPIOMF#ScalarOneOfRestriction): api.taggedTypes.ScalarOneOfRestrictionUUID = term.uuid

  override def lookupDataRange
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#DataRange]
  = lookupTerm(tbox, iri, recursively) match {
      case Some(t: OWLAPIOMF#DataRange) => Some(t)
      case _ => None
    }

  def lookupDataRange
  (tbox: OWLAPIOMF#TerminologyBox, iri: Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#DataRange]
  = iri.map(IRI.create).flatMap(lookupDataRange(tbox, _, recursively))

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

  override def getStructureUUID(term: Structure): api.taggedTypes.StructureUUID = term.uuid

  override def lookupStructure
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Structure]
  = lookupTerm(tbox, iri, recursively) match {
      case Some(t: OWLAPIOMF#Structure) => Some(t)
      case _ => None
    }

  def lookupStructure
  (graph: OWLAPIOMF#TerminologyBox, iri: Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#Structure]
  = iri.map(IRI.create).flatMap(lookupStructure(graph, _, recursively))

  override def lookupEntityScalarDataProperty
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#EntityScalarDataProperty]
  = lookupTerm(tbox, iri, recursively) match {
      case Some(t: OWLAPIOMF#EntityScalarDataProperty) => Some(t)
      case _ => None
    }

  override def getDataRelationshipToScalarUUID(term: DataRelationshipToScalar): api.taggedTypes.DataRelationshipToScalarUUID = term.uuid

  override def getDataRelationshipToStructureUUID(term: DataRelationshipToStructure): api.taggedTypes.DataRelationshipToStructureUUID = term.uuid

  override def getEntityScalarDataPropertyUUID(term: EntityScalarDataProperty): api.taggedTypes.EntityScalarDataPropertyUUID = term.uuid

  def lookupEntityScalarDataProperty
  (tbox: OWLAPIOMF#TerminologyBox, iri: Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#EntityScalarDataProperty]
  = iri.map(IRI.create).flatMap(lookupEntityScalarDataProperty(tbox, _, recursively))

  override def lookupEntityStructuredDataProperty
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#EntityStructuredDataProperty]
  = lookupTerm(tbox, iri, recursively) match {
      case Some(t: OWLAPIOMF#EntityStructuredDataProperty) => Some(t)
      case _ => None
    }

  override def getEntityStructuredDataPropertyUUID(term: EntityStructuredDataProperty): api.taggedTypes.EntityStructuredDataPropertyUUID = term.uuid

  def lookupEntityStructuredDataProperty
  (tbox: OWLAPIOMF#TerminologyBox, iri: Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#EntityStructuredDataProperty]
  = iri.map(IRI.create).flatMap(lookupEntityStructuredDataProperty(tbox, _, recursively))

  override def lookupScalarDataProperty
  (graph: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#ScalarDataProperty]
  = lookupTerm(graph, iri, recursively) match {
      case Some(t: OWLAPIOMF#ScalarDataProperty) => Some(t)
      case _ => None
    }

  override def getScalarDataPropertyUUID(term: ScalarDataProperty): api.taggedTypes.ScalarDataPropertyUUID = term.uuid

  def lookupScalarDataProperty
  (tbox: OWLAPIOMF#TerminologyBox, iri: Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#ScalarDataProperty]
  = iri.map(IRI.create).flatMap(lookupScalarDataProperty(tbox, _, recursively))

  override def lookupStructuredDataProperty
  (tbox: OWLAPIOMF#TerminologyBox, iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#StructuredDataProperty]
  = lookupTerm(tbox, iri, recursively) match {
      case Some(t: OWLAPIOMF#StructuredDataProperty) => Some(t)
      case _ => None
    }

  override def getStructuredDataPropertyUUID(term: StructuredDataProperty): api.taggedTypes.StructuredDataPropertyUUID = term.uuid

  def lookupStructuredDataProperty
  (tbox: OWLAPIOMF#TerminologyBox, iri: Option[gov.nasa.jpl.imce.oml.tables.taggedTypes.IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#StructuredDataProperty]
  = iri.map(IRI.create).flatMap(lookupStructuredDataProperty(tbox, _, recursively))

  override def getAxiomUUID
  (ax: OWLAPIOMF#Axiom)
  : api.taggedTypes.TerminologyBoxStatementUUID
  = ax.uuid

  override def getAxioms
  (tbox: OWLAPIOMF#TerminologyBox)
  : ( IRI, Iterable[OWLAPIOMF#Axiom] )
  = tbox.getTermAxioms

  override def getTerms
  (tbox: OWLAPIOMF#TerminologyBox)
  : ( IRI, Iterable[OWLAPIOMF#Term] )
  = tbox.getTypeTerms

  def foldTerm[T]
  (funAspect: OWLAPIOMF#Aspect => T,
   funCardinalityRestrictedAspect: OWLAPIOMF#CardinalityRestrictedAspect => T,
   funConcept: OWLAPIOMF#Concept => T,
   funCardinalityRestrictedConcept: OWLAPIOMF#CardinalityRestrictedConcept => T,
   funCardinalityRestrictedReifiedRelationship: OWLAPIOMF#CardinalityRestrictedReifiedRelationship => T,
   funReifiedRelationshipRestriction: OWLAPIOMF#ReifiedRelationshipRestriction => T,
   funReifiedRelationship: OWLAPIOMF#ReifiedRelationship => T,
   funUnreifiedRelationship: OWLAPIOMF#UnreifiedRelationship => T,
   funScalar: OWLAPIOMF#Scalar => T,
   funStructure: OWLAPIOMF#Structure => T,
   funScalarOneOfRestriction: OWLAPIOMF#ScalarOneOfRestriction => T,
   funBinaryScalarRestriction: OWLAPIOMF#BinaryScalarRestriction => T,
   funIRIScalarRestriction: OWLAPIOMF#IRIScalarRestriction => T,
   funPlainLiteralScalarRestriction: OWLAPIOMF#PlainLiteralScalarRestriction => T,
   funStringScalarRestriction: OWLAPIOMF#StringScalarRestriction => T,
   funSynonymScalarRestriction: OWLAPIOMF#SynonymScalarRestriction => T,
   funTimeScalarRestriction: OWLAPIOMF#TimeScalarRestriction => T,
   funEntityScalarDataProperty: OWLAPIOMF#EntityScalarDataProperty => T,
   funEntityStructuredDataProperty: OWLAPIOMF#EntityStructuredDataProperty => T,
   funScalarDataProperty: OWLAPIOMF#ScalarDataProperty => T,
   funStructuredDataProperty: OWLAPIOMF#StructuredDataProperty => T,
   funChainRule: OWLAPIOMF#ChainRule => T)
  (t: types.Term)
  : T = t match {
    case et: OWLAPIOMF#Aspect =>
      funAspect(et)
    case et: OWLAPIOMF#CardinalityRestrictedAspect =>
      funCardinalityRestrictedAspect(et)
    case et: OWLAPIOMF#Concept =>
      funConcept(et)
    case et: OWLAPIOMF#CardinalityRestrictedConcept =>
      funCardinalityRestrictedConcept(et)
    case et: OWLAPIOMF#CardinalityRestrictedReifiedRelationship =>
      funCardinalityRestrictedReifiedRelationship(et)
    case ep: OWLAPIOMF#ReifiedRelationshipRestriction =>
      funReifiedRelationshipRestriction(ep)
    case et: OWLAPIOMF#ReifiedRelationship =>
      funReifiedRelationship(et)
    case et: OWLAPIOMF#UnreifiedRelationship =>
      funUnreifiedRelationship(et)
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

  override def getTermIRI
  (term: OWLAPIOMF#Term)
  : OWLAPIOMF#IRI
  = term.iri

  override def getTermName
  (term: OWLAPIOMF#Term)
  : tables.taggedTypes.LocalName
  = term.name

  override def getTermUUID
  (term: OWLAPIOMF#Term)
  : api.taggedTypes.TermUUID
  = term.uuid

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

  override def getConceptTreeDisjunctionUUID
  (ctd: ConceptTreeDisjunction)
  : api.taggedTypes.ConceptTreeDisjunctionUUID
  = ctd.uuid

  override def lookupNestingAxiomForNestedChildIfAny
  (nestedG: OWLAPIOMF#TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[OWLAPIOMF#TerminologyNestingAxiom]
  = store.lookupNestingAxiomForNestedChildIfAny(nestedG)

  override def lookupNestingAxiomsForNestingContext
  (nestingC: OWLAPIOMF#ConceptKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[OWLAPIOMF#TerminologyNestingAxiom]
  = store.lookupNestingAxiomsForNestingContext(nestingC)

  override def fromEntity
  (e: OWLAPIOMF#Entity)
  : EntitySignature[OWLAPIOMF]
  = EntitySignature[OWLAPIOMF](e.uuid, e.name, e.iri)

  override def fromAspect
  (a: OWLAPIOMF#Aspect)
  : AspectSignature[OWLAPIOMF]
  = AspectSignature[OWLAPIOMF](a.uuid, a.name, a.iri)

  override def fromCardinalityRestrictedAspect
  (ca: OWLAPIOMF#CardinalityRestrictedAspect)
  : CardinalityRestrictedAspectSignature[OWLAPIOMF]
  = CardinalityRestrictedAspectSignature[OWLAPIOMF](
    ca.uuid, ca.name, ca.iri,
    ca.restrictionKind,
    ca.restrictedRelationship,
    ca.restrictedRange,
    ca.restrictedCardinality)

  override def fromConcept
  (c: OWLAPIOMF#Concept)
  : ConceptSignature[OWLAPIOMF]
  = ConceptSignature[OWLAPIOMF](c.uuid, c.name, c.iri)

  override def fromCardinalityRestrictedConcept
  (cc: OWLAPIOMF#CardinalityRestrictedConcept)
  : CardinalityRestrictedConceptSignature[OWLAPIOMF]
  = CardinalityRestrictedConceptSignature[OWLAPIOMF](
    cc.uuid, cc.name, cc.iri,
    cc.restrictionKind,
    cc.restrictedRelationship,
    cc.restrictedRange,
    cc.restrictedCardinality)

  override def fromReifiedRelationshipRestriction
  (ax: OWLAPIOMF#ReifiedRelationshipRestriction)
  : ReifiedRelationshipRestrictionSignature[OWLAPIOMF]
  = ReifiedRelationshipRestrictionSignature[OWLAPIOMF](ax.uuid, ax.name, ax.iri, ax.source, ax.target)

  override def fromReifiedRelationship
  (r: OWLAPIOMF#ReifiedRelationship)
  : ReifiedRelationshipSignature[OWLAPIOMF]
  = ReifiedRelationshipSignature[OWLAPIOMF](
    r.uuid, r.name, r.iri, r.source, r.target, r.characteristics,
    r.forwardProperty,
    ReifiedRelationshipSignature.ForwardPropertySignature[OWLAPIOMF](
      r.forwardProperty.uuid, r.forwardProperty.iri, r.forwardProperty.name),
    r.inverseProperty,
    r.inverseProperty.map { inv =>
      ReifiedRelationshipSignature.InversePropertySignature[OWLAPIOMF](inv.uuid, inv.iri, inv.name)
    })

  override def fromCardinalityRestrictedReifiedRelationship
  (crr: OWLAPIOMF#CardinalityRestrictedReifiedRelationship)
  : CardinalityRestrictedReifiedRelationshipSignature[OWLAPIOMF]
  = CardinalityRestrictedReifiedRelationshipSignature[OWLAPIOMF](
    crr.uuid, crr.name, crr.iri,
    crr.restrictionKind,
    crr.restrictedRelationship,
    crr.restrictedRange,
    crr.restrictedCardinality)

  override def fromUnreifiedRelationship
  (r: OWLAPIOMF#UnreifiedRelationship)
  : UnreifiedRelationshipSignature[OWLAPIOMF]
  = UnreifiedRelationshipSignature[OWLAPIOMF](r.uuid, r.name, r.iri, r.source, r.target, r.characteristics)

  override def fromPredicate
  (p: OWLAPIOMF#Predicate)
  : PredicateSignature[OWLAPIOMF]
  = PredicateSignature[OWLAPIOMF](p.uuid, p.name)

  override def fromRestrictableRelationship
  (r: OWLAPIOMF#RestrictableRelationship)
  : RestrictableRelationshipSignature
  = RestrictableRelationshipSignature(r.uuid, r.name)

  override def fromEntityScalarDataProperty
  (esc: OWLAPIOMF#EntityScalarDataProperty)
  : EntityScalarDataPropertySignature[OWLAPIOMF]
  = EntityScalarDataPropertySignature[OWLAPIOMF](esc.uuid, esc.name, esc.iri, esc.domain, esc.range, esc.isIdentityCriteria)

  override def fromEntityStructuredDataProperty
  (est: OWLAPIOMF#EntityStructuredDataProperty)
  : EntityStructuredDataPropertySignature[OWLAPIOMF]
  = EntityStructuredDataPropertySignature[OWLAPIOMF](est.uuid, est.name, est.iri, est.domain, est.range, est.isIdentityCriteria)

  override def fromScalarDataProperty
  (ssc: OWLAPIOMF#ScalarDataProperty)
  : ScalarDataPropertySignature[OWLAPIOMF]
  = ScalarDataPropertySignature[OWLAPIOMF](ssc.uuid, ssc.name, ssc.iri, ssc.domain, ssc.range)

  override def fromStructuredDataProperty
  (sst: OWLAPIOMF#StructuredDataProperty)
  : StructuredDataPropertySignature[OWLAPIOMF]
  = StructuredDataPropertySignature[OWLAPIOMF](sst.uuid, sst.name, sst.iri, sst.domain, sst.range)

  override def foldAxiom[T]
  (funAspectSpecializationAxiom
   : OWLAPIOMF#AspectSpecializationAxiom => T,
   funConceptSpecializationAxiom
   : OWLAPIOMF#ConceptSpecializationAxiom => T,
   funReifiedRelationshipSpecializationAxiom
   : OWLAPIOMF#ReifiedRelationshipSpecializationAxiom => T,
   funSubDataPropertyOfAxiom
   : OWLAPIOMF#SubDataPropertyOfAxiom => T,
   funSubObjectPropertyOfAxiom
   : OWLAPIOMF#SubObjectPropertyOfAxiom => T,
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
   funEntityStructuredDataPropertyParticularRestrictionAxiom
   : OWLAPIOMF#EntityStructuredDataPropertyParticularRestrictionAxiom => T,
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
      funReifiedRelationshipSpecializationAxiom(ax)
    case ax: OWLAPIOMF#SubDataPropertyOfAxiom =>
      funSubDataPropertyOfAxiom(ax)
    case ax: OWLAPIOMF#SubObjectPropertyOfAxiom =>
      funSubObjectPropertyOfAxiom(ax)
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
    case ax: OWLAPIOMF#EntityStructuredDataPropertyParticularRestrictionAxiom =>
      funEntityStructuredDataPropertyParticularRestrictionAxiom(ax)
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
  : AspectSpecializationSignature[OWLAPIOMF]
  = AspectSpecializationSignature[OWLAPIOMF](ax.uuid, ax.sub, ax.sup)

  override def fromConceptSpecializationAxiom
  (ax: OWLAPIOMF#ConceptSpecializationAxiom)
  : ConceptSpecializationSignature[OWLAPIOMF]
  = ConceptSpecializationSignature[OWLAPIOMF](ax.uuid, ax.sub, ax.sup)

  override def fromReifiedRelationshipSpecializationAxiom
  (ax: OWLAPIOMF#ReifiedRelationshipSpecializationAxiom)
  : ReifiedRelationshipSpecializationSignature[OWLAPIOMF]
  = ReifiedRelationshipSpecializationSignature[OWLAPIOMF](ax.uuid, ax.sub, ax.sup)

  override def fromSubDataPropertyOfAxiom
  (ax: OWLAPIOMF#SubDataPropertyOfAxiom)
  : SubDataPropertyOfAxiomSignature[OWLAPIOMF]
  = SubDataPropertyOfAxiomSignature[OWLAPIOMF](ax.uuid, ax.sub, ax.sup)

  override def fromSubObjectPropertyOfAxiom
  (ax: OWLAPIOMF#SubObjectPropertyOfAxiom)
  : SubObjectPropertyOfAxiomSignature[OWLAPIOMF]
  = SubObjectPropertyOfAxiomSignature[OWLAPIOMF](ax.uuid, ax.sub, ax.sup)

  override def fromEntityExistentialRestrictionAxiom
  (ax: OWLAPIOMF#EntityExistentialRestrictionAxiom)
  : EntityExistentialRestrictionSignature[OWLAPIOMF]
  = EntityExistentialRestrictionSignature[OWLAPIOMF](
      ax.uuid,
      ax.restrictedDomain,
      ax.restrictedRelationship, ax.restrictedRange)

  override def fromEntityUniversalRestrictionAxiom
  (ax: OWLAPIOMF#EntityUniversalRestrictionAxiom)
  : EntityUniversalRestrictionSignature[OWLAPIOMF]
  = EntityUniversalRestrictionSignature[OWLAPIOMF](
    ax.uuid,
    ax.restrictedDomain,
    ax.restrictedRelationship, ax.restrictedRange)

  override def fromEntityScalarDataPropertyExistentialRestrictionAxiom
  (ax: OWLAPIOMF#EntityScalarDataPropertyExistentialRestrictionAxiom)
  : EntityScalarDataPropertyExistentialRestrictionSignature[OWLAPIOMF]
  = EntityScalarDataPropertyExistentialRestrictionSignature[OWLAPIOMF](
    ax.uuid, ax.restrictedEntity, ax.scalarProperty, ax.scalarRestriction)

  override def fromEntityScalarDataPropertyParticularRestrictionAxiom
  (ax: OWLAPIOMF#EntityScalarDataPropertyParticularRestrictionAxiom)
  : EntityScalarDataPropertyParticularRestrictionSignature[OWLAPIOMF]
  = EntityScalarDataPropertyParticularRestrictionSignature[OWLAPIOMF](
    ax.uuid, ax.restrictedEntity, ax.scalarProperty, ax.literalValue, ax.valueType)

  override def fromEntityScalarDataPropertyUniversalRestrictionAxiom
  (ax: OWLAPIOMF#EntityScalarDataPropertyUniversalRestrictionAxiom)
  : EntityScalarDataPropertyUniversalRestrictionSignature[OWLAPIOMF]
  = EntityScalarDataPropertyUniversalRestrictionSignature[OWLAPIOMF](
    ax.uuid, ax.restrictedEntity, ax.scalarProperty, ax.scalarRestriction)

  override def fromEntityStructuredDataPropertyParticularRestrictionAxiom
  (ax: OWLAPIOMF#EntityStructuredDataPropertyParticularRestrictionAxiom)
  : EntityStructuredDataPropertyParticularRestrictionSignature[OWLAPIOMF]
  = EntityStructuredDataPropertyParticularRestrictionSignature[OWLAPIOMF](
    ax.uuid, ax.restrictedEntity, ax.structuredDataProperty)

  override def fromRestrictionStructuredDataPropertyTuple
  (ax: OWLAPIOMF#RestrictionStructuredDataPropertyTuple)
  : RestrictionStructuredDataPropertyTupleSignature[OWLAPIOMF]
  = RestrictionStructuredDataPropertyTupleSignature[OWLAPIOMF](
    ax.uuid, ax.structuredDataPropertyContext, ax.structuredDataProperty)

  override def fromRestrictionScalarDataPropertyValue
  (ax: OWLAPIOMF#RestrictionScalarDataPropertyValue)
  : RestrictionScalarDataPropertyValueSignature[OWLAPIOMF]
  = RestrictionScalarDataPropertyValueSignature[OWLAPIOMF](
    ax.uuid, ax.structuredDataPropertyContext, ax.scalarProperty, ax.literalValue, ax.valueType)

  override def fromScalarOneOfLiteralAxiom
  (ax: OWLAPIOMF#ScalarOneOfLiteralAxiom)
  : ScalarOneOfLiteralSignature[OWLAPIOMF]
  = ScalarOneOfLiteralSignature[OWLAPIOMF](ax.uuid, ax.axiom, ax.value, ax.valueType)

  override def fromBinaryScalarRestriction
  (ax: OWLAPIOMF#BinaryScalarRestriction)
  : BinaryScalarRestrictionSignature[OWLAPIOMF]
  = BinaryScalarRestrictionSignature[OWLAPIOMF](
    ax.uuid, ax.name, ax.iri,
    ax.length, ax.minLength, ax.maxLength, ax.restrictedDataRange)

  override def fromIRIScalarRestriction
  (ax: OWLAPIOMF#IRIScalarRestriction)
  : IRIScalarRestrictionSignature[OWLAPIOMF]
  = IRIScalarRestrictionSignature[OWLAPIOMF](ax.uuid, ax.name, ax.iri,
    ax.length, ax.minLength, ax.maxLength, ax.pattern,
    ax.restrictedDataRange)

  override def fromNumericScalarRestriction
  (ax: OWLAPIOMF#NumericScalarRestriction)
  : NumericScalarRestrictionSignature[OWLAPIOMF]
  = NumericScalarRestrictionSignature[OWLAPIOMF](ax.uuid, ax.name, ax.iri,
    ax.minInclusive, ax.maxInclusive, ax.minExclusive, ax.maxExclusive,
    ax.restrictedDataRange)

  override def fromPlainLiteralScalarRestriction
  (ax: OWLAPIOMF#PlainLiteralScalarRestriction)
  : PlainLiteralScalarRestrictionSignature[OWLAPIOMF]
  = PlainLiteralScalarRestrictionSignature[OWLAPIOMF](ax.uuid, ax.name, ax.iri,
    ax.length, ax.minLength, ax.maxLength, ax.pattern, ax.language,
    ax.restrictedDataRange)

  override def fromScalarOneOfRestriction
  (ax: OWLAPIOMF#ScalarOneOfRestriction)
  : ScalarOneOfRestrictionSignature[OWLAPIOMF]
  = ScalarOneOfRestrictionSignature[OWLAPIOMF](ax.uuid, ax.name, ax.iri, ax.restrictedDataRange)

  override def fromStringScalarRestriction
  (ax: OWLAPIOMF#StringScalarRestriction)
  : StringScalarRestrictionSignature[OWLAPIOMF]
  = StringScalarRestrictionSignature[OWLAPIOMF](ax.uuid, ax.name, ax.iri,
    ax.length, ax.minLength, ax.maxLength,
    ax.pattern, ax.restrictedDataRange)

  override def fromSynonymScalarRestriction
  (ax: OWLAPIOMF#SynonymScalarRestriction)
  : SynonymScalarRestrictionSignature[OWLAPIOMF]
  = SynonymScalarRestrictionSignature[OWLAPIOMF](ax.uuid, ax.name, ax.iri, ax.restrictedDataRange)

  override def fromTimeScalarRestriction
  (ax: OWLAPIOMF#TimeScalarRestriction)
  : TimeScalarRestrictionSignature[OWLAPIOMF]
  = TimeScalarRestrictionSignature[OWLAPIOMF](ax.uuid, ax.name, ax.iri,
    ax.minInclusive, ax.maxInclusive, ax.minExclusive, ax.maxExclusive,
    ax.restrictedDataRange)

  override def fromConceptDesignationTerminologyAxiom
  (ax: OWLAPIOMF#ConceptDesignationTerminologyAxiom)
  : ConceptDesignationTerminologySignature[OWLAPIOMF]
  = ConceptDesignationTerminologySignature[OWLAPIOMF](ax.uuid, ax.graph, ax.designatedConcept, ax.targetModuleIRI)

  override def fromTerminologyExtensionAxiom
  (ax: OWLAPIOMF#TerminologyExtensionAxiom)
  : TerminologyExtensionSignature[OWLAPIOMF]
  = TerminologyExtensionSignature[OWLAPIOMF](ax.uuid, ax.targetModuleIRI)

  def fromTerminologyNestingAxiom
  (ax: OWLAPIOMF#TerminologyNestingAxiom)
  : TerminologyNestingSignature[OWLAPIOMF]
  = TerminologyNestingSignature[OWLAPIOMF](ax.uuid, ax.nestingContext, ax.targetModuleIRI)

  override def fromBundledTerminologyAxiom
  (ax: OWLAPIOMF#BundledTerminologyAxiom)
  : BundledTerminologySignature[OWLAPIOMF]
  = BundledTerminologySignature[OWLAPIOMF](ax.uuid, ax.terminologyBundle, ax.targetModuleIRI)

  override def fromAnonymousConceptTaxonomyAxiom
  (ax: OWLAPIOMF#AnonymousConceptTaxonomyAxiom)
  : AnonymousConceptUnionSignature[OWLAPIOMF]
  = AnonymousConceptUnionSignature[OWLAPIOMF](ax.uuid, ax.name, ax.terminologyBundle, ax.disjointTaxonomyParent)

  override def fromRootConceptTaxonomyAxiom
  (ax: OWLAPIOMF#RootConceptTaxonomyAxiom)
  : RootConceptTaxonomySignature[OWLAPIOMF]
  = RootConceptTaxonomySignature[OWLAPIOMF](ax.uuid, ax.terminologyBundle, ax.root)

  override def fromSpecificDisjointConceptAxiom
  (ax: OWLAPIOMF#SpecificDisjointConceptAxiom)
  : SpecificDisjointConceptSignature[OWLAPIOMF]
  = SpecificDisjointConceptSignature[OWLAPIOMF](ax.uuid, ax.terminologyBundle, ax.disjointTaxonomyParent, ax.disjointLeaf)

  override def getChainRuleUUID(term: OWLAPIOMF#ChainRule): api.taggedTypes.ChainRuleUUID = term.uuid

  @scala.annotation.tailrec
  override final def getChainRule
  (ax: OWLAPIOMF#RuleBodySegment)
  : Throwables \/ OWLAPIOMF#ChainRule
  = ax.chainRule match {
    case Some(cr) =>
      cr.right
    case None =>
      ax.previousSegment match {
        case Some(prev) =>
          getChainRule(prev)
        case None =>
          Set[java.lang.Throwable](
            OMFError.omfError(s"getChainRule: $ax must have either a chain rule or a previous segment!")
          ).left
      }
  }

  override def fromChainRule
  (ax: OWLAPIOMF#ChainRule)
  : ChainRuleSignature[OWLAPIOMF]
  = ChainRuleSignature[OWLAPIOMF](ax.name, ax.uuid, ax.head)

  override def fromRuleBodySegment
  (ax: OWLAPIOMF#RuleBodySegment)
  : RuleBodySegmentSignature[OWLAPIOMF]
  = RuleBodySegmentSignature[OWLAPIOMF](ax.uuid, ax.position, ax.chainRule, ax.previousSegment)

  override def fromSegmentPredicate
  (ax: OWLAPIOMF#SegmentPredicate)
  : SegmentPredicateSignature[OWLAPIOMF]
  = SegmentPredicateSignature[OWLAPIOMF](
    ax.uuid, ax.bodySegment,
    ax.predicate,
    ax.reifiedRelationshipSource,
    ax.reifiedRelationshipInverseSource,
    ax.reifiedRelationshipTarget,
    ax.reifiedRelationshipInverseTarget,
    ax.unreifiedRelationshipInverse)

}

trait OWLAPIMutableTerminologyGraphOps
  extends MutableTerminologyGraphOps[OWLAPIOMF]
    with OWLAPIImmutableTerminologyGraphOps {
  self: OWLAPIOMFOps =>

  override def addTerminologyAnnotationProperty
  (tbox: OWLAPIOMF#MutableTerminologyBox,
   ap: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ AnnotationProperty
  = tbox.addAnnotationProperty(ap)

  override def addTerminologyAnnotation
  (tbox: MutableTerminologyBox,
   subject: OWLAPIOMF#LogicalElement,
   property: AnnotationProperty,
   value: tables.taggedTypes.StringDataType)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ AnnotationPropertyValue
  = for {
    _ <- tbox.addAnnotationProperty(property)
    a <- tbox.addAnnotation(subject, property, value)
  } yield a

  override def removeTerminologyAnnotations
  (tbox: OWLAPIOMF#MutableTerminologyBox,
   subject: OWLAPIOMF#LogicalElement,
   property: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Set[AnnotationPropertyValue]
  = tbox.removeAnnotations(subject, property)

  def addAnnotationAssertions
  (tbox: MutableTerminologyBox,
   subject: OWLAPIOMF#LogicalElement,
   aas: Vector[OWLAnnotationAssertionAxiom])
  (implicit store: OWLAPIOMFGraphStore)
  : types.UnitNES
  = aas.foldLeft[types.UnitNES](types.rightUnitNES) { case (acc, aa) =>
    for {
      _ <- acc
      ap <- getAnnotationPropertyFromOWLAnnotation(aa.getAnnotation)
      _ <- tbox.addAnnotationProperty(ap)
      av <- getAnnotationValueFromOWLAnnotation(aa.getValue)
      _ <- tbox.addAnnotation(subject, ap, av)
    } yield ()
  }

  def addReificationAnnotationAssertions
  (tbox: MutableTerminologyBox,
   subject: OWLAPIOMF#ReifiedRelationship,
   aas: Vector[OWLAnnotationAssertionAxiom])
  (implicit store: OWLAPIOMFGraphStore)
  : types.UnitNES
  = aas.foldLeft[types.UnitNES](types.rightUnitNES) { case (acc, aa) =>
    for {
      _ <- acc
      ap <- getAnnotationPropertyFromOWLAnnotation(aa.getAnnotation)
      isRDFSLabel = store.RDFS_LABEL == aa.getAnnotation.getProperty
      _ <- if (isRDFSLabel)
        types.rightUnitNES
      else
        tbox.addAnnotationProperty(ap)
      av <- getAnnotationValueFromOWLAnnotation(aa.getValue)
      _ <- tbox.addAnnotation(
        subject,
        if (isRDFSLabel) store.ops.omlHasReificationLabelAP else ap,
        av)
    } yield ()
  }

  def addReifiedPropertyAnnotationAssertions
  (tbox: MutableTerminologyBox,
   subject: OWLAPIOMF#ReifiedRelationship,
   aas: Vector[OWLAnnotationAssertionAxiom])
  (implicit store: OWLAPIOMFGraphStore)
  : types.UnitNES
  = aas.foldLeft[types.UnitNES](types.rightUnitNES) { case (acc, aa) =>
    for {
      _ <- acc
      ap <- getAnnotationPropertyFromOWLAnnotation(aa.getAnnotation)
      isRDFSLabel = store.RDFS_LABEL == aa.getAnnotation.getProperty
      _ <- if (isRDFSLabel)
        types.rightUnitNES
      else
        tbox.addAnnotationProperty(ap)
      av <- getAnnotationValueFromOWLAnnotation(aa.getValue)
      _ <- tbox.addAnnotation(
        subject,
        if (isRDFSLabel) store.ops.omlHasPropertyLabelAP else ap,
        av)
    } yield ()
  }

  def addReifiedInverseAnnotationAssertions
  (tbox: MutableTerminologyBox,
   subject: OWLAPIOMF#ReifiedRelationship,
   aas: Vector[OWLAnnotationAssertionAxiom])
  (implicit store: OWLAPIOMFGraphStore)
  : types.UnitNES
  = aas.foldLeft[types.UnitNES](types.rightUnitNES) { case (acc, aa) =>
    for {
      _ <- acc
      ap <- getAnnotationPropertyFromOWLAnnotation(aa.getAnnotation)
      isRDFSLabel = store.RDFS_LABEL == aa.getAnnotation.getProperty
      _ <- if (isRDFSLabel)
        types.rightUnitNES
      else
        tbox.addAnnotationProperty(ap)
      av <- getAnnotationValueFromOWLAnnotation(aa.getValue)
      _ <- tbox.addAnnotation(
        subject,
        if (isRDFSLabel) store.ops.omlHasInverseLabelAP else ap,
        av)
    } yield ()
  }

  override protected def addAspect
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.AspectUUID,
   aspectIRI: IRI,
   aspectName: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Aspect
  = tbox.addEntityAspect(aspectIRI, aspectName, uuid)

  override protected def addCardinalityRestrictedAspectInternal
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.CardinalityRestrictedAspectUUID,
   aspectIRI: IRI,
   aspectName: tables.taggedTypes.LocalName,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: RestrictableRelationship,
   restrictedRange: Option[Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ CardinalityRestrictedAspect
  = tbox.addCardinalityRestrictedAspect(
    aspectIRI, aspectName, uuid,
    restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)

  override protected def addConcept
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.ConceptUUID,
   conceptIRI: IRI,
   conceptName: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Concept
  = tbox.addEntityConcept(conceptIRI, conceptName, uuid)

  override protected def addCardinalityRestrictedConceptInternal
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.CardinalityRestrictedConceptUUID,
   conceptIRI: IRI,
   conceptName: tables.taggedTypes.LocalName,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: RestrictableRelationship,
   restrictedRange: Option[Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ CardinalityRestrictedConcept
  = tbox.addCardinalityRestrictedConcept(
    conceptIRI, conceptName, uuid,
    restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)


  override protected def addReifiedRelationshipRestriction
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.ReifiedRelationshipRestrictionUUID,
   iri: IRI,
   name: tables.taggedTypes.LocalName,
   source: Entity,
   target: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ ReifiedRelationshipRestriction
  = tbox.addReifiedRelationshipRestriction(uuid, iri, name, source, target)

  override protected def addReifiedRelationship
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.ReifiedRelationshipUUID,
   rIRI: IRI,
   source: Entity,
   target: Entity,
   characteristics: Iterable[RelationshipCharacteristics],
   reifiedRelationshipName: tables.taggedTypes.LocalName,
   unreifiedRelationshipName: tables.taggedTypes.LocalName,
   unreifiedInverseRelationshipName: Option[tables.taggedTypes.LocalName])
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ ReifiedRelationship
  = for {
    uIRI <- withFragment(tbox.iri, unreifiedRelationshipName)
    uiIRI <- unreifiedInverseRelationshipName.fold[Throwables \/ Option[IRI]](\/-(None)) { uName =>
      withFragment(tbox.iri, uName).map(Some(_))
    }
    rIRISource = toSourceIRI(rIRI)
    rIRITarget = toTargetIRI(rIRI)
    result <- tbox.addEntityReifiedRelationship(
      rIRI, reifiedRelationshipName, unreifiedRelationshipName, unreifiedInverseRelationshipName, uuid,
      rIRISource, rIRITarget,
      uIRI, uiIRI,
      source, target,
      characteristics)
  } yield result

  override protected def addCardinalityRestrictedReifiedRelationshipInternal
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.CardinalityRestrictedReifiedRelationshipUUID,
   rIRI: IRI,
   reifiedRelationshipName: tables.taggedTypes.LocalName,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: RestrictableRelationship,
   restrictedRange: Option[Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ CardinalityRestrictedReifiedRelationship
  = tbox.addCardinalityRestrictedReifiedRelationship(
    rIRI, reifiedRelationshipName, uuid,
    restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)

  override protected def addUnreifiedRelationship
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.UnreifiedRelationshipUUID,
   rIRI: IRI,
   source: Entity,
   target: Entity,
   characteristics: Iterable[RelationshipCharacteristics],
   unreifiedRelationshipName: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ UnreifiedRelationship
  = tbox.addEntityUnreifiedRelationship(
      rIRI, unreifiedRelationshipName, uuid,
      source, target,
      characteristics)

  override protected def addScalarDataType
  (tbox: MutableTerminologyBox,
   dataTypeUUID: api.taggedTypes.ScalarUUID,
   dataTypeIRI: IRI,
   dataTypeName: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Scalar
  = tbox.addScalarDataType(dataTypeIRI, dataTypeName, dataTypeUUID)

  override protected def addStructuredDataType
  (tbox: MutableTerminologyBox,
   dataTypeUUID: api.taggedTypes.StructureUUID,
   dataTypeIRI: IRI,
   dataTypeName: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Structure
  = tbox.addStructuredDataType(dataTypeIRI, dataTypeName, dataTypeUUID)

  override protected def addScalarOneOfRestriction
  (tbox: MutableTerminologyBox,
   dataTypeUUID: api.taggedTypes.ScalarOneOfRestrictionUUID,
   dataTypeIRI: IRI,
   dataTypeName: tables.taggedTypes.LocalName,
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ ScalarOneOfRestriction
  = tbox.addScalarOneOfRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange)

  override protected def addBinaryScalarRestriction
  (tbox: MutableTerminologyBox,
   dataTypeUUID: api.taggedTypes.BinaryScalarRestrictionUUID,
   dataTypeIRI: IRI,
   dataTypeName: tables.taggedTypes.LocalName,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ BinaryScalarRestriction
  = tbox.addBinaryScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    length, minLength, maxLength)
  
  override protected def addIRIScalarRestriction
  (tbox: MutableTerminologyBox,
   dataTypeUUID: api.taggedTypes.IRIScalarRestrictionUUID,
   dataTypeIRI: IRI,
   dataTypeName: tables.taggedTypes.LocalName,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ IRIScalarRestriction
  = tbox.addIRIScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    length, minLength, maxLength, pattern)

  override protected def addNumericScalarRestriction
  (tbox: MutableTerminologyBox,
   dataTypeUUID: api.taggedTypes.NumericScalarRestrictionUUID,
   dataTypeIRI: IRI,
   dataTypeName: tables.taggedTypes.LocalName,
   minInclusive: Option[tables.LiteralNumber],
   maxInclusive: Option[tables.LiteralNumber],
   minExclusive: Option[tables.LiteralNumber],
   maxExclusive: Option[tables.LiteralNumber],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ NumericScalarRestriction
  = tbox.addNumericScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    minInclusive, maxInclusive, minExclusive, maxExclusive)

  override protected def addPlainLiteralScalarRestriction
  (tbox: MutableTerminologyBox,
   dataTypeUUID: api.taggedTypes.PlainLiteralScalarRestrictionUUID,
   dataTypeIRI: IRI,
   dataTypeName: tables.taggedTypes.LocalName,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern],
   language: Option[tables.taggedTypes.LanguageTagDataType],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ PlainLiteralScalarRestriction
  = tbox.addPlainLiteralScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    length, minLength, maxLength, pattern, language)

  override protected def addStringScalarRestriction
  (tbox: MutableTerminologyBox,
   dataTypeUUID: api.taggedTypes.StringScalarRestrictionUUID,
   dataTypeIRI: IRI,
   dataTypeName: tables.taggedTypes.LocalName,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ StringScalarRestriction
  = tbox.addStringScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    length, minLength, maxLength, pattern)

  override protected def addSynonymScalarRestriction
  (tbox: MutableTerminologyBox,
   dataTypeUUID: api.taggedTypes.SynonymScalarRestrictionUUID,
   dataTypeIRI: IRI,
   dataTypeName: tables.taggedTypes.LocalName,
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ SynonymScalarRestriction
  = tbox.addSynonymScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange)

  override protected def addScalarOneOfLiteralAxiom
  (tbox: MutableTerminologyBox,
   axiomUUID: api.taggedTypes.ScalarOneOfLiteralAxiomUUID,
   scalarOneOfRestriction: ScalarOneOfRestriction,
   value: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ ScalarOneOfLiteralAxiom
  = tbox.addScalarOneOfLiteralAxiom(axiomUUID, scalarOneOfRestriction, value, valueType)

  override protected def addTimeScalarRestriction
  (tbox: MutableTerminologyBox,
   dataTypeUUID: api.taggedTypes.TimeScalarRestrictionUUID,
   dataTypeIRI: IRI,
   dataTypeName: tables.taggedTypes.LocalName,
   minInclusive: Option[tables.LiteralDateTime],
   maxInclusive: Option[tables.LiteralDateTime],
   minExclusive: Option[tables.LiteralDateTime],
   maxExclusive: Option[tables.LiteralDateTime],
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ TimeScalarRestriction
  = tbox.addTimeScalarRestriction(
    dataTypeIRI, dataTypeName, dataTypeUUID, restrictedRange,
    minInclusive, maxInclusive, minExclusive, maxExclusive)

  override protected def addEntityScalarDataProperty
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.EntityScalarDataPropertyUUID,
   dataRelationshipIRI: IRI,
   source: Entity,
   target: DataRange,
   dataRelationshipName: tables.taggedTypes.LocalName,
   isIdentityCriteria: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  = tbox.addDataRelationshipFromEntityToScalar(dataRelationshipIRI, dataRelationshipName, isIdentityCriteria, uuid, source, target)

  override protected def addEntityStructuredDataProperty
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.EntityStructuredDataPropertyUUID,
   dataRelationshipIRI: IRI,
   source: Entity,
   target: Structure,
   dataRelationshipName: tables.taggedTypes.LocalName,
   isIdentityCriteria: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ EntityStructuredDataProperty
  = tbox.addDataRelationshipFromEntityToStructure(dataRelationshipIRI, dataRelationshipName, isIdentityCriteria, uuid, source, target)

  override protected def addScalarDataProperty
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.ScalarDataPropertyUUID,
   dataRelationshipIRI: IRI,
   source: Structure,
   target: DataRange,
   dataRelationshipName: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ ScalarDataProperty
  = tbox.addDataRelationshipFromStructureToScalar(dataRelationshipIRI, dataRelationshipName, uuid, source, target)

  override protected def addStructuredDataProperty
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.StructuredDataPropertyUUID,
   dataRelationshipIRI: IRI,
   source: Structure,
   target: Structure,
   dataRelationshipName: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ StructuredDataProperty
  = tbox.addDataRelationshipFromStructureToStructure(dataRelationshipIRI, dataRelationshipName, uuid, source, target)

  override def makeChainRule
  (tbox: MutableTerminologyBox,
   rule: ChainRule)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Unit
  = tbox.makeChainRule(rule)

  override protected def addChainRule
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.ChainRuleUUID,
   iri: IRI,
   head: RestrictableRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ ChainRule
  = tbox.addChainRule(iri, uuid, head)

  override protected def addRuleBodySegment
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.RuleBodySegmentUUID,
   chainRule: Option[ChainRule],
   previousSegment: Option[RuleBodySegment])
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ RuleBodySegment
  = tbox.addRuleBodySegment(uuid, chainRule, previousSegment)

  override protected def addSegmentPredicate
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.SegmentPredicateUUID,
   bodySegment: RuleBodySegment,
   predicate: Option[Predicate],
   reifiedRelationshipSource: Option[ReifiedRelationship],
   reifiedRelationshipInverseSource: Option[ReifiedRelationship],
   reifiedRelationshipTarget: Option[ReifiedRelationship],
   reifiedRelationshipInverseTarget: Option[ReifiedRelationship],
   unreifiedRelationshipInverse: Option[UnreifiedRelationship])
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ SegmentPredicate
  = tbox.addSegmentPredicate(
    uuid, bodySegment,
    predicate,
    reifiedRelationshipSource,
    reifiedRelationshipInverseSource,
    reifiedRelationshipTarget,
    reifiedRelationshipInverseTarget,
    unreifiedRelationshipInverse)

  override protected def addAspectSpecializationAxiom
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.AspectSpecializationAxiomUUID,
   sub: Entity,
   sup: AspectKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ AspectSpecializationAxiom
  = tbox.addEntityDefinitionAspectSubClassAxiom(uuid, sub, sup)

  override protected def addConceptSpecializationAxiom
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.ConceptSpecializationAxiomUUID,
   sub: ConceptKind,
   sup: ConceptKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ ConceptSpecializationAxiom
  = tbox.addEntityConceptSubClassAxiom(uuid, sub, sup)

  override protected def addReifiedRelationshipSpecializationAxiom
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.ReifiedRelationshipSpecializationAxiomUUID,
   sub: ConceptualRelationship,
   sup: ConceptualRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ ReifiedRelationshipSpecializationAxiom
  = tbox.addReifiedRelationshipSpecializationAxiom(uuid, sub, sup)

  override protected def addSubDataPropertyOfAxiom
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.SubDataPropertyOfAxiomUUID,
   sub: EntityScalarDataProperty,
   sup: EntityScalarDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ SubDataPropertyOfAxiom
  = tbox.addSubDataPropertyOfAxiom(uuid, sub, sup)

  override protected def addSubObjectPropertyOfAxiom
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.SubObjectPropertyOfAxiomUUID,
   sub: UnreifiedRelationship,
   sup: UnreifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ SubObjectPropertyOfAxiom
  = tbox.addSubObjectPropertyOfAxiom(uuid, sub, sup)

  override protected def addEntityUniversalRestrictionAxiom
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.EntityUniversalRestrictionAxiomUUID,
   sub: Entity,
   rel: RestrictableRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ EntityUniversalRestrictionAxiom
  = tbox.addEntityDefinitionUniversalRestrictionAxiom(uuid, sub, rel, range)

  override protected def addEntityExistentialRestrictionAxiom
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.EntityExistentialRestrictionAxiomUUID,
   sub: Entity,
   rel: RestrictableRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ EntityExistentialRestrictionAxiom
  = tbox.addEntityDefinitionExistentialRestrictionAxiom(uuid, sub, rel, range)
  
  override protected def addEntityScalarDataPropertyExistentialRestrictionAxiom
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.EntityScalarDataPropertyExistentialRestrictionAxiomUUID,
   restrictedEntity: Entity,
   scalarProperty: EntityScalarDataProperty,
   range: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ EntityScalarDataPropertyExistentialRestrictionAxiom
  = tbox.addEntityScalarDataPropertyExistentialRestrictionAxiom(uuid, restrictedEntity, scalarProperty, range)
  
  override protected def addEntityScalarDataPropertyUniversalRestrictionAxiom
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.EntityScalarDataPropertyUniversalRestrictionAxiomUUID,
   restrictedEntity: Entity,
   scalarProperty: EntityScalarDataProperty,
   range: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ EntityScalarDataPropertyUniversalRestrictionAxiom
  = tbox.addEntityScalarDataPropertyUniversalRestrictionAxiom(uuid, restrictedEntity, scalarProperty, range)
  
  override protected def addEntityScalarDataPropertyParticularRestrictionAxiom
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.EntityScalarDataPropertyParticularRestrictionAxiomUUID,
   restrictedEntity: Entity,
   scalarProperty: EntityScalarDataProperty,
   literalValue: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ EntityScalarDataPropertyParticularRestrictionAxiom
  = tbox.addEntityScalarDataPropertyParticularRestrictionAxiom(uuid, restrictedEntity, scalarProperty, literalValue, valueType)

  override protected def addEntityStructuredDataPropertyParticularRestrictionAxiom
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.EntityStructuredDataPropertyParticularRestrictionAxiomUUID,
   restrictedEntity: Entity,
   structuredProperty: EntityStructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ EntityStructuredDataPropertyParticularRestrictionAxiom
  = tbox.addEntityStructuredDataPropertyParticularRestrictionAxiom(uuid, restrictedEntity, structuredProperty)

  override protected def addRestrictionStructuredDataPropertyTuple
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.RestrictionStructuredDataPropertyTupleUUID,
   structuredDataPropertyContext: RestrictionStructuredDataPropertyContext,
   structuredProperty: DataRelationshipToStructure)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ RestrictionStructuredDataPropertyTuple
  = tbox.addRestrictionStructuredDataPropertyTuple(uuid, structuredDataPropertyContext, structuredProperty)

  override protected def addRestrictionScalarDataPropertyValue
  (tbox: MutableTerminologyBox,
   uuid: api.taggedTypes.RestrictionScalarDataPropertyValueUUID,
   structuredDataPropertyContext: RestrictionStructuredDataPropertyContext,
   scalarProperty: DataRelationshipToScalar,
   literalValue: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ RestrictionScalarDataPropertyValue
  = tbox.addRestrictionScalarDataPropertyValue(uuid, structuredDataPropertyContext, scalarProperty, literalValue, valueType)

  override protected def addBundledTerminologyAxiom
  (uuid: api.taggedTypes.BundledTerminologyAxiomUUID,
   terminologyBundle: MutableBundle,
   bundledTerminology: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ BundledTerminologyAxiom
  = terminologyBundle.addBundledTerminologyAxiom(uuid, bundledTerminology)

  override protected def addTerminologyExtension
  (uuid: api.taggedTypes.TerminologyExtensionAxiomUUID,
   extendingTerminology: MutableTerminologyBox,
   extendedTerminology: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ TerminologyExtensionAxiom
  = extendingTerminology.addTerminologyGraphExtension(uuid, extendedTerminology)

  override protected def addNestedTerminology
  (uuid: api.taggedTypes.TerminologyNestingAxiomUUID,
   nestingTerminology: TerminologyBox,
   nestingContext: ConceptKind,
   nestedTerminology: MutableTerminologyGraph )
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ TerminologyNestingAxiom
  = nestedTerminology.addNestedTerminologyGraph(uuid, nestingTerminology, nestingContext)

  override protected def addEntityConceptDesignationTerminologyAxiom
  (tbox: OWLAPIOMF#MutableTerminologyBox,
   uuid: api.taggedTypes.ConceptDesignationTerminologyAxiomUUID,
   designatedConcept: OWLAPIOMF#ConceptKind,
   designatedTerminology: OWLAPIOMF#TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ OWLAPIOMF#ConceptDesignationTerminologyAxiom
  = tbox.addEntityConceptDesignationTerminologyGraphAxiom(uuid, tbox, designatedConcept, designatedTerminology)

  override protected def addAnonymousConceptTaxonomyAxiom
  (uuid: api.taggedTypes.AnonymousConceptUnionAxiomUUID,
   terminologyBundle: OWLAPIOMF#MutableBundle,
   disjointTerminologyParent: OWLAPIOMF#ConceptTreeDisjunction,
   name: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ OWLAPIOMF#AnonymousConceptTaxonomyAxiom
  = terminologyBundle.addAnonymousConceptTaxonomyAxiom(uuid, name, disjointTerminologyParent)

  override protected def addRootConceptTaxonomyAxiom
  (uuid: api.taggedTypes.RootConceptTaxonomyAxiomUUID,
   terminologyBundle: OWLAPIOMF#MutableBundle,
   root: OWLAPIOMF#ConceptKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ OWLAPIOMF#RootConceptTaxonomyAxiom
  = terminologyBundle.addRootConceptTaxonomyAxiom(uuid, root)

  override protected def addSpecificDisjointConceptAxiom
  (uuid: api.taggedTypes.SpecificDisjointConceptAxiomUUID,
   terminologyBundle: OWLAPIOMF#MutableBundle,
   disjointTerminologyParent: OWLAPIOMF#ConceptTreeDisjunction,
   disjointLeaf: OWLAPIOMF#ConceptKind)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ OWLAPIOMF#SpecificDisjointConceptAxiom
  = terminologyBundle.addSpecificDisjointConceptAxiom(uuid, disjointTerminologyParent, disjointLeaf)
}

trait OWLAPIImmutableDescriptionBoxOps
  extends ImmutableDescriptionBoxOps[OWLAPIOMF] {
  self: OWLAPIOMFOps =>

  override def getConceptInstanceUUID(i: OWLAPIOMF#ConceptInstance)
  : api.taggedTypes.ConceptInstanceUUID = i.uuid
  override def getConceptualEntitySingletonInstanceUUID(i: OWLAPIOMF#ConceptualEntitySingletonInstance)
  : api.taggedTypes.ConceptualEntitySingletonInstanceUUID = i.uuid
  override def getDescriptionBoxExtendsClosedWorldDefinitionsUUID(i:OWLAPIOMF#DescriptionBoxExtendsClosedWorldDefinitions)
  : api.taggedTypes.DescriptionBoxExtendsClosedWorldDefinitionsUUID = i.uuid
  override def getDescriptionBoxRefinementUUID(i:OWLAPIOMF#DescriptionBoxRefinement)
  : api.taggedTypes.DescriptionBoxRefinementUUID = i.uuid
  override def getReifiedRelationshipInstanceDomainUUID(i:OWLAPIOMF#ReifiedRelationshipInstanceDomain)
  : api.taggedTypes.ReifiedRelationshipInstanceDomainUUID = i.uuid
  override def getReifiedRelationshipInstanceRangeUUID(i:OWLAPIOMF#ReifiedRelationshipInstanceRange)
  : api.taggedTypes.ReifiedRelationshipInstanceRangeUUID = i.uuid
  override def getReifiedRelationshipInstanceUUID(i:OWLAPIOMF#ReifiedRelationshipInstance)
  : api.taggedTypes.ReifiedRelationshipInstanceUUID = i.uuid
  override def getScalarDataPropertyValueUUID(i:OWLAPIOMF#ScalarDataPropertyValue)
  : api.taggedTypes.ScalarDataPropertyValueUUID = i.uuid
  override def getSingletonInstanceScalarDataPropertyValueUUID(i:OWLAPIOMF#SingletonInstanceScalarDataPropertyValue)
  : api.taggedTypes.SingletonInstanceScalarDataPropertyValueUUID = i.uuid
  override def getSingletonInstanceStructuredDataPropertyContextUUID(i:OWLAPIOMF#SingletonInstanceStructuredDataPropertyContext)
  : api.taggedTypes.SingletonInstanceStructuredDataPropertyContextUUID = i.uuid
  override def getSingletonInstanceStructuredDataPropertyValueUUID(i:OWLAPIOMF#SingletonInstanceStructuredDataPropertyValue)
  : api.taggedTypes.SingletonInstanceStructuredDataPropertyValueUUID = i.uuid
  override def getStructuredDataPropertyTupleUUID(i:OWLAPIOMF#StructuredDataPropertyTuple)
  : api.taggedTypes.StructuredDataPropertyTupleUUID = i.uuid
  override def getUnreifiedRelationshipInstanceTupleUUID(i:OWLAPIOMF#UnreifiedRelationshipInstanceTuple)
  : api.taggedTypes.UnreifiedRelationshipInstanceTupleUUID = i.uuid
  override def getInstanceRelationshipEnumerationRestrictionUUID(i:OWLAPIOMF#InstanceRelationshipEnumerationRestriction)
  : api.taggedTypes.InstanceRelationshipEnumerationRestrictionUUID = i.uuid
  override def getInstanceRelationshipValueRestrictionUUID(i:OWLAPIOMF#InstanceRelationshipValueRestriction)
  : api.taggedTypes.InstanceRelationshipValueRestrictionUUID = i.uuid
  override def getInstanceRelationshipExistentialRangeRestrictionUUID(i:OWLAPIOMF#InstanceRelationshipExistentialRangeRestriction)
  : api.taggedTypes.InstanceRelationshipExistentialRangeRestrictionUUID = i.uuid
  override def getInstanceRelationshipUniversalRangeRestrictionUUID(i:OWLAPIOMF#InstanceRelationshipUniversalRangeRestriction)
  : api.taggedTypes.InstanceRelationshipUniversalRangeRestrictionUUID = i.uuid

  override def getImmutableDescriptionBoxIRI
  (graph: descriptions.ImmutableDescriptionBox) =
    graph.iri

  override def fromConceptInstance
  (o: descriptions.ConceptInstance)
  : ConceptInstanceSignature[OWLAPIOMF]
  = ConceptInstanceSignature[OWLAPIOMF](o.uuid, o.conceptType)

  override def fromReifiedRelationshipInstance
  (r: descriptions.ReifiedRelationshipInstance)
  : ReifiedRelationshipInstanceSignature[OWLAPIOMF]
  = ReifiedRelationshipInstanceSignature[OWLAPIOMF](r.uuid, r.relationshipType)

  override def fromReifiedRelationshipInstanceDomain
  (r: descriptions.ReifiedRelationshipInstanceDomain)
  : ReifiedRelationshipInstanceDomainSignature[OWLAPIOMF]
  = ReifiedRelationshipInstanceDomainSignature[OWLAPIOMF](r.uuid, r.relationshipInstance, r.domain)

  override def fromReifiedRelationshipInstanceRange
  (r: descriptions.ReifiedRelationshipInstanceRange)
  : ReifiedRelationshipInstanceRangeSignature[OWLAPIOMF]
  = ReifiedRelationshipInstanceRangeSignature[OWLAPIOMF](r.uuid, r.relationshipInstance, r.range)

  override def fromUnreifiedRelationshipInstanceTuple
  (ur: descriptions.UnreifiedRelationshipInstanceTuple)
  : UnreifiedRelationshipInstanceTupleSignature[OWLAPIOMF]
  = UnreifiedRelationshipInstanceTupleSignature[OWLAPIOMF](
    ur.uuid, ur.unreifiedRelationship, ur.domain, ur.range)

  override def fromInstanceRelationshipEnumerationRestriction
  (er: descriptions.InstanceRelationshipEnumerationRestriction)
  : InstanceRelationshipEnumerationRestrictionSignature[OWLAPIOMF]
  = InstanceRelationshipEnumerationRestrictionSignature[OWLAPIOMF](
    er.uuid, er.restrictedRelationship, er.domain, er.references)
  
  override def fromInstanceRelationshipValueRestriction
  (ur: descriptions.InstanceRelationshipValueRestriction)
  : InstanceRelationshipValueRestrictionSignature[OWLAPIOMF]
  = InstanceRelationshipValueRestrictionSignature[OWLAPIOMF](
    ur.uuid, ur.restrictedRelationship, ur.domain, ur.range)

  override def fromInstanceRelationshipExistentialRangeRestriction
  (ur: descriptions.InstanceRelationshipExistentialRangeRestriction)
  : InstanceRelationshipExistentialRangeRestrictionSignature[OWLAPIOMF]
  = InstanceRelationshipExistentialRangeRestrictionSignature[OWLAPIOMF](
    ur.uuid, ur.restrictedRelationship, ur.domain, ur.range)

  override def fromInstanceRelationshipUniversalRangeRestriction
  (ur: descriptions.InstanceRelationshipUniversalRangeRestriction)
  : InstanceRelationshipUniversalRangeRestrictionSignature[OWLAPIOMF]
  = InstanceRelationshipUniversalRangeRestrictionSignature[OWLAPIOMF](
    ur.uuid, ur.restrictedRelationship, ur.domain, ur.range)

  override def fromSingletonInstanceScalarDataPropertyValue
  (e2sc: descriptions.SingletonInstanceScalarDataPropertyValue)
  : SingletonInstanceScalarDataPropertyValueSignature[OWLAPIOMF]
  = SingletonInstanceScalarDataPropertyValueSignature[OWLAPIOMF](
    e2sc.uuid, e2sc.ei, e2sc.dataRelationship, e2sc.value, e2sc.valueType)

  override def fromSingletonInstanceStructuredDataPropertyValue
  (dbox: descriptions.DescriptionBox, e2st: descriptions.SingletonInstanceStructuredDataPropertyValue)
  : SingletonInstanceStructuredDataPropertyValueSignature[OWLAPIOMF]
  = SingletonInstanceStructuredDataPropertyValueSignature[OWLAPIOMF](
    e2st.uuid, e2st.ei, e2st.structuredDataProperty)

  override def fromScalarDataPropertyValue
  (s2sc: descriptions.ScalarDataPropertyValue)
  : ScalarDataPropertyValueSignature[OWLAPIOMF]
  = ScalarDataPropertyValueSignature[OWLAPIOMF](
    s2sc.uuid, s2sc.context, s2sc.dataRelationship, s2sc.value, s2sc.valueType)

  override def fromStructuredDataPropertyTuple
  (dbox: descriptions.DescriptionBox, s2st: descriptions.StructuredDataPropertyTuple)
  : StructuredDataPropertyTupleSignature[OWLAPIOMF]
  = StructuredDataPropertyTupleSignature[OWLAPIOMF](
    s2st.uuid, s2st.context, s2st.structuredDataProperty)

  override def fromDescriptionBoxRefinementAxiom
  (ax: descriptions.DescriptionBoxRefinement)
  : DescriptionBoxRefinementSignature[OWLAPIOMF]
  = DescriptionBoxRefinementSignature[OWLAPIOMF](
    ax.uuid, ax.sourceModule, ax.targetModuleIRI)

  override def fromClosedWorldDefinitionsAxiom
  (ax: descriptions.DescriptionBoxExtendsClosedWorldDefinitions)
  : DescriptionBoxExtendsClosedWorldDefinitionsSignature[OWLAPIOMF]
  = DescriptionBoxExtendsClosedWorldDefinitionsSignature[OWLAPIOMF](
    ax.uuid, ax.sourceModule, ax.targetModuleIRI)

}

trait OWLAPIMutableDescriptionBoxOps
  extends MutableDescriptionBoxOps[OWLAPIOMF]
    with OWLAPIImmutableDescriptionBoxOps {
  self: OWLAPIOMFOps =>

  override def addDescriptionAnnotationProperty
  (dbox: MutableDescriptionBox,
   ap: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ AnnotationProperty
  = dbox.addAnnotationProperty(ap)

  override def addDescriptionAnnotation
  (dbox: MutableDescriptionBox,
   subject: OWLAPIOMF#LogicalElement,
   property: AnnotationProperty,
   value: tables.taggedTypes.StringDataType)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ AnnotationPropertyValue
  = for {
    _ <- dbox.addAnnotationProperty(property)
    a <- dbox.addAnnotation(subject, property, value)
  } yield a

  override def removeDescriptionAnnotations
  (dbox: MutableDescriptionBox,
   subject: OWLAPIOMF#LogicalElement,
   property: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Set[AnnotationPropertyValue]
  = dbox.removeAnnotations(subject, property)

  override def getMutableDescriptionBoxIRI
  (dbox: descriptions.MutableDescriptionBox)
  = dbox.iri

  override protected def addDescriptionBoxExtendsClosedWorldDefinitions
  (uuid: api.taggedTypes.DescriptionBoxExtendsClosedWorldDefinitionsUUID,
   dbox: descriptions.MutableDescriptionBox,
   closedWorldDefinitions: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.DescriptionBoxExtendsClosedWorldDefinitions
  = dbox.addDescriptionBoxExtendsClosedWorldDefinitions(uuid, closedWorldDefinitions)

  override protected def addDescriptionBoxRefinement
  (uuid: api.taggedTypes.DescriptionBoxRefinementUUID,
   refiningDescriptionBox: descriptions.MutableDescriptionBox,
   refinedDescriptionBox: descriptions.DescriptionBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.DescriptionBoxRefinement
  = refiningDescriptionBox.addDescriptionBoxRefinement(uuid, refinedDescriptionBox)

  override protected def addConceptInstance
  (uuid: api.taggedTypes.ConceptInstanceUUID,
   dbox: descriptions.MutableDescriptionBox,
   iri: IRI,
   conceptType: ConceptKind,
   fragment: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.ConceptInstance
  = dbox.addConceptInstance(uuid, iri, conceptType, fragment)

  override protected def addReifiedRelationshipInstance
  (uuid: api.taggedTypes.ReifiedRelationshipInstanceUUID,
   dbox: descriptions.MutableDescriptionBox,
   iri: IRI,
   relationshipType: ConceptualRelationship,
   fragment: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.ReifiedRelationshipInstance
  = dbox.addReifiedRelationshipInstance(uuid, iri, relationshipType, fragment)

  override protected def addReifiedRelationshipInstanceDomain
  (uuid: api.taggedTypes.ReifiedRelationshipInstanceDomainUUID,
   dbox: descriptions.MutableDescriptionBox,
   relationshipInstance: descriptions.ReifiedRelationshipInstance,
   source: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.ReifiedRelationshipInstanceDomain
  = dbox.addReifiedRelationshipInstanceDomain(uuid, relationshipInstance, source)

  override protected def addReifiedRelationshipInstanceRange
  (uuid: api.taggedTypes.ReifiedRelationshipInstanceRangeUUID,
   dbox: descriptions.MutableDescriptionBox,
   relationshipInstance: descriptions.ReifiedRelationshipInstance,
   target: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.ReifiedRelationshipInstanceRange
  = dbox.addReifiedRelationshipInstanceRange(uuid, relationshipInstance, target)

  override protected def addUnreifiedRelationshipInstanceTuple
  (uuid: api.taggedTypes.UnreifiedRelationshipInstanceTupleUUID,
   dbox: descriptions.MutableDescriptionBox,
   unreifiedRelationship: UnreifiedRelationship,
   source: descriptions.ConceptualEntitySingletonInstance,
   target: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.UnreifiedRelationshipInstanceTuple
  = dbox.addUnreifiedRelationshipInstanceTuple(uuid, unreifiedRelationship, source, target)

  override protected def addInstanceRelationshipEnumerationRestriction
  (uuid: api.taggedTypes.InstanceRelationshipEnumerationRestrictionUUID,
   dbox: descriptions.MutableDescriptionBox,
   restrictedRelationship: RestrictableRelationship with LogicalElement,
   domain: descriptions.ConceptualEntitySingletonInstance,
   references: Vector[descriptions.ConceptualEntitySingletonInstance])
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.InstanceRelationshipEnumerationRestriction
  = dbox.addInstanceRelationshipEnumerationRestriction(uuid, restrictedRelationship, domain, references)

  override protected def addInstanceRelationshipValueRestriction
  (uuid: api.taggedTypes.InstanceRelationshipValueRestrictionUUID,
   dbox: descriptions.MutableDescriptionBox,
   restrictedRelationship: RestrictableRelationship with LogicalElement,
   domain: descriptions.ConceptualEntitySingletonInstance,
   range: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.InstanceRelationshipValueRestriction
  = dbox.addInstanceRelationshipValueRestriction(uuid, restrictedRelationship, domain, range)

  override protected def addInstanceRelationshipExistentialRangeRestriction
  (uuid: api.taggedTypes.InstanceRelationshipExistentialRangeRestrictionUUID,
   dbox: descriptions.MutableDescriptionBox,
   restrictedRelationship: RestrictableRelationship with LogicalElement,
   domain: descriptions.ConceptualEntitySingletonInstance,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.InstanceRelationshipExistentialRangeRestriction
  = dbox.addInstanceRelationshipExistentialRangeRestriction(uuid, restrictedRelationship, domain, range)

  override protected def addInstanceRelationshipUniversalRangeRestriction
  (uuid: api.taggedTypes.InstanceRelationshipUniversalRangeRestrictionUUID,
   dbox: descriptions.MutableDescriptionBox,
   restrictedRelationship: RestrictableRelationship with LogicalElement,
   domain: descriptions.ConceptualEntitySingletonInstance,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.InstanceRelationshipUniversalRangeRestriction
  = dbox.addInstanceRelationshipUniversalRangeRestriction(uuid, restrictedRelationship, domain, range)

  override protected def addSingletonInstanceScalarDataPropertyValue
  (uuid: api.taggedTypes.SingletonInstanceScalarDataPropertyValueUUID,
   dbox: descriptions.MutableDescriptionBox,
   ei: descriptions.ConceptualEntitySingletonInstance,
   e2sc: EntityScalarDataProperty,
   value: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.SingletonInstanceScalarDataPropertyValue
  = dbox.addSingletonInstanceScalarDataPropertyValue(uuid, ei, e2sc, value, valueType)

  override protected def addSingletonInstanceStructuredDataPropertyValue
  (uuid: api.taggedTypes.SingletonInstanceStructuredDataPropertyValueUUID,
   dbox: descriptions.MutableDescriptionBox,
   ei: descriptions.ConceptualEntitySingletonInstance,
   e2st: EntityStructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.SingletonInstanceStructuredDataPropertyValue
  = dbox.addSingletonInstanceStructuredDataPropertyValue(uuid, ei, e2st)

  override protected def makeScalarDataPropertyValue
  (uuid: api.taggedTypes.ScalarDataPropertyValueUUID,
   dbox: descriptions.MutableDescriptionBox,
   structuredDataPropertyContext: SingletonInstanceStructuredDataPropertyContext,
   scalarDataProperty: ScalarDataProperty,
   value: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.ScalarDataPropertyValue
  = dbox.makeScalarDataPropertyValue(uuid, structuredDataPropertyContext, scalarDataProperty, value, valueType)

  override protected def makeStructuredDataPropertyTuple
  (uuid: api.taggedTypes.StructuredDataPropertyTupleUUID,
   dbox: descriptions.MutableDescriptionBox,
   structuredDataPropertyContext: SingletonInstanceStructuredDataPropertyContext,
   structuredDataProperty: StructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ descriptions.StructuredDataPropertyTuple
  = dbox.makeStructuredDataPropertyTuple(uuid, structuredDataPropertyContext, structuredDataProperty)

}

class OWLAPIOMFOps
(val rdfs_label: IRI,
 val AnnotationOMLHasReificationLabel: IRI,
 val AnnotationOMLHasPropertyLabel: IRI,
 val AnnotationOMLHasInverseLabel: IRI,
 val AnnotationHasUUID: IRI,
 val AnnotationIsAbstract: IRI,
 val AnnotationIsDerived: IRI,
 val AnnotationIsBundle: IRI,
 val AnnotationIsDescriptionBox: IRI,
 val AnnotationIsTerminologyGraph: IRI,
 val AnnotationIsTerminologyBoxOpen: IRI,
 val AnnotationIsDescriptionBoxRefinable: IRI,
 val AnnotationHasContext: IRI,
 val AnnotationHasGraph: IRI,
 val AnnotationHasRestrictedSourceProperty: IRI,
 val AnnotationHasRestrictedTargetProperty: IRI)
  extends OMFOps[OWLAPIOMF]
          with OWLAPIIRIOps
          with OWLAPIMutableTerminologyGraphOps
          with OWLAPIMutableDescriptionBoxOps
          with OWLAPIStoreOps {

  override
  def initializeOntologyMapping
  (drc: builtin.BuiltInDatatypeMaps.DataRangeCategories[OWLAPIOMF])
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ OntologyMapping
  = store.initializeOntologyMapping(drc)

  val omlIRI: tables.taggedTypes.IRI
  = tables.taggedTypes.iri("http://imce.jpl.nasa.gov/oml/oml")

  val omlUUIDG: OMLUUIDGenerator = new JVMUUIDGenerator()

  val omlUUID: tables.taggedTypes.ModuleUUID
  = tables.taggedTypes.moduleUUID(omlUUIDG.namespaceUUID(omlIRI).toString)

  val omlHasReificationLabelIRI: tables.taggedTypes.IRI
  = tables.taggedTypes.iri(AnnotationOMLHasReificationLabel.toString)
  val omlHasReificationLabelAP: tables.AnnotationProperty
  = tables.AnnotationProperty(
    tables.taggedTypes.annotationPropertyUUID(generateUUIDFromString(omlHasReificationLabelIRI).toString),
    omlUUID,
    omlHasReificationLabelIRI,
    tables.taggedTypes.abbrevIRI("oml:hasReificationLabel"))

  val omlHasPropertyLabelIRI: tables.taggedTypes.IRI
  = tables.taggedTypes.iri(AnnotationOMLHasPropertyLabel.toString)
  val omlHasPropertyLabelAP: tables.AnnotationProperty
  = tables.AnnotationProperty(
    tables.taggedTypes.annotationPropertyUUID(generateUUIDFromString(omlHasPropertyLabelIRI).toString),
    omlUUID,
    omlHasPropertyLabelIRI,
    tables.taggedTypes.abbrevIRI("oml:hasPropertyLabel"))

  val omlHasInverseLabelIRI: tables.taggedTypes.IRI
  = tables.taggedTypes.iri(AnnotationOMLHasInverseLabel.toString)
  val omlHasInverseLabelAP: tables.AnnotationProperty
  = tables.AnnotationProperty(
    tables.taggedTypes.annotationPropertyUUID(generateUUIDFromString(omlHasInverseLabelIRI).toString),
    omlUUID,
    omlHasInverseLabelIRI,
    tables.taggedTypes.abbrevIRI("oml:hasInverseLabel"))

  def isAnnotatedAbstract
  (ont: OWLOntology,
   termIRI: IRI)
  : Throwables \/ Boolean
  = findAnnotationAssertionAxiom(ont, termIRI, AnnotationIsAbstract)
    .fold[Throwables \/ Boolean] {
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
  : Throwables \/ Boolean
  = findAnnotationAssertionAxiom(ont, termIRI, AnnotationIsDerived)
    .fold[Throwables \/ Boolean]{
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

  val df: OWLDataFactory = ont.getOWLOntologyManager.getOWLDataFactory

  val ontAS: Seq[OWLAnnotation] = ont.annotations().toScala[Seq]

  def booleanAnnotationValue(a: OWLAnnotation)
  : Option[Boolean]
  = a.getValue match {
    case l: OWLLiteral if l.isBoolean =>
      Some(l.parseBoolean)
    case _ =>
      None
  }

  val isBundleAP: OWLAnnotationProperty = df.getOWLAnnotationProperty(ops.AnnotationIsBundle)
  val ontIsBundle: Boolean = ont.annotations(isBundleAP).toScala[Seq].exists(a => booleanAnnotationValue(a).contains(true))

  val isTerminologyGraphAP: OWLAnnotationProperty = df.getOWLAnnotationProperty(ops.AnnotationIsTerminologyGraph)
  val isDescriptionBoxAP: OWLAnnotationProperty = df.getOWLAnnotationProperty(ops.AnnotationIsDescriptionBox)

  val isTerminologyBoxOpenAP: OWLAnnotationProperty = df.getOWLAnnotationProperty(ops.AnnotationIsTerminologyBoxOpen)
  val isDescriptionBoxRefinableAP: OWLAnnotationProperty = df.getOWLAnnotationProperty(ops.AnnotationIsDescriptionBoxRefinable)


  def findOntologyAnnotation(ap: IRI): Option[OWLAnnotation]
  = ontAS.find { _.getProperty.getIRI == ap }

  def checkOntologyAnnotationPresent(ap: IRI, ifAbsent: Boolean): Boolean
  = findOntologyAnnotation(ap) match {
    case Some(a) =>
      a.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          l.parseBoolean()
        case _ =>
          false
      }
    case _ =>
      ifAbsent
  }

  def checkOntologyAnnotationAbsent(ap: IRI): Boolean
  = findOntologyAnnotation(ap) match {
    case Some(a) =>
      a.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          !l.parseBoolean()
        case _ =>
          false
      }
    case _ =>
      true
  }

  def isTerminologyBoxOntology
  : Boolean
  = isBundleOntology || isTerminologyGraphOntology

  def isOpenWorldDefinitionTerminologyBoxOntology
  : Boolean
  = isTerminologyBoxOntology &&
    checkOntologyAnnotationPresent(ops.AnnotationIsTerminologyBoxOpen, ifAbsent=true)

  def isClosedWorldDesignationTerminologyBoxOntology
  : Boolean
  = isTerminologyBoxOntology &&
    checkOntologyAnnotationPresent(ops.AnnotationIsTerminologyBoxOpen, ifAbsent=false)

  def isTerminologyGraphOntology
  : Boolean
  = checkOntologyAnnotationPresent(ops.AnnotationIsTerminologyGraph, ifAbsent=true) &&
    !isBundleOntology && !isDescriptionBoxOntology

  def isBundleOntology
  : Boolean
  = checkOntologyAnnotationPresent(ops.AnnotationIsBundle, ifAbsent=false)

  def isDescriptionBoxOntology
  : Boolean
  = checkOntologyAnnotationPresent(ops.AnnotationIsDescriptionBox, ifAbsent=false)

  def isFinalDescriptionBoxOntology
  : Boolean
  = isDescriptionBoxOntology &&
    checkOntologyAnnotationPresent(ops.AnnotationIsDescriptionBoxRefinable, ifAbsent=false)

  def isPartialDescriptionBoxOntology
  : Boolean
  = isDescriptionBoxOntology &&
    checkOntologyAnnotationPresent(ops.AnnotationIsDescriptionBoxRefinable, ifAbsent=true)

}
