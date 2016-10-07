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

package gov.nasa.jpl.omf.scala.binding.owlapi.types

import java.io.OutputStream
import java.util.UUID

import gov.nasa.jpl.imce.omf.schema.tables.LocalName
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import org.semanticweb.owlapi.model._

import scala.collection.immutable._
import scala.collection.JavaConversions._
import scala.compat.java8.StreamConverters._
import scala.{Boolean, None, Option, Some, StringContext, Unit}
import scala.Predef.{Map => _, Set => _, _}
import scala.util.control.Exception._
import scalaz._
import Scalaz._

abstract class ModelTerminologyGraph
( val uuid: UUID,
  val name: LocalName,
  val kind: TerminologyKind,
  val ont: OWLOntology,
  val extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance] )
( implicit val ops: OWLAPIOMFOps ) {

  require(null != kind)
  require(null != ont)
  require(null != ops)

  val mutabilityKind: String
  val isImmutableModelTerminologyGraph: Boolean
  val isMutableModelTerminologyGraph: Boolean

  val ontManager = ont.getOWLOntologyManager
  val owlDataFactory = ontManager.getOWLDataFactory

  protected val aspects: scala.collection.Seq[ModelEntityAspect]
  protected val concepts: scala.collection.Seq[ModelEntityConcept]
  protected val reifiedRelationships: scala.collection.Seq[ModelEntityReifiedRelationship]
  protected val unreifiedRelationships: scala.collection.Seq[ModelEntityUnreifiedRelationship]
  protected val sc: scala.collection.Seq[ModelScalarDataType]
  protected val st: scala.collection.Seq[ModelStructuredDataType]
  protected val e2sc: scala.collection.Seq[ModelDataRelationshipFromEntityToScalar]
  protected val e2st: scala.collection.Seq[ModelDataRelationshipFromEntityToStructure]
  protected val s2sc: scala.collection.Seq[ModelDataRelationshipFromStructureToScalar]
  protected val s2st: scala.collection.Seq[ModelDataRelationshipFromStructureToStructure]
  protected val ax: scala.collection.Seq[ModelTermAxiom]

  protected val iri2typeTerm: scala.collection.Map[IRI, ModelTypeTerm]

  def isTypeTermDefined
  ( t: ModelTypeTerm )
  : Boolean =
    iri2typeTerm.values.contains( t )

  def isTypeTermDefinedRecursively
  ( t: ModelTypeTerm )
  ( implicit store: OWLAPIOMFGraphStore )
  : Boolean =
    terminologyGraphImportClosure[OWLAPIOMF, ModelTerminologyGraph](this, onlyCompatibleKind = true).
      exists ( _.isTypeTermDefined( t ) )

  def lookupTypeTerm
  ( iri: IRI, recursively: Boolean )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ModelTypeTerm] =
  if (recursively)
    lookupTypeTermRecursively(iri)
  else
    iri2typeTerm.get( iri )

  def lookupTypeTerm
  ( iri: Option[IRI], recursively: Boolean )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ModelTypeTerm] =
    for {
      _iri <- iri
      _t <- lookupTypeTerm( _iri, recursively )
    } yield _t

  def lookupTypeTermRecursively
  ( iri: IRI )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ModelTypeTerm] =
    terminologyGraphImportClosure[OWLAPIOMF, ModelTerminologyGraph](this, onlyCompatibleKind = true).
      flatMap(_.lookupTypeTerm( iri, recursively=false )).headOption

  def lookupTypeTermRecursively
  ( iri: Option[IRI] )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ModelTypeTerm] =
    for {
      _iri <- iri
      _t <- lookupTypeTermRecursively( _iri )
    } yield _t

  val iri: IRI = ont.getOntologyID.getOntologyIRI.get

  val kindIRI: IRI

  protected def makeKindIRI(kind: String)
  : IRI
  = iri.resolve(iri.getRemainder.orElse("") + "?kind=" + kind)

  def getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition]

  def getDataRelationshipsFromEntityToScalar
  : Seq[ModelDataRelationshipFromEntityToScalar]
  = e2sc.to[Seq]

  def getScalarDatatypeDefinitionMap: Map[OWLDatatype, ModelScalarDataType]

  def getTermAxioms: ( IRI, Iterable[ModelTermAxiom] ) = ( iri, ax.to[Iterable] )

  def getTypeTerms: ( IRI, Iterable[ModelTypeTerm] ) = ( iri, iri2typeTerm.values.to[Iterable] )

  def fromTerminologyGraph
  ( extended: Iterable[ModelTerminologyGraph],
    nesting: Option[(ModelEntityConcept, ModelTerminologyGraph)])
  : OWLAPITerminologyGraphSignature
  = OWLAPITerminologyGraphSignature(
      uuid, name, iri, kind,
      extended,
      nesting,
      aspects.to[Iterable],
      concepts.to[Iterable],
      reifiedRelationships.to[Iterable],
      unreifiedRelationships.to[Iterable],
      sc.to[Iterable],
      st.to[Iterable],
      e2sc.to[Iterable],
      e2st.to[Iterable],
      s2sc.to[Iterable],
      s2st.to[Iterable],
      ax.to[Iterable])

  def getTerminologyGraphShortNameAnnotation
  : Option[OWLAnnotation]
  = ont.annotations.toScala[Set].find( _.getProperty.getIRI == ops.rdfs_label )

  def getTerminologyGraphShortName
  : Option[String] =
    getTerminologyGraphShortNameAnnotation.
      flatMap ( _.getValue match {
      case l: OWLLiteral =>
        Some( l.getLiteral )
      case _  =>
        None
  } )


  def getTerminologyGraphUUIDAnnotation
  : Option[OWLAnnotation]
  = ont.annotations.toScala[Set].find( _.getProperty.getIRI == ops.AnnotationHasUUID )

  def getTerminologyGraphUUID
  : UUID
  = getTerminologyGraphUUIDAnnotation
    .flatMap ( _.getValue match {
      case l: OWLLiteral =>
        Some( l.getLiteral )
      case _  =>
        None
    } )
    .fold[UUID]({
    throw OMFError.omfBindingError(s"Missing UUID annotation on graph $iri")
  })( UUID.fromString )

  def getTermLocalNameAnnotationAssertionAxiom
  ( term: types.ModelTypeTerm )
  : Option[OWLAnnotationAssertionAxiom]
  = findAnnotationAssertionAxiom(ont, term.iri, ops.rdfs_label)

  def getTermUUIDAnnotationAssertionAxiom
  ( term: types.ModelTypeTerm )
  : Option[OWLAnnotationAssertionAxiom]
  = findAnnotationAssertionAxiom(ont, term.iri, ops.AnnotationHasUUID )

  def getTermIDAnnotationAssertionAxiom
  ( term: types.ModelTypeTerm )
  : Option[OWLAnnotationAssertionAxiom] =
    ont.annotationAssertionAxioms( term.iri ).toScala[Set].
      find( _.getProperty.getIRI == ops.AnnotationHasID )

  def getTermURLAnnotationAssertionAxiom
  ( term: types.ModelTypeTerm )
  : Option[OWLAnnotationAssertionAxiom] =
    ont.annotationAssertionAxioms( term.iri ).toScala[Set].
      find( _.getProperty.getIRI == ops.AnnotationHasURL )


  def save( saveIRI: IRI ): Set[java.lang.Throwable] \/ Unit =
    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set(
            OMFError.omfException(
              s"saving ModelTerminologyGraph failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        ontManager.saveOntology(ont, saveIRI).right
      })

  def save( os: OutputStream ): Set[java.lang.Throwable] \/ Unit =
    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set(
            OMFError.omfException(
              s"saving ModelTerminologyGraph failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        ontManager.saveOntology(ont, os).right
      })
}