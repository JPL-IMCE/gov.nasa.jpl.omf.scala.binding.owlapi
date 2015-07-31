/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2015, California Institute of Technology ("Caltech").
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
package gov.nasa.jpl.omf.scala.binding.owlapi.types

import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.{ChangeApplied, Imports}
import scala.collection.JavaConversions._
import scala.language.postfixOps
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import java.io.OutputStream

object EntityExceptionKind extends Enumeration {
  type EntityExceptionKind = Value
  val EntityAspect,
  EntityConcept,
  EntityReifiedRelationship,
  DataRelationshipFromEntityToScalar,
  ScalarDataType,
  StructuredDataType = Value
}

import EntityExceptionKind._

object RelationshipScopeAccessKind extends Enumeration {
  type RelationshipScopeAccessKind = Value
  val Source, Target = Value
}

import RelationshipScopeAccessKind._

object AxiomExceptionKind extends Enumeration {
  type AxiomExceptionKind = Value
  val AspectSubclassAxiom, ConceptSubclassAxiom, ConceptRestrictionAxiom, ReifiedRelationshipSubclassAxiom = Value
}

import AxiomExceptionKind._

object AxiomScopeAccessKind extends Enumeration {
  type AxiomScopeAccessKind = Value
  val Sub, Sup, Rel, Range = Value
}

import AxiomScopeAccessKind._

sealed abstract class MutableModelTerminologyGraphException( val message: String )
  extends IllegalArgumentException( message ) {
  require(null != message)
}

case class EntityConflictException
( kind: EntityExceptionKind,
  iri: IRI,
  conflictingTerm: ModelTypeTerm )
  extends MutableModelTerminologyGraphException(
    s"Cannot create $kind with IRI='$iri' because this IRI refers to: $conflictingTerm" ) {

  require(null != kind)
  require(null != iri)
  require(null != conflictingTerm)
}

case class EntityScopeException
( kind: EntityExceptionKind,
  iri: IRI,
  unaccessibleTerms: Map[RelationshipScopeAccessKind, ModelTypeTerm] )
  extends MutableModelTerminologyGraphException(
    s"""Cannot create $kind with IRI='$iri' because
       |there are ${unaccessibleTerms.size} terms out of scope of the graph: """.stripMargin +
      ( unaccessibleTerms.map { case ( kind, term ) => s"$kind: $term" } mkString ", " ) )

case class AxiomScopeException
( kind: AxiomExceptionKind,
  unaccessibleTerms: Map[AxiomScopeAccessKind, ModelTypeTerm] )
  extends MutableModelTerminologyGraphException(
    s"""Cannot create $kind because
       |there are ${unaccessibleTerms.size} terms out of scope of the graph: """.stripMargin +
      ( unaccessibleTerms.map { case ( kind, term ) => s"$kind: $term" } mkString ", " ) )

case class MutableModelTerminologyGraph
( override val kind: TerminologyKind,
  override val ont: OWLOntology )
( override implicit val ops: OWLAPIOMFOps )
  extends ModelTerminologyGraph( kind, ont )( ops ) {

  override val isImmutableModelTerminologyGraph = true
  override val isMutableModelTerminologyGraph = false

  def setTerminologyGraphShortName
  (shortName: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Try[Unit] =
  for {
    c1 <- getTerminologyGraphShortNameAnnotation match {
      case Some(annotation) =>
        ontManager.applyChange( new RemoveOntologyAnnotation(ont, annotation) ) match {
          case ChangeApplied.SUCCESSFULLY =>
            Success(Unit)
          case ChangeApplied.UNSUCCESSFULLY =>
            Failure(new IllegalArgumentException(
              s"Failed to remove the tbox ontology 'rdfs:label' annotation"))
        }
      case None =>
        Success(Unit)
    }
    c2 <- shortName match {
      case None =>
        Success(Unit)
      case Some(label) =>
        ontManager.applyChange(new AddOntologyAnnotation(
          ont,
          owlDataFactory.getOWLAnnotation(
            omfStore.RDFS_LABEL,
            owlDataFactory.getOWLLiteral(label)))) match {
          case ChangeApplied.SUCCESSFULLY =>
            Success(Unit)
          case ChangeApplied.UNSUCCESSFULLY =>
            Failure(new IllegalArgumentException(
              s"Failed to add the tbox ontology 'rdfs:label' annotation"))
        }
    }
  } yield ()

  def setTerminologyGraphUUID
  (uuid: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Try[Unit] =
    for {
      c1 <- getTerminologyGraphUUIDAnnotation match {
        case Some(annotation) =>
          ontManager.applyChange( new RemoveOntologyAnnotation(ont, annotation) ) match {
            case ChangeApplied.SUCCESSFULLY =>
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to remove the tbox ontology 'uuid' annotation"))
          }
        case None =>
          Success(Unit)
      }
      c2 <- uuid match {
        case None =>
          Success(Unit)
        case Some(id) =>
          ontManager.applyChange(new AddOntologyAnnotation(
            ont,
            owlDataFactory.getOWLAnnotation(
              omfStore.ANNOTATION_HAS_UUID,
              owlDataFactory.getOWLLiteral(id)))) match {
            case ChangeApplied.SUCCESSFULLY =>
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to add the tbox ontology 'uuid' annotation"))
          }
      }
    } yield ()


  import ops._
  import EntityConflictException._

  val rdfs_labelAP = owlDataFactory.getOWLAnnotationProperty( rdfs_label )
  val isAbstractAP = owlDataFactory.getOWLAnnotationProperty( AnnotationIsAbstract )
  val isDerivedAP = owlDataFactory.getOWLAnnotationProperty( AnnotationIsDerived )

  override protected val aspects = scala.collection.mutable.ListBuffer[ModelEntityAspect]()
  override protected val concepts = scala.collection.mutable.ListBuffer[ModelEntityConcept]()
  override protected val reifiedRelationships = scala.collection.mutable.ListBuffer[ModelEntityReifiedRelationship]()
  override protected val unreifiedRelationships = scala.collection.mutable.ListBuffer[ModelEntityUnreifiedRelationship]()
  override protected val sc = scala.collection.mutable.ListBuffer[ModelScalarDataType]()
  override protected val st = scala.collection.mutable.ListBuffer[ModelStructuredDataType]()
  override protected val e2sc = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromEntityToScalar]()
  override protected val e2st = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromEntityToStructure]()
  override protected val s2sc = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromStructureToScalar]()
  override protected val s2st = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromStructureToStructure]()
  override protected val ax = scala.collection.mutable.ListBuffer[ModelTermAxiom]()

  override def getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition] =
    ( ( aspects map ( a => a.e -> a ) ) ++
      ( concepts map ( c => c.e -> c ) ) ++
      ( reifiedRelationships map ( r => r.e -> r ) ) ) toMap

  override def getScalarDatatypeDefinitionMap: Map[OWLDatatype, ModelScalarDataType] =
    sc map ( t => t.sc -> t ) toMap

  override protected val iri2typeTerm = scala.collection.mutable.HashMap[IRI, ModelTypeTerm]()

  val backbone = Backbone.createBackbone( ont, kind, ops ).get

  def addTerminologyGraphExtension
  ( extendedG: ModelTerminologyGraph )
  : Try[types.TerminologyGraphDirectExtensionAxiom] =
    {
      val decl = ontManager.getOWLDataFactory.getOWLImportsDeclaration( extendedG.iri )
      val changeApplied = ontManager.applyChange( new AddImport( ont, decl ) )
      Success( types.TerminologyGraphDirectExtensionAxiom(
        extendingChild = this,
        extendedParent = extendedG) )
    }

  def setTermShortName
  ( term: types.ModelTypeTerm,
    shortName: Option[String] )
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Try[Unit] =
    for {
      c1 <- getTermShortNameAnnotationAssertionAxiom(term) match {
        case Some(annotationAssertionAxiom) =>
          ontManager.applyChange( new RemoveAxiom(ont, annotationAssertionAxiom) ) match {
            case ChangeApplied.SUCCESSFULLY =>
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to remove a tbox ontology 'rdfs:label' annotation assertion axiom"))
          }
        case None =>
          Success(Unit)
      }
      c2 <- shortName match {
        case None =>
          Success(Unit)
        case Some(label) =>
          ontManager.applyChange(new AddAxiom(
            ont,
            owlDataFactory.getOWLAnnotationAssertionAxiom(
              omfStore.RDFS_LABEL,
              term.iri,
              owlDataFactory.getOWLLiteral(label)))) match {
            case ChangeApplied.SUCCESSFULLY =>
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to add a tbox ontology 'rdfs:label' annotation assertion axiom"))
          }
      }
    } yield ()

  def setTermShortUUID
  ( term: types.ModelTypeTerm,
    uuid: Option[String] )
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Try[Unit] =
    for {
      c1 <- getTermUUIDAnnotationAssertionAxiom(term) match {
        case Some(annotationAssertionAxiom) =>
          ontManager.applyChange( new RemoveAxiom(ont, annotationAssertionAxiom) ) match {
            case ChangeApplied.SUCCESSFULLY =>
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to remove a tbox ontology 'uuid' annotation assertion axiom"))
          }
        case None =>
          Success(Unit)
      }
      c2 <- uuid match {
        case None =>
          Success(Unit)
        case Some(id) =>
          ontManager.applyChange(new AddAxiom(
            ont,
            owlDataFactory.getOWLAnnotationAssertionAxiom(
              omfStore.ANNOTATION_HAS_UUID,
              term.iri,
              owlDataFactory.getOWLLiteral(id)))) match {
            case ChangeApplied.SUCCESSFULLY =>
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to add a tbox ontology 'uuid' annotation assertion axiom"))
          }
      }
    } yield ()

  protected def createModelEntityAspect
  ( a: OWLClass )
  : ModelEntityAspect = {
    val _a = ModelEntityAspect( a )
    aspects += _a
    iri2typeTerm += a.getIRI -> _a
    _a
  }

  def addEntityAspect
  ( aspectIRI: IRI )
  : Try[types.ModelEntityAspect] =
    iri2typeTerm get aspectIRI match {
    case None =>
      val aspectC = owlDataFactory.getOWLClass( aspectIRI )
      val aspectTerm = createModelEntityAspect( aspectC )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( aspectC ) ) )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubClassOfAxiom( aspectC, backbone.AspectC ) ) )
      Success( aspectTerm )

    case Some( term ) =>
      Failure( EntityConflictException( EntityAspect, aspectIRI, term ) )
  }

  protected def createModelEntityConcept
  ( c: OWLClass,
    isAbstract: Boolean )
  : ModelEntityConcept = {
    val _c = ModelEntityConcept( c, isAbstract )
    concepts += _c
    iri2typeTerm += c.getIRI -> _c
    _c
  }

  def addEntityConcept
  ( conceptIRI: IRI,
    isAbstract: Boolean )
  ( implicit store: OWLAPIOMFGraphStore )
  : Try[types.ModelEntityConcept] =

    iri2typeTerm get conceptIRI match {
      case None =>

        val conceptC = owlDataFactory.getOWLClass( conceptIRI )
        ontManager.applyChange( new AddAxiom( ont,
          owlDataFactory.getOWLDeclarationAxiom( conceptC ) ) )
        ontManager.applyChange( new AddAxiom( ont,
          owlDataFactory.getOWLAnnotationAssertionAxiom(
            isAbstractAP, conceptIRI, owlDataFactory.getOWLLiteral( isAbstract ) ) ) )
        ontManager.applyChange( new AddAxiom( ont,
          owlDataFactory.getOWLSubClassOfAxiom( conceptC, backbone.EntityC ) ) )

        Success( createModelEntityConcept( conceptC, isAbstract ) )

      case Some( term ) =>
        Failure( EntityConflictException( EntityConcept, conceptIRI, term ) )
    }

  protected def createEntityReifiedRelationship
  ( r: OWLClass,
    u: OWLObjectProperty, ui: Option[OWLObjectProperty],
    source: ModelEntityDefinition, rSource: OWLObjectProperty,
    target: ModelEntityDefinition, rTarget: OWLObjectProperty,
    characteristics: Iterable[RelationshipCharacteristics],
    isAbstract: Boolean )
  : types.ModelEntityReifiedRelationship = {
    val _term = ModelEntityReifiedRelationship(
      r,
      u, ui,
      source, rSource,
      target, rTarget,
      characteristics, isAbstract )
    reifiedRelationships += _term
    iri2typeTerm += r.getIRI -> _term
    _term
  }

  protected def makeEntityReifiedRelationship
  ( rIRI: IRI,
    rIRISource: IRI, rIRITarget: IRI,
    uIRI: IRI, uiIRI: Option[IRI],
    source: ModelEntityDefinition, target: ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics],
    isAbstract: Boolean )
  ( implicit store: OWLAPIOMFGraphStore )
  : Try[types.ModelEntityReifiedRelationship] = {

    val sourceC = owlDataFactory.getOWLClass( source.iri )
    val targetC = owlDataFactory.getOWLClass( target.iri )
    val r = owlDataFactory.getOWLClass( rIRI )
    val rSource = owlDataFactory.getOWLObjectProperty( rIRISource )
    val rTarget = owlDataFactory.getOWLObjectProperty( rIRITarget )
    val u = owlDataFactory.getOWLObjectProperty( uIRI )
    val ui = if ( uiIRI.isEmpty ) None else Some( owlDataFactory.getOWLObjectProperty( uiIRI.get ) )
    val term = createEntityReifiedRelationship(
      r, u, ui, source, rSource, target, rTarget, characteristics, isAbstract )

    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLDeclarationAxiom( r ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLAnnotationAssertionAxiom(
        isAbstractAP, rIRI, owlDataFactory.getOWLLiteral( isAbstract ) ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLSubClassOfAxiom( r, backbone.ReifiedObjectPropertyC ) ) )

    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLDeclarationAxiom( rSource ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLSubObjectPropertyOfAxiom( rSource, backbone.topReifiedObjectPropertySourceOP ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLObjectPropertyDomainAxiom( rSource, r ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLObjectPropertyRangeAxiom( rSource, sourceC ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLFunctionalObjectPropertyAxiom( rSource ) ) )

    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLDeclarationAxiom( rTarget ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLSubObjectPropertyOfAxiom( rTarget, backbone.topReifiedObjectPropertyTargetOP ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLObjectPropertyDomainAxiom( rTarget, r ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLObjectPropertyRangeAxiom( rTarget, targetC ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLFunctionalObjectPropertyAxiom( rTarget ) ) )

    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLDeclarationAxiom( u ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLSubObjectPropertyOfAxiom( u, backbone.topReifiedObjectPropertyOP ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLObjectPropertyDomainAxiom( u, sourceC ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLObjectPropertyRangeAxiom( u, targetC ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLSubPropertyChainOfAxiom(
        List( owlDataFactory.getOWLObjectInverseOf( rSource ), rTarget ), u ) ) )

    if ( ui.isDefined ) {
      ontManager.applyChange( new AddAxiom( ont,
        owlDataFactory.getOWLDeclarationAxiom( ui.get ) ) )
      ontManager.applyChange( new AddAxiom( ont,
        owlDataFactory.getOWLAnnotationAssertionAxiom(
          isDerivedAP, ui.get.getIRI, owlDataFactory.getOWLLiteral( true ) ) ) )
      ontManager.applyChange( new AddAxiom( ont,
        owlDataFactory.getOWLSubObjectPropertyOfAxiom( ui.get, backbone.topReifiedObjectPropertyOP ) ) )
      ontManager.applyChange( new AddAxiom( ont,
        owlDataFactory.getOWLObjectPropertyDomainAxiom( ui.get, targetC ) ) )
      ontManager.applyChange( new AddAxiom( ont,
        owlDataFactory.getOWLObjectPropertyRangeAxiom( ui.get, sourceC ) ) )
      ontManager.applyChange( new AddAxiom( ont,
        owlDataFactory.getOWLSubPropertyChainOfAxiom(
          List( owlDataFactory.getOWLObjectInverseOf( rTarget ), rSource ), ui.get ) ) )
    }

    Success( term )

  }

  def addEntityReifiedRelationship
  ( rIRI: IRI,
    rIRISource: IRI, rIRITarget: IRI,
    uIRI: IRI, uiIRI: Option[IRI],
    source: ModelEntityDefinition, target: ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics],
    isAbstract: Boolean )
  ( implicit store: OWLAPIOMFGraphStore )
  : Try[types.ModelEntityReifiedRelationship] =
    ( lookupTypeTerm( rIRI, recursively = true ),
      lookupTypeTerm( rIRISource, recursively = true ),
      lookupTypeTerm( rIRITarget, recursively = true ),
      lookupTypeTerm( uIRI, recursively = true ),
      lookupTypeTerm( uiIRI, recursively = true ) ) match {
        case ( None, None, None, None, None ) =>
          ( isTypeTermDefinedRecursively( source ), isTypeTermDefinedRecursively( target ) ) match {
            case ( true, true ) =>
              makeEntityReifiedRelationship(
                rIRI, rIRISource, rIRITarget, uIRI, uiIRI,
                source, target, characteristics, isAbstract )
            case ( false, true ) =>
              Failure( EntityScopeException(
                EntityReifiedRelationship, rIRI, Map( Source -> source ) ) )

            case ( true, false ) =>
              Failure( EntityScopeException(
                EntityReifiedRelationship, rIRI, Map( Target -> target ) ) )

            case ( false, false ) =>
              Failure( EntityScopeException(
                EntityReifiedRelationship, rIRI, Map( Source -> source, Target -> target ) ) )
          }

        case ( Some( t ), _, _, _, _ ) =>
          Failure( EntityConflictException(
            EntityReifiedRelationship, rIRI, t ) )

        case ( _, Some( t ), _, _, _ ) =>
          Failure( EntityConflictException(
            EntityReifiedRelationship, rIRISource, t ) )

        case ( _, _, Some( t ), _, _ ) =>
          Failure( EntityConflictException(
            EntityReifiedRelationship, rIRITarget, t ) )

        case ( _, _, _, Some( t ), _ ) =>
          Failure( EntityConflictException(
            EntityReifiedRelationship, uIRI, t ) )

        case ( _, _, _, _, Some( t ) ) =>
          require( uiIRI.isDefined )
          Failure( EntityConflictException(
            EntityReifiedRelationship, uiIRI.get, t ) )
      }

  protected def createModelScalarDataType
  ( dt: OWLDatatype )
  : ModelScalarDataType = {
    val _dt = ModelScalarDataType( dt )
    sc += _dt
    iri2typeTerm += dt.getIRI -> _dt
    _dt
  }

  def addScalarDataType
  ( scalarIRI: IRI )
  : Try[types.ModelScalarDataType] =
    iri2typeTerm get scalarIRI match {
    case None =>
      val scalarDT = owlDataFactory.getOWLDatatype( scalarIRI )
      val scalarTerm = createModelScalarDataType( scalarDT )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( scalarDT ) ) )
      Success( scalarTerm )

    case Some( term ) =>
      Failure( EntityConflictException( ScalarDataType, scalarIRI, term ) )
  }

  protected def createModelStructuredDataType
  ( c: OWLClass )
  : ModelStructuredDataType = {
    val _st = ModelStructuredDataType( c )
    st += _st
    iri2typeTerm += c.getIRI -> _st
    _st
  }

  def addStructuredDataType
  ( structuredDataTypeIRI: IRI )
  ( implicit store: OWLAPIOMFGraphStore )
  : Try[types.ModelStructuredDataType] =

    iri2typeTerm get structuredDataTypeIRI match {
      case None =>

        val structuredDataTypeC = owlDataFactory.getOWLClass( structuredDataTypeIRI )
        ontManager.applyChange( new AddAxiom( ont,
          owlDataFactory.getOWLDeclarationAxiom( structuredDataTypeC ) ) )
        ontManager.applyChange( new AddAxiom( ont,
          owlDataFactory.getOWLSubClassOfAxiom( structuredDataTypeC, backbone.StructuredDatatypeC ) ) )

        Success( createModelStructuredDataType( structuredDataTypeC ) )

      case Some( term ) =>
        Failure( EntityConflictException( StructuredDataType, structuredDataTypeIRI, term ) )
    }

  def createDataRelationshipFromEntityToScalar
  ( esc: OWLDataProperty, source: ModelEntityDefinition, target: ModelScalarDataType )
  : ModelDataRelationshipFromEntityToScalar = {
    val _esc = ModelDataRelationshipFromEntityToScalar( esc, source, target )
    e2sc += _esc
    iri2typeTerm += esc.getIRI -> _esc
    _esc
  }

  protected def makeDataRelationshipFromEntityToScalar
  ( dIRI: IRI,
    source: types.ModelEntityDefinition,
    target: types.ModelScalarDataType )
  : Try[types.ModelDataRelationshipFromEntityToScalar] = {
    val escDP = owlDataFactory.getOWLDataProperty( dIRI )
    val escTerm = createDataRelationshipFromEntityToScalar( escDP, source, target )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLDeclarationAxiom( escDP ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLSubDataPropertyOfAxiom( escDP, backbone.topDataPropertyDP ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLDataPropertyDomainAxiom( escDP, source.e ) ) )
    ontManager.applyChange( new AddAxiom( ont,
      owlDataFactory.getOWLDataPropertyRangeAxiom( escDP, owlDataFactory.getOWLDatatype( target.iri ) ) ) )
    Success( escTerm )
  }

  def addDataRelationshipFromEntityToScalar
  ( dIRI: IRI,
    source: types.ModelEntityDefinition,
    target: types.ModelScalarDataType )
  ( implicit store: OWLAPIOMFGraphStore )
  : Try[types.ModelDataRelationshipFromEntityToScalar] = iri2typeTerm get dIRI match {
    case None =>
      ( isTypeTermDefinedRecursively( source ),
        isTypeTermDefinedRecursively( target )
        ) match {
        case ( true, true ) =>
          makeDataRelationshipFromEntityToScalar( dIRI, source, target )
        case ( false, true ) =>
          Failure( EntityScopeException(
            DataRelationshipFromEntityToScalar, dIRI, Map( Source -> source ) ) )

        case ( true, false ) =>
          Failure( EntityScopeException(
            DataRelationshipFromEntityToScalar, dIRI, Map( Target -> target ) ) )

        case ( false, false ) =>
          Failure( EntityScopeException(
            DataRelationshipFromEntityToScalar, dIRI, Map( Source -> source, Target -> target ) ) )
      }

    case Some( term ) =>
      Failure( EntityConflictException(
        DataRelationshipFromEntityToScalar, dIRI, term ) )
  }

  def addDataRelationshipFromEntityToStructure
  ( dIRI: IRI,
    source: types.ModelEntityDefinition,
    target: types.ModelStructuredDataType )
  : Try[types.ModelDataRelationshipFromEntityToStructure] = ???

  def addDataRelationshipFromStructureToScalar
  ( dIRI: IRI,
    source: types.ModelStructuredDataType,
    target: types.ModelScalarDataType )
  : Try[types.ModelDataRelationshipFromStructureToScalar] = ???

  def addDataRelationshipFromStructureToStructure
  ( dIRI: IRI,
    source: types.ModelStructuredDataType,
    target: types.ModelStructuredDataType )
  : Try[types.ModelDataRelationshipFromStructureToStructure] = ???

  def addEntityConceptSubClassAxiom
  ( sub: types.ModelEntityConcept,
    sup: types.ModelEntityConcept )
  ( implicit store: OWLAPIOMFGraphStore )
  : Try[types.EntityConceptSubClassAxiom] =
    ( isTypeTermDefinedRecursively( sub ),
      isTypeTermDefinedRecursively( sup ) ) match {
      case ( true, true ) =>
        val subC = owlDataFactory.getOWLClass( sub.iri )
        val supC = owlDataFactory.getOWLClass( sup.iri )
        ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubClassOfAxiom( subC, supC ) ) )
        val axiom = EntityConceptSubClassAxiom( sub, sup )
        ax += axiom
        Success( axiom )

      case ( false, true ) =>
        Failure( AxiomScopeException( ConceptSubclassAxiom, Map( Sub -> sub ) ) )

      case ( true, false ) =>
        Failure( AxiomScopeException( ConceptSubclassAxiom, Map( Sup -> sup ) ) )

      case ( false, false ) =>
        Failure( AxiomScopeException( ConceptSubclassAxiom, Map( Sub -> sub, Sup -> sup ) ) )
    }

  def addEntityConceptUniversalRestrictionAxiom
  ( sub: types.ModelEntityConcept,
    rel: types.ModelEntityReifiedRelationship,
    range: types.ModelEntityDefinition )
  ( implicit store: OWLAPIOMFGraphStore )
  : Try[types.EntityConceptUniversalRestrictionAxiom] =
    ( isTypeTermDefinedRecursively( sub ),
      isTypeTermDefinedRecursively( rel ),
      isTypeTermDefinedRecursively( range ) ) match {
      case ( true, true, true ) =>
        val subC = owlDataFactory.getOWLClass( sub.iri )
        val rangeC = owlDataFactory.getOWLClass( range.iri )
        ontManager.applyChange( 
            new AddAxiom( ont, 
                owlDataFactory.getOWLSubClassOfAxiom( 
                    subC, 
                    owlDataFactory.getOWLObjectAllValuesFrom( rel.unreified, rangeC ) ) ) )
        val axiom = EntityConceptUniversalRestrictionAxiom( sub, rel, range )
        ax += axiom
        Success( axiom )

      case ( false, _, _ ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Sub -> sub ) ) )

      case ( _, false, _ ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Rel -> rel ) ) )

      case ( _, _, false ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Range -> range ) ) )

    }
  
  def addEntityConceptExistentialRestrictionAxiom
  ( sub: types.ModelEntityConcept,
    rel: types.ModelEntityReifiedRelationship,
    range: types.ModelEntityDefinition )
  ( implicit store: OWLAPIOMFGraphStore )
  : Try[types.EntityConceptExistentialRestrictionAxiom] =
    ( isTypeTermDefinedRecursively( sub ),
      isTypeTermDefinedRecursively( rel ),
      isTypeTermDefinedRecursively( range ) ) match {
      case ( true, true, true ) =>
        val subC = owlDataFactory.getOWLClass( sub.iri )
        val rangeC = owlDataFactory.getOWLClass( range.iri )
        ontManager.applyChange( 
            new AddAxiom( ont, 
                owlDataFactory.getOWLSubClassOfAxiom( 
                    subC, 
                    owlDataFactory.getOWLObjectSomeValuesFrom( rel.unreified, rangeC ) ) ) )
        val axiom = EntityConceptExistentialRestrictionAxiom( sub, rel, range )
        ax += axiom
        Success( axiom )

      case ( false, _, _ ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Sub -> sub ) ) )

      case ( _, false, _ ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Rel -> rel ) ) )

      case ( _, _, false ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Range -> range ) ) )
    }
  
  def addEntityDefinitionAspectSubClassAxiom
  ( sub: types.ModelEntityDefinition,
    sup: types.ModelEntityAspect )
  ( implicit store: OWLAPIOMFGraphStore )
  : Try[types.EntityDefinitionAspectSubClassAxiom] =
    ( isTypeTermDefinedRecursively( sub ),
      isTypeTermDefinedRecursively( sup ) ) match {
      case ( true, true ) =>
        ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubClassOfAxiom( sub.e, sup.e ) ) )
        val axiom = EntityDefinitionAspectSubClassAxiom( sub, sup )
        ax += axiom
        Success( axiom )

      case ( false, true ) =>
        Failure( AxiomScopeException( AspectSubclassAxiom, Map( Sub -> sub ) ) )

      case ( true, false ) =>
        Failure( AxiomScopeException( AspectSubclassAxiom, Map( Sup -> sub ) ) )

      case ( false, false ) =>
        Failure( AxiomScopeException( AspectSubclassAxiom, Map( Sub -> sub, Sup -> sub ) ) )
    }

  def addEntityReifiedRelationshipSubClassAxiom
  ( sub: types.ModelEntityReifiedRelationship,
    sup: types.ModelEntityReifiedRelationship )
  ( implicit store: OWLAPIOMFGraphStore )
  : Try[types.EntityReifiedRelationshipSubClassAxiom] =
    ( isTypeTermDefinedRecursively( sub ), isTypeTermDefinedRecursively( sup ) ) match {
      case ( true, true ) =>
        ontManager.applyChange( new AddAxiom( ont,
          owlDataFactory.getOWLSubClassOfAxiom( sub.e, sup.e ) ) )
        ontManager.applyChange( new AddAxiom( ont,
          owlDataFactory.getOWLSubObjectPropertyOfAxiom( sub.rSource, sup.rSource ) ) )
        ontManager.applyChange( new AddAxiom( ont,
          owlDataFactory.getOWLSubObjectPropertyOfAxiom( sub.rTarget, sup.rTarget) ) )
        val axiom = EntityReifiedRelationshipSubClassAxiom( sub, sup )
        ax += axiom
        Success( axiom )

      case ( false, true ) =>
        Failure( AxiomScopeException( ReifiedRelationshipSubclassAxiom, Map( Sub -> sub ) ) )

      case ( true, false ) =>
        Failure( AxiomScopeException( ReifiedRelationshipSubclassAxiom, Map( Sup -> sup ) ) )

      case ( false, false ) =>
        Failure( AxiomScopeException( ReifiedRelationshipSubclassAxiom, Map( Sub -> sub, Sup -> sup ) ) )
    }
}