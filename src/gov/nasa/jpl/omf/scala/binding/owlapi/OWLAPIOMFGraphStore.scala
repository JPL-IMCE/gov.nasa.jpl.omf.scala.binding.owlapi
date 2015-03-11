/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.binding.owlapi

import scala.Iterable
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyAlreadyExistsException
import org.semanticweb.owlapi.model.OWLOntologyCreationException
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper
import org.semanticweb.owlapi.model.OWLOntologyManager
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.ResolverHelper
import org.semanticweb.owlapi.util.PriorityCollection
import java.io.OutputStream
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.AddAxiom
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLAnnotation
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

case class OWLAPIOMFGraphStore( val omfModule: OWLAPIOMFModule, val ontManager: OWLOntologyManager ) {

  val catalogIRIMapper: Option[CatalogIRIMapper] =
    for {
      catalogManager <- omfModule.catalogManager
    } yield {
      val mappers: PriorityCollection[OWLOntologyIRIMapper] = ontManager.getIRIMappers()

      val mapper = new CatalogIRIMapper( catalogManager )
      mappers.add( Iterable[OWLOntologyIRIMapper]( mapper ).asJava )

      mapper
    }

  protected lazy val omfModelOntology = {
    val o = ontManager.loadOntology( omfModule.omfOntologyIRI )
    require( o != null, s"Could not find the OMF metadata ontology: ${omfModule.omfOntologyIRI}" )
    o
  }

  protected lazy val omfModelClasses = omfModelOntology.getClassesInSignature( Imports.EXCLUDED ).map { c => ( c.getIRI.getRemainder.get -> c ) } toMap;
  protected lazy val omfModelObjectProperties = omfModelOntology.getObjectPropertiesInSignature( Imports.EXCLUDED ).map { op => ( op.getIRI.getRemainder.get -> op ) } toMap;
  protected lazy val omfModelDataProperties = omfModelOntology.getDataPropertiesInSignature( Imports.EXCLUDED ).map { dp => ( dp.getIRI.getRemainder.get -> dp ) } toMap;
  protected lazy val allAnnotationProperties = omfModelOntology.getAnnotationPropertiesInSignature( Imports.INCLUDED ).map { ap => ( ap.getIRI.getShortForm -> ap ) } toMap;

  lazy val RDFS_LABEL = ontManager.getOWLDataFactory.getRDFSLabel

  // OMF model.

  // ModelTermAxiom
  lazy val OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM = omfModelClasses( "EntityConceptExistentialRestrictionAxiom" )
  protected val OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM2Instance = scala.collection.mutable.HashMap[types.EntityConceptExistentialRestrictionAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM = omfModelClasses( "EntityConceptUniversalRestrictionAxiom" )
  protected val OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM2Instance = scala.collection.mutable.HashMap[types.EntityConceptUniversalRestrictionAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM = omfModelClasses( "EntityConceptSubClassAxiom" )
  protected val OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance = scala.collection.mutable.HashMap[types.EntityConceptSubClassAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM = omfModelClasses( "EntityDefinitionAspectSubClassAxiom" )
  protected val OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM2Instance = scala.collection.mutable.HashMap[types.EntityDefinitionAspectSubClassAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_RELATIONSHIP_SUB_CLASS_AXIOM = omfModelClasses( "EntityRelationshipSubClassAxiom" )
  protected val OMF_ENTITY_RELATIONSHIP_SUB_CLASS_AXIOM2Instance = scala.collection.mutable.HashMap[types.EntityRelationshipSubClassAxiom, OWLNamedIndividual]()

  lazy val OMF_SCALAR_DATA_TYPE_FACET_RESTRICTION = omfModelClasses( "ScalarDataTypeFacetRestriction" )
  protected val OMF_SCALAR_DATA_TYPE_FACET_RESTRICTION2Instance = scala.collection.mutable.HashMap[types.ScalarDataTypeFacetRestriction, OWLNamedIndividual]()

  lazy val OMF_MODEL_TERMINOLOGY_GRAPH = omfModelClasses( "ModelTerminologyGraph" )
  protected val OMF_MODEL_TERMINOLOGY_GRAPH2Instance = scala.collection.mutable.HashMap[types.ModelTerminologyGraph, OWLNamedIndividual]()

  // ModelTypeTerm  
  // ModelDataRelationship
  lazy val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR = omfModelClasses( "ModelDataRelationshipFromEntityToScalar" )
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR2Instance = scala.collection.mutable.HashMap[types.ModelDataRelationshipFromEntityToScalar, OWLNamedIndividual]()

  lazy val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_STRUCTURE = omfModelClasses( "ModelDataRelationshipFromEntityToStructure" )
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_STRUCTURE2Instance = scala.collection.mutable.HashMap[types.ModelDataRelationshipFromEntityToStructure, OWLNamedIndividual]()

  lazy val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_SCALAR = omfModelClasses( "ModelDataRelationshipFromStructureToScalar" )
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_SCALAR2Instance = scala.collection.mutable.HashMap[types.ModelDataRelationshipFromStructureToScalar, OWLNamedIndividual]()

  lazy val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_STRUCTURE = omfModelClasses( "ModelDataRelationshipFromStructureToStructure" )
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_STRUCTURE2Instance = scala.collection.mutable.HashMap[types.ModelDataRelationshipFromStructureToStructure, OWLNamedIndividual]()

  // ModelDataTypeDefinition
  lazy val OMF_MODEL_SCALAR_DATA_TYPE = omfModelClasses( "ModelScalarDataType" )
  protected val OMF_MODEL_SCALAR_DATA_TYPE2Instance = scala.collection.mutable.HashMap[types.ModelScalarDataType, OWLNamedIndividual]()

  lazy val OMF_MODEL_STRUCTURED_DATA_TYPE = omfModelClasses( "ModelStructuredDataType" )
  protected val OMF_MODEL_STRUCTURED_DATA_TYPE2Instance = scala.collection.mutable.HashMap[types.ModelStructuredDataType, OWLNamedIndividual]()

  // ModelEntityDefinition
  protected val OMF_MODEL_ENTITY_DEFINITION2Instance = scala.collection.mutable.HashMap[types.ModelEntityDefinition, OWLNamedIndividual]()

  lazy val OMF_MODEL_ENTITY_ASPECT = omfModelClasses( "ModelEntityAspect" )
  protected val OMF_MODEL_ENTITY_ASPECT2Instance = scala.collection.mutable.HashMap[types.ModelEntityAspect, OWLNamedIndividual]()

  lazy val OMF_MODEL_ENTITY_CONCEPT = omfModelClasses( "ModelEntityConcept" )
  protected val OMF_MODEL_ENTITY_CONCEPT2Instance = scala.collection.mutable.HashMap[types.ModelEntityConcept, OWLNamedIndividual]()

  lazy val OMF_MODEL_ENTITY_RELATIONSHIP = omfModelClasses( "ModelEntityRelationship" )
  protected val OMF_MODEL_ENTITY_RELATIONSHIP2Instance = scala.collection.mutable.HashMap[types.ModelEntityRelationship, OWLNamedIndividual]()

  // Object Properties
  lazy val OMF_DEFINES_TYPE_TERM = omfModelObjectProperties( "definesTypeTerm" )
  lazy val OMF_HAS_ASSERTS_AXIOM = omfModelObjectProperties( "hasAssertsAxiom" )
  lazy val OMF_HAS_GENERAL_ASPECT = omfModelObjectProperties( "hasGeneralAspect" )
  lazy val OMF_HAS_GENERAL_CONCEPT = omfModelObjectProperties( "hasGeneralConcept" )
  lazy val OMF_HAS_GENERAL_RELATIONSHIP = omfModelObjectProperties( "hasGeneralRelationship" )
  lazy val OMF_HAS_GRAPH = omfModelObjectProperties( "hasGraph" )
  lazy val OMF_HAS_RESTRICTED_RANGE = omfModelObjectProperties( "hasRestrictedRange" )
  lazy val OMF_HAS_SOURCE = omfModelObjectProperties( "hasSource" )
  lazy val OMF_HAS_SPECIFIC_CONCEPT = omfModelObjectProperties( "hasSpecificConcept" )
  lazy val OMF_HAS_SPECIFIC_ENTITY = omfModelObjectProperties( "hasSpecificEntity" )
  lazy val OMF_HAS_SPECIFIC_RELATIONSHIP = omfModelObjectProperties( "hasSpecificRelationship" )
  lazy val OMF_HAS_TARGET = omfModelObjectProperties( "hasTarget" )
  lazy val OMF_IMPORTS = omfModelObjectProperties( "imports" )
  lazy val OMF_IS_GRAPH_OF_ENTITY = omfModelObjectProperties( "isGraphOfEntity" )
  lazy val OMF_RESTRICTS_CONCEPT = omfModelObjectProperties( "restrictsConcept" )
  lazy val OMF_RESTRICTS_RELATIONSHIP = omfModelObjectProperties( "restrictsRelationship" )

  // Data Properties
  lazy val OMF_HAS_IRI = omfModelDataProperties( "hasIRI" )
  lazy val OMF_HAS_NAME = omfModelDataProperties( "hasName" )
  lazy val OMF_HAS_PROVENANCE_FROM_RULE = omfModelDataProperties( "hasProvenanceFromRule" )
  lazy val OMF_HAS_RELATIVE_IRI_PATH = omfModelDataProperties( "hasRelativeIRIPath" )
  lazy val OMF_HAS_QUALIFIED_NAME = omfModelDataProperties( "hasQualifiedName" )
  lazy val OMF_HAS_UUID = omfModelDataProperties( "hasUUID" )
  lazy val OMF_IS_ABSTRACT = omfModelDataProperties( "isAbstract" )
  lazy val OMF_IS_ASYMMETRIC = omfModelDataProperties( "isAsymmetric" )
  lazy val OMF_IS_FUNCTIONAL = omfModelDataProperties( "isFunctional" )
  lazy val OMF_IS_INVERSE_FUNCTIONAL = omfModelDataProperties( "isInverseFunctional" )
  lazy val OMF_IS_IRREFLEXIVE = omfModelDataProperties( "isIrreflexive" )
  lazy val OMF_IS_REFLEXIVE = omfModelDataProperties( "isReflexive" )
  lazy val OMF_IS_SYMMETRIC = omfModelDataProperties( "isSymmetric" )
  lazy val OMF_IS_TRANSITIVE = omfModelDataProperties( "isTransitive" )

  val immutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, types.ImmutableModelTerminologyGraph]()
  val mutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, types.MutableModelTerminologyGraph]()

  // OMF Ontology Instance Model Constructors  

  val owlDataFactory = ontManager.getOWLDataFactory

  def makeMetadataInstanceIRI( o: OWLOntology, instanceKind: String, map: scala.collection.Map[_, OWLNamedIndividual] ): IRI =
    omfModule.ops.withFragment( o.getOntologyID.getOntologyIRI.get, instanceKind + ( map.size ) ).get

  def registerOMFModelTerminologyGraphMapping(
    o: OWLOntology,
    graphT: types.ModelTerminologyGraph ): Unit = {

    def registerTBoxGraphDefinitions( tboxT: types.ModelTerminologyGraph ): Unit =
      if ( !OMF_MODEL_TERMINOLOGY_GRAPH2Instance.contains( tboxT ) ) {
        val tboxI = owlDataFactory.getOWLNamedIndividual( makeMetadataInstanceIRI( o, "G", OMF_MODEL_TERMINOLOGY_GRAPH2Instance ) )
        OMF_MODEL_TERMINOLOGY_GRAPH2Instance += ( tboxT -> tboxI )
        //System.out.println( s"*** Begin registering ${tboxT.iri} as ${tboxI}..." )
        val ( _, _e, _, _, _a, _c, _r, _sc, _st, _esc, _est, _ssc, _sst, _ax ) = omfModule.ops.fromTerminologyGraph( tboxT )
        require( _e.isEmpty )
        _a foreach ( registerOMFModelEntityAspectInstance( o, _ ) )
        _c foreach ( registerOMFModelEntityConceptInstance( o, _ ) )
        _r foreach ( registerOMFModelEntityRelationshipInstance( o, _ ) )
        //System.out.println( s"*** Finished registering ${tboxT.iri} as ${tboxI}" )
      }

    def registerTBoxGraph(
      queue: List[types.ModelTerminologyGraph],
      ready: List[types.ModelTerminologyGraph],
      visited: List[types.ModelTerminologyGraph] ): Unit = {

      //      System.out.println( s"*** registerTBoxGraph(queue=${queue.size}, ready=${ready.size}, visited=${visited.size})" )
      //      queue.foreach( q => System.out.println( s"queue: ${q.iri}" ) )
      //      ready.foreach( r => System.out.println( s"ready: ${r.iri}" ) )
      //      visited.foreach( v => System.out.println( s"visited: ${v.iri}" ) )
      queue match {
        case Nil =>
          ready match {
            case Nil =>
              ()
            case g :: gs =>
              if ( visited.contains( g ) ) {
                //System.out.println( s"- visited: ${g.iri}" )
                registerTBoxGraph( Nil, gs, visited )
              } else {
                registerTBoxGraphDefinitions( g )
                registerTBoxGraph( Nil, gs, g :: visited )
              }
          }
        case g :: gs =>
          if ( ready.contains( g ) ) {
            //System.out.println( s"- skip: ${g.iri}" )
            registerTBoxGraph( gs, ready, visited )
          } else {
            //System.out.println( s"- imports: ${g.iri}" )
            registerTBoxGraph( g.imports.toList ::: gs, g :: ready, visited )
          }
      }
    }

    registerTBoxGraph( List( graphT ), List(), List() )
  }

  def createOMFModelTerminologyGraph(
    o: OWLOntology,
    hasProvenanceFromRule: String,
    hasRelativeIRIPath: String,
    graphT: types.ModelTerminologyGraph,
    hasName: String,
    hasQualifiedName: String,
    hasUUID: String ): Unit = {
    val graphI = owlDataFactory.getOWLNamedIndividual( makeMetadataInstanceIRI( o, "G", OMF_MODEL_TERMINOLOGY_GRAPH2Instance ) )
    OMF_MODEL_TERMINOLOGY_GRAPH2Instance += ( graphT -> graphI )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( graphI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_MODEL_TERMINOLOGY_GRAPH, graphI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, graphI, hasProvenanceFromRule ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_IRI, graphI, graphT.iri.toString ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_RELATIVE_IRI_PATH, graphI, hasRelativeIRIPath ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, graphI, hasName ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, graphI, hasQualifiedName ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, graphI, hasUUID ) ) )
  }

  def createOMFModelTerminologyGraphExtension(
    o: OWLOntology,
    extendingG: types.MutableModelTerminologyGraph,
    extendedG: types.ModelTerminologyGraph ): Unit = {
    val extendingI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance( extendingG )
    val extendedI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance( extendedG )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_IMPORTS, extendingI, extendedI ) ) )
  }

  def registerOMFModelEntityAspectInstance(
    o: OWLOntology,
    aspectT: types.ModelEntityAspect,
    hasQualifiedName: Option[String] = None ): OWLNamedIndividual =
    OMF_MODEL_ENTITY_ASPECT2Instance.get( aspectT ) match {
      case Some( aspectI ) =>
        //System.out.println( s"#! Aspect: ${aspectT.iri}" )
        aspectI
      case None =>
        val aspectI = owlDataFactory.getOWLNamedIndividual( makeMetadataInstanceIRI( o, "A", OMF_MODEL_ENTITY_ASPECT2Instance ) )
        OMF_MODEL_ENTITY_DEFINITION2Instance += ( aspectT -> aspectI )
        OMF_MODEL_ENTITY_ASPECT2Instance += ( aspectT -> aspectI )
        ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( aspectI ) ) )
        ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_MODEL_ENTITY_ASPECT, aspectI ) ) )
        hasQualifiedName match {
          case None => ()
          case Some( qName ) => ontManager.applyChange( new AddAxiom(
            o,
            owlDataFactory.getOWLAnnotationAssertionAxiom( RDFS_LABEL, aspectI.getIRI, owlDataFactory.getOWLLiteral( qName ) ) ) )
        }
        //System.out.println( s"## Aspect: ${aspectT.iri}" )
        aspectI
    }

  def createOMFModelEntityAspectInstance(
    o: OWLOntology,
    hasProvenanceFromRule: String,
    aspectT: types.ModelEntityAspect,
    hasName: String,
    hasQualifiedName: String,
    hasUUID: String ): Unit = {
    val aspectI = registerOMFModelEntityAspectInstance( o, aspectT, Some( hasQualifiedName ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, aspectI, hasProvenanceFromRule ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_IRI, aspectI, aspectT.iri.toString ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, aspectI, hasName ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, aspectI, hasQualifiedName ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, aspectI, hasUUID ) ) )
  }

  def registerOMFModelEntityConceptInstance(
    o: OWLOntology,
    conceptT: types.ModelEntityConcept,
    hasQualifiedName: Option[String] = None ): OWLNamedIndividual =
    OMF_MODEL_ENTITY_CONCEPT2Instance.get( conceptT ) match {
      case Some( conceptI ) =>
        //System.out.println( s"#! Concept: ${conceptT.iri}" )
        conceptI
      case None =>
        val conceptI = owlDataFactory.getOWLNamedIndividual( makeMetadataInstanceIRI( o, "C", OMF_MODEL_ENTITY_CONCEPT2Instance ) )
        OMF_MODEL_ENTITY_DEFINITION2Instance += ( conceptT -> conceptI )
        OMF_MODEL_ENTITY_CONCEPT2Instance += ( conceptT -> conceptI )
        ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( conceptI ) ) )
        ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_MODEL_ENTITY_CONCEPT, conceptI ) ) )
        hasQualifiedName match {
          case None => ()
          case Some( qName ) => ontManager.applyChange( new AddAxiom(
            o,
            owlDataFactory.getOWLAnnotationAssertionAxiom( RDFS_LABEL, conceptI.getIRI, owlDataFactory.getOWLLiteral( qName ) ) ) )
        }
        //System.out.println( s"## Concept: ${conceptT.iri}" )
        conceptI
    }

  def createOMFModelEntityConceptInstance(
    o: OWLOntology,
    hasProvenanceFromRule: String,
    conceptT: types.ModelEntityConcept,
    graphO: Option[types.MutableModelTerminologyGraph],
    hasName: String,
    hasQualifiedName: String,
    hasUUID: String,
    isAbstract: Boolean ): Unit = {
    val conceptI = registerOMFModelEntityConceptInstance( o, conceptT, Some( hasQualifiedName ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, conceptI, hasProvenanceFromRule ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_IRI, conceptI, conceptT.iri.toString ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, conceptI, hasName ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, conceptI, hasQualifiedName ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, conceptI, hasUUID ) ) )
    graphO match {
      case None => ()
      case Some( graphT ) =>
        val graphI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance( graphT )
        ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_GRAPH, conceptI, graphI ) ) )
        ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_IS_GRAPH_OF_ENTITY, graphI, conceptI ) ) )
    }
  }

  def registerOMFModelEntityRelationshipInstance(
    o: OWLOntology,
    relationshipT: types.ModelEntityRelationship,
    hasQualifiedName: Option[String] = None ): OWLNamedIndividual =
    OMF_MODEL_ENTITY_RELATIONSHIP2Instance.get( relationshipT ) match {
      case Some( relationshipI ) =>
        //System.out.println( s"#! Relationship: ${relationshipT.iri}" )
        relationshipI
      case None =>
        val relationshipI = owlDataFactory.getOWLNamedIndividual( makeMetadataInstanceIRI( o, "R", OMF_MODEL_ENTITY_RELATIONSHIP2Instance ) )
        OMF_MODEL_ENTITY_DEFINITION2Instance += ( relationshipT -> relationshipI )
        OMF_MODEL_ENTITY_RELATIONSHIP2Instance += ( relationshipT -> relationshipI )
        ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( relationshipI ) ) )
        ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_MODEL_ENTITY_RELATIONSHIP, relationshipI ) ) )
        hasQualifiedName match {
          case None => ()
          case Some( qName ) => ontManager.applyChange( new AddAxiom(
            o,
            owlDataFactory.getOWLAnnotationAssertionAxiom( RDFS_LABEL, relationshipI.getIRI, owlDataFactory.getOWLLiteral( qName ) ) ) )
        }
        //System.out.println( s"## Relationship: ${relationshipT.iri}" )
        relationshipI
    }

  def createOMFModelEntityRelationshipInstance(
    o: OWLOntology,
    hasProvenanceFromRule: String,
    relationshipT: types.ModelEntityRelationship,
    graphO: Option[types.MutableModelTerminologyGraph],
    hasName: String,
    hasQualifiedName: String,
    hasUUID: String,
    isAbstract: Boolean ): Unit = {
    val relationshipI = registerOMFModelEntityRelationshipInstance( o, relationshipT, Some( hasQualifiedName ) )
    val sourceI = OMF_MODEL_ENTITY_DEFINITION2Instance( relationshipT.source )
    val targetI = OMF_MODEL_ENTITY_DEFINITION2Instance( relationshipT.target)    
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, relationshipI, hasProvenanceFromRule ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_IRI, relationshipI, relationshipT.iri.toString ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, relationshipI, hasName ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, relationshipI, hasQualifiedName ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, relationshipI, hasUUID ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_SOURCE, relationshipI, sourceI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_TARGET, relationshipI, targetI ) ) )
    graphO match {
      case None => ()
      case Some( graphT ) =>
        val graphI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance( graphT )
        ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_GRAPH, relationshipI, graphI ) ) )
        ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_IS_GRAPH_OF_ENTITY, graphI, relationshipI ) ) )
    }
  }

  def createOMFEntityDefinitionAspectSubClassAxiomInstance(
    o: OWLOntology,
    hasProvenanceFromRule: String,
    axiomT: types.EntityDefinitionAspectSubClassAxiom,
    subT: types.ModelEntityDefinition,
    supT: types.ModelEntityAspect ): Unit = {
    val subI = OMF_MODEL_ENTITY_DEFINITION2Instance( subT )
    val supI = OMF_MODEL_ENTITY_ASPECT2Instance( supT )
    val axiomI = owlDataFactory.getOWLNamedIndividual( makeMetadataInstanceIRI( o, "DefinitionAspectSubClass", OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM2Instance ) )
    OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM2Instance += ( axiomT -> axiomI )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( axiomI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM, axiomI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_GENERAL_ASPECT, axiomI, supI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_SPECIFIC_ENTITY, axiomI, subI ) ) )
  }

  def createOMFEntityConceptSubClassAxiomInstance(
    o: OWLOntology,
    hasProvenanceFromRule: String,
    axiomT: types.EntityConceptSubClassAxiom,
    subT: types.ModelEntityConcept,
    supT: types.ModelEntityConcept ): Unit = {
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance( subT )
    val supI = OMF_MODEL_ENTITY_CONCEPT2Instance( supT )
    val axiomI = owlDataFactory.getOWLNamedIndividual( makeMetadataInstanceIRI( o, "ConceptSubClass", OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance ) )
    OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance += ( axiomT -> axiomI )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( axiomI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM, axiomI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_GENERAL_CONCEPT, axiomI, supI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_SPECIFIC_CONCEPT, axiomI, subI ) ) )
  }

  def createOMFEntityConceptUniversalRestrictionAxiomInstance(
    o: OWLOntology,
    hasProvenanceFromRule: String,
    axiomT: types.EntityConceptUniversalRestrictionAxiom,
    subT: types.ModelEntityConcept,
    relT: types.ModelEntityRelationship,
    rangeT: types.ModelEntityDefinition ): Unit = {
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance( subT )
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance( relT )
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance( rangeT )
    val axiomI = owlDataFactory.getOWLNamedIndividual( makeMetadataInstanceIRI( o, "UniversalConceptRestriction", OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM2Instance ) )
    OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM2Instance += ( axiomT -> axiomI )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( axiomI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM, axiomI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_RESTRICTS_CONCEPT, axiomI, subI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_RESTRICTS_RELATIONSHIP, axiomI, relI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_RESTRICTED_RANGE, axiomI, rangeI ) ) )
  }

  def createOMFEntityConceptExistentialRestrictionAxiomInstance(
    o: OWLOntology,
    hasProvenanceFromRule: String,
    axiomT: types.EntityConceptExistentialRestrictionAxiom,
    subT: types.ModelEntityConcept,
    relT: types.ModelEntityRelationship,
    rangeT: types.ModelEntityDefinition ): Unit = {
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance( subT )
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance( relT )
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance( rangeT )
    val axiomI = owlDataFactory.getOWLNamedIndividual( makeMetadataInstanceIRI( o, "ExistentialConceptRestriction", OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM2Instance ) )
    OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM2Instance += ( axiomT -> axiomI )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( axiomI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM, axiomI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_RESTRICTS_CONCEPT, axiomI, subI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_RESTRICTS_RELATIONSHIP, axiomI, relI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_RESTRICTED_RANGE, axiomI, rangeI ) ) )
  }

  def createOMFEntityRelationshipSubClassAxiomInstance(
    o: OWLOntology,
    hasProvenanceFromRule: String,
    axiomT: types.EntityRelationshipSubClassAxiom,
    subT: types.ModelEntityRelationship,
    supT: types.ModelEntityRelationship ): Unit = {
    val subI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance( subT )
    val supI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance( supT )
    val axiomI = owlDataFactory.getOWLNamedIndividual( makeMetadataInstanceIRI( o, "RelationshipSubClass", OMF_ENTITY_RELATIONSHIP_SUB_CLASS_AXIOM2Instance ) )
    OMF_ENTITY_RELATIONSHIP_SUB_CLASS_AXIOM2Instance += ( axiomT -> axiomI )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( axiomI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_ENTITY_RELATIONSHIP_SUB_CLASS_AXIOM, axiomI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_GENERAL_RELATIONSHIP, axiomI, supI ) ) )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom( OMF_HAS_SPECIFIC_RELATIONSHIP, axiomI, subI ) ) )
  }

  // OMF API
  def asImmutableTerminologyGraph( g: types.MutableModelTerminologyGraph ): Try[types.ImmutableModelTerminologyGraph] = {
    import g.ops._
    val ( iri, _e, _k, _i, _a, _c, _r, _sc, _st, _esc, _est, _ssc, _sst, _ax ) = fromTerminologyGraph( g )
    if ( immutableTBoxGraphs.contains( iri ) )
      Failure( new IllegalArgumentException( s"There is already an immutable terminology graph with IRI='${iri}'" ) )
    else {
      val ig = types.ImmutableModelTerminologyGraph(
        _k,
        _i.toList, g.ont, _e,
        _a.toList, _c.toList, _r.toList,
        _sc.toList, _st.toList,
        _esc.toList, _est.toList, _ssc.toList, _sst.toList,
        _ax.toList )( g.ops )
      immutableTBoxGraphs.put( iri, ig )
      Success( ig )
    }
  }

  protected def registerImmutableOntologyAsTerminologyGraph(
    o: OWLOntology,
    extendedTGraphs: Iterable[types.ImmutableModelTerminologyGraph] = Nil )( implicit ops: OWLAPIOMFOps ): Try[types.ImmutableModelTerminologyGraph] = {
    val iri = o.getOntologyID.getOntologyIRI
    if ( !iri.isPresent )
      Failure( new IllegalArgumentException( "An ontology must have an OntologyID with an Ontology IRI" ) )
    else
      immutableTBoxGraphs.get( iri.get ) match {
        case Some( g ) =>
          // already registered.
          Success( g )

        case None =>
          // not yet registered.

          val importedOrExtendedTGraphs = scala.collection.mutable.HashSet[types.ImmutableModelTerminologyGraph]()

          o.getDirectImports foreach ( registerImmutableOntologyAsTerminologyGraph( _ ) match {
            case Failure( t ) =>
              return Failure( t )

            case Success( ig ) =>
              importedOrExtendedTGraphs.add( ig )
          } )

          extendedTGraphs.foreach { tg =>
            immutableTBoxGraphs.get( tg.iri ) match {
              case None => return Failure( new IllegalArgumentException(
                s"Cannot create an ontology with iri='${iri.get}' extending a foreign terminology graph, '${tg.iri}' not managed by this ontology manager" ) )
              case Some( eg ) =>
                importedOrExtendedTGraphs.add( eg )
                val decl = ontManager.getOWLDataFactory.getOWLImportsDeclaration( tg.iri )
                ontManager.applyChange( new AddImport( o, decl ) )
            }
          }

          for {
            g <- types.ImmutableModelTerminologyGraphResolver( ResolverHelper( importedOrExtendedTGraphs, o, ops ) ).resolve
          } yield {
            immutableTBoxGraphs.put( iri.get, g )
            g
          }
      }
  }

  protected def registerMutableOntologyAsTerminologyGraph(
    o: OWLOntology,
    kind: TerminologyKind,
    entity: Option[IRI] )( implicit ops: OWLAPIOMFOps ): Try[types.MutableModelTerminologyGraph] = {
    val iri = o.getOntologyID.getOntologyIRI
    if ( !iri.isPresent )
      Failure( new IllegalArgumentException( "An ontology must have an OntologyID with an Ontology IRI" ) )
    else
      mutableTBoxGraphs.get( iri.get ) match {
        case Some( g ) =>
          // already registered.
          Success( g )

        case None =>
          // not yet registered.
          val g = new types.MutableModelTerminologyGraph(
            kind = kind,
            o,
            entity )
          mutableTBoxGraphs.put( iri.get, g )
          Success( g )
      }
  }

  def saveTerminologyGraph( g: types.MutableModelTerminologyGraph )( implicit ops: OWLAPIOMFOps ): Try[Unit] =
    catalogIRIMapper match {
      case None => Failure( new IllegalArgumentException( s"Cannot save a terminology graph without a catalog IRI mapper" ) )
      case Some( iriMapper ) =>
        val iri = iriMapper.resolveIRI( g.iri, iriMapper.saveResolutionStrategy( _ ) )
        g.save( iri )
    }

  def saveTerminologyGraph( g: types.MutableModelTerminologyGraph, os: OutputStream )( implicit ops: OWLAPIOMFOps ): Try[Unit] =
    g.save( os )

  def loadTerminologyGraph( iri: IRI )( implicit ops: OWLAPIOMFOps ): Try[types.ImmutableModelTerminologyGraph] =
    immutableTBoxGraphs.get( iri ) match {
      case Some( o ) => Success( o )
      case None =>
        try {
          val o =
            if ( ontManager.contains( iri ) ) ontManager.getOntology( iri )
            else ontManager.loadOntology( iri )
          //System.out.println( s"loadTerminologyGraph: iri=${iri}, o=${o.getOWLOntologyManager.getOntologyDocumentIRI( o )}" )
          registerImmutableOntologyAsTerminologyGraph( o )
        } catch {
          case t: OWLOntologyCreationException =>
            Failure( t.fillInStackTrace )
        }
    }

  def makeTerminologyGraph(
    iri: IRI,
    kind: TerminologyKind,
    entity: Option[IRI] )( implicit ops: OWLAPIOMFOps ): Try[types.MutableModelTerminologyGraph] =
    if ( ontManager.contains( iri ) )
      Failure( new IllegalArgumentException( s"An ontology with iri='${iri}' already exists" ) )
    else
      for {
        b <- Backbone.createBackbone( ontManager.createOntology( iri ), kind, omfModule.ops )
        g <- registerMutableOntologyAsTerminologyGraph( b.ont, b.kind, entity )
      } yield g

  def loadInstanceGraph( iri: IRI ): Try[instances.ImmutableModelInstanceGraph] = ???

  def asImmutableInstanceGraph( g: instances.MutableModelInstanceGraph ): Try[instances.ImmutableModelInstanceGraph] = ???

  def makeInstanceGraph(
    iri: IRI,
    instantiatedTGraphs: Iterable[types.ImmutableModelTerminologyGraph],
    extendedIGraphs: Iterable[instances.ImmutableModelInstanceGraph] ): Try[instances.MutableModelInstanceGraph] = ???

}