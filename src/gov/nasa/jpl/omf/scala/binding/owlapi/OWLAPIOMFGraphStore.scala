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
    
  protected val omfModelOntology = ontManager.getOntology( omfModule.omfOntologyIRI )
  protected val omfModelClasses = omfModelOntology.getClassesInSignature( Imports.EXCLUDED ).map { c => ( c.getIRI.getRemainder.get -> c ) } toMap;  
  protected val omfModelObjectProperties = omfModelOntology.getObjectPropertiesInSignature( Imports.EXCLUDED ).map { op => ( op.getIRI.getRemainder.get -> op ) } toMap;
  protected val omfModelDataProperties = omfModelOntology.getDataPropertiesInSignature( Imports.EXCLUDED ).map { dp => ( dp.getIRI.getRemainder.get -> dp ) } toMap;
  
  // OMF model.
  
  // ModelTermAxiom
  val OMF_ENTITY_CONCEPT_RESTRICTION_AXIOM = omfModelClasses("EntityConceptRestrictionAxiom")
  val OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM = omfModelClasses("EntityConceptSubClassAxiom")
  val OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM = omfModelClasses("EntityDefinitionAspectSubClassAxiom")
  val OMF_ENTITY_RELATIONSHIP_SUB_CLASS_AXIOM = omfModelClasses("EntityRelationshipSubClassAxiom")
  val OMF_SCALAR_DATA_TYPE_FACET_RESTRICTION = omfModelClasses("ScalarDataTypeFacetRestriction")
  protected val OMF_ENTITY_CONCEPT_RESTRICTION_AXIOM2Instance = scala.collection.mutable.HashMap[types.EntityConceptRestrictionAxiom, OWLNamedIndividual]()
  protected val OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance = scala.collection.mutable.HashMap[types.EntityConceptSubClassAxiom, OWLNamedIndividual]()
  protected val OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM2Instance = scala.collection.mutable.HashMap[types.EntityDefinitionAspectSubClassAxiom, OWLNamedIndividual]()
  protected val OMF_ENTITY_RELATIONSHIP_SUB_CLASS_AXIOM2Instance = scala.collection.mutable.HashMap[types.EntityRelationshipSubClassAxiom, OWLNamedIndividual]()
  protected val OMF_SCALAR_DATA_TYPE_FACET_RESTRICTION2Instance = scala.collection.mutable.HashMap[types.ScalarDataTypeFacetRestriction, OWLNamedIndividual]()
  
  val OMF_MODEL_TERMINOLOGY_GRAPH = omfModelClasses("ModelTerminologyGraph")
  protected val OMF_MODEL_TERMINOLOGY_GRAPH2Instance = scala.collection.mutable.HashMap[types.ModelTerminologyGraph, OWLNamedIndividual]()

  // ModelTypeTerm  
  // ModelDataRelationship
  val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR = omfModelClasses("ModelDataRelationshipFromEntityToScalar")  
  val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_STRUCTURE = omfModelClasses("ModelDataRelationshipFromEntityToStructure")
  val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_SCALAR = omfModelClasses("ModelDataRelationshipFromStructureToScalar")
  val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_STRUCTURE = omfModelClasses("ModelDataRelationshipFromStructureToStructure")
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR2Instance = scala.collection.mutable.HashMap[types.ModelDataRelationshipFromEntityToScalar, OWLNamedIndividual]()
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_STRUCTURE2Instance = scala.collection.mutable.HashMap[types.ModelDataRelationshipFromEntityToStructure, OWLNamedIndividual]()
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_SCALAR2Instance = scala.collection.mutable.HashMap[types.ModelDataRelationshipFromStructureToScalar, OWLNamedIndividual]()
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_STRUCTURE2Instance = scala.collection.mutable.HashMap[types.ModelDataRelationshipFromStructureToStructure, OWLNamedIndividual]()
    
  // ModelDataTypeDefinition
  val OMF_MODEL_SCALAR_DATA_TYPE = omfModelClasses("ModelScalarDataType")
  val OMF_MODEL_STRUCTURED_DATA_TYPE = omfModelClasses("ModelStructuredDataType")
  protected val OMF_MODEL_SCALAR_DATA_TYPE2Instance = scala.collection.mutable.HashMap[types.ModelScalarDataType, OWLNamedIndividual]()
  protected val OMF_MODEL_STRUCTURED_DATA_TYPE2Instance = scala.collection.mutable.HashMap[types.ModelStructuredDataType, OWLNamedIndividual]()
  
  // ModelEntityDefinition
  val OMF_MODEL_ENTITY_ASPECT = omfModelClasses("ModelEntityAspect")
  val OMF_MODEL_ENTITY_CONCEPT = omfModelClasses("ModelEntityConcept")
  val OMF_MODEL_ENTITY_RELATIONSHIP = omfModelClasses("ModelEntityRelationship")
  protected val OMF_MODEL_ENTITY_ASPECT2Instance = scala.collection.mutable.HashMap[types.ModelEntityAspect, OWLNamedIndividual]()
  protected val OMF_MODEL_ENTITY_CONCEPT2Instance = scala.collection.mutable.HashMap[types.ModelEntityConcept, OWLNamedIndividual]()
  protected val OMF_MODEL_ENTITY_RELATIONSHIP2Instance = scala.collection.mutable.HashMap[types.ModelEntityRelationship, OWLNamedIndividual]()
  
  // Object Properties
  val OMF_DEFINES_TYPE_TERM = omfModelObjectProperties("definesTypeTerm")
  val OMF_HAS_ASSERTS_AXIOM = omfModelObjectProperties("hasAssertsAxiom")
  val OMF_HAS_GENERAL_ASPECT = omfModelObjectProperties("hasGeneralAspect")
  val OMF_HAS_GENERAL_CONCEPT = omfModelObjectProperties("hasGeneralConcept")
  val OMF_HAS_GENERAL_RELATIONSHIP = omfModelObjectProperties("hasGeneralRelationship")
  val OMF_HAS_GRAPH = omfModelObjectProperties("hasGraph")
  val OMF_HAS_RESTRICTED_RANGE = omfModelObjectProperties("hasRestrictedRange")
  val OMF_HAS_SOURCE = omfModelObjectProperties("hasSource")
  val OMF_HAS_SPECIFIC_CONCEPT = omfModelObjectProperties("hasSpecificConcept")
  val OMF_HAS_SPECIFIC_ENTITY = omfModelObjectProperties("hasSpecificEntity")
  val OMF_HAS_SPECIFIC_RELATIONSHIP = omfModelObjectProperties("hasSpecificRelationship")
  val OMF_HAS_TARGET = omfModelObjectProperties("hasTarget")
  val OMF_IMPORTS = omfModelObjectProperties("imports")
  val OMF_IS_GRAPH_OF_ENTITY = omfModelObjectProperties("isGraphOfEntity")
  val OMF_RESTRICTS_CONCEPT = omfModelObjectProperties("restrictsConcept")
  val OMF_RESTRICTS_RELATIONSHIP = omfModelObjectProperties("restrictsRelationship")
  
  // Data Properties
  val OMF_HAS_IRI = omfModelDataProperties("hasIRI")
  val OMF_HAS_NAME = omfModelDataProperties("hasName")
  val OMF_HAS_QUALIFIED_NAME = omfModelDataProperties("hasQualifiedName")
  val OMF_HAS_UUID = omfModelDataProperties("hasUUID")
  val OMF_IS_ABSTRACT = omfModelDataProperties("isAbstract")
  val OMF_IS_ASYMMETRIC = omfModelDataProperties("isAsymmetric")
  val OMF_IS_FUNCTIONAL = omfModelDataProperties("isFunctional")
  val OMF_IS_INVERSE_FUNCTIONAL = omfModelDataProperties("isInverseFunctional")
  val OMF_IS_IRREFLEXIVE = omfModelDataProperties("isIrreflexive")
  val OMF_IS_REFLEXIVE = omfModelDataProperties("isReflexive")
  val OMF_IS_SYMMETRIC = omfModelDataProperties("isSymmetric")
  val OMF_IS_TRANSITIVE = omfModelDataProperties("isTransitive")    
    
  
  val immutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, types.ImmutableModelTerminologyGraph]()
  val mutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, types.MutableModelTerminologyGraph]()

  // OMF Ontology Instance Model Constructors  
  
  val owlDataFactory = ontManager.getOWLDataFactory
  
  def createOMFModelTerminologyGraphExtension( 
      o: OWLOntology, 
      extendingG: types.MutableModelTerminologyGraph, 
      extendedG: types.ModelTerminologyGraph): Unit = {
    val extendingI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance( extendingG )
    val extendedI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance( extendedG ) 
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_IMPORTS, extendingI, extendedI)))
  }
  
  def createOMFModelTerminologyGraph( o: OWLOntology, graphT: types.ModelTerminologyGraph, iri: IRI, hasName: String, hasQualifiedName: String, hasUUID: String ): Unit = {    
    val graphI = owlDataFactory.getOWLNamedIndividual(iri)
    OMF_MODEL_TERMINOLOGY_GRAPH2Instance += ( graphT -> graphI )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( graphI )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_MODEL_TERMINOLOGY_GRAPH, graphI )))    
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, graphI, hasName )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, graphI, hasQualifiedName )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, graphI, hasUUID )))
  }
  
  def createOMFModelEntityAspectInstance( o: OWLOntology, aspectT: types.ModelEntityAspect, iri: IRI, hasName: String, hasQualifiedName: String, hasUUID: String ): Unit = {    
    val aspectI = owlDataFactory.getOWLNamedIndividual(iri)
    OMF_MODEL_ENTITY_ASPECT2Instance += ( aspectT -> aspectI )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( aspectI )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_MODEL_ENTITY_ASPECT, aspectI )))     
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, aspectI, hasName )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, aspectI, hasQualifiedName )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, aspectI, hasUUID )))
  }
  
  def createOMFModelEntityConceptInstance( o: OWLOntology, conceptT: types.ModelEntityConcept, iri: IRI, hasName: String, hasQualifiedName: String, hasUUID: String, isAbstract: Boolean ): Unit = {    
    val conceptI = owlDataFactory.getOWLNamedIndividual(iri)
    OMF_MODEL_ENTITY_CONCEPT2Instance += ( conceptT -> conceptI )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( conceptI )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_MODEL_ENTITY_CONCEPT, conceptI )))   
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, conceptI, hasName )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, conceptI, hasQualifiedName )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, conceptI, hasUUID )))
  }  
  
  def createOMFModelEntityRelationshipInstance( o: OWLOntology, relationshipT: types.ModelEntityRelationship, iri: IRI, hasName: String, hasQualifiedName: String, hasUUID: String, isAbstract: Boolean ): Unit = {    
    val relationshipI = owlDataFactory.getOWLNamedIndividual(iri)
    OMF_MODEL_ENTITY_RELATIONSHIP2Instance += ( relationshipT -> relationshipI )
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDeclarationAxiom( relationshipI )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLClassAssertionAxiom( OMF_MODEL_ENTITY_CONCEPT, relationshipI )))   
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, relationshipI, hasName )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, relationshipI, hasQualifiedName )))
    ontManager.applyChange( new AddAxiom( o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, relationshipI, hasUUID )))
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
            if (ontManager.contains(iri)) ontManager.getOntology(iri) 
            else ontManager.loadOntology( iri )
          System.out.println( s"loadTerminologyGraph: iri=${iri}, o=${o.getOWLOntologyManager.getOntologyDocumentIRI( o )}" )
          registerImmutableOntologyAsTerminologyGraph( o )
        }
        catch {
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