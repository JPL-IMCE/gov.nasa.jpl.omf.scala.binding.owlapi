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

import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader._
import gov.nasa.jpl.omf.scala.core.builtin.BuiltInDatatypeMaps
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.{Imports, _}
import org.semanticweb.owlapi.util.PriorityCollection

import scala.collection.immutable._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.compat.java8.StreamConverters._

import scala.util.control.Exception._
import scala.{Boolean, None, Option, Some, StringContext, Tuple2, Unit}
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._

case class OWLAPIOMFGraphStore(omfModule: OWLAPIOMFModule, ontManager: OWLOntologyManager) {

  require(null != omfModule)
  require(null != ontManager)

  val LOG: Boolean =
    "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.GraphStore")

  implicit val ops = omfModule.ops

  val catalogIRIMapper: CatalogIRIMapper = {
      val mappers: PriorityCollection[OWLOntologyIRIMapper] = ontManager.getIRIMappers
      val mapper = new CatalogIRIMapper(omfModule.catalogManager)
      mappers.add(Iterable[OWLOntologyIRIMapper](mapper).asJava)
      mapper
  }

  protected lazy val omfModelOntology = {
    val o = ontManager.loadOntology(omfModule.omfOntologyIRI)
    require(o != null, s"Could not find the OMF metadata ontology: ${omfModule.omfOntologyIRI}")
    o
  }

  protected lazy val omfModelClasses =
    omfModelOntology
    .classesInSignature(Imports.EXCLUDED).toScala[Set]
    .map { c => c.getIRI.getRemainder.get -> c }
    .toMap

  protected lazy val omfModelObjectPropertiesMap =
    omfModelOntology
    .objectPropertiesInSignature(Imports.EXCLUDED).toScala[Set]
    .map { op => op.getIRI.getRemainder.get -> op }
    .toMap

  protected def omfModelObjectProperties(opIRI: String): OWLObjectProperty =
    omfModelObjectPropertiesMap.get(opIRI) match {
      case Some(op) =>
        op
      case None     =>
        val keys = omfModelObjectPropertiesMap.keys.toList.sorted.mkString("\nkey: ", "\nkey: ", "\n")
        throw new java.lang.IllegalArgumentException(s"No OMF Metadata ontology object property with iri $opIRI" + keys)
    }

  protected lazy val omfModelDataTypes =
    omfModelOntology
    .datatypesInSignature(Imports.EXCLUDED).toScala[Set]
    .map { dt => dt.getIRI.getRemainder.get -> dt }
    .toMap

  protected lazy val omfModelDataProperties =
    omfModelOntology
    .dataPropertiesInSignature(Imports.EXCLUDED).toScala[Set]
    .map { dp => dp.getIRI.getRemainder.get -> dp }
    .toMap

  protected lazy val omfNamedIndividuals =
    omfModelOntology
    .individualsInSignature(Imports.EXCLUDED).toScala[Set]
    .map { dp => dp.getIRI.getRemainder.get -> dp }
    .toMap

  protected lazy val allAnnotationProperties =
    omfModelOntology
    .annotationPropertiesInSignature(Imports.INCLUDED).toScala[Set]
    .map { ap => ap.getIRI.getShortForm -> ap }
    .toMap

  lazy val RDFS_LABEL: OWLAnnotationProperty = ontManager.getOWLDataFactory.getRDFSLabel

  lazy val ANNOTATION_HAS_UUID: OWLAnnotationProperty =
    ontManager
    .getOWLDataFactory
    .getOWLAnnotationProperty(omfModule.ops.AnnotationHasUUID)

  lazy val ANNOTATION_HAS_ID: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasID)

  lazy val ANNOTATION_HAS_URL: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasURL)

  lazy val ANNOTATION_HAS_RELATIVE_IRI: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasRelativeIRI)

  def createAddOntologyHasRelativeIRIAnnotation
  (o: OWLOntology,
   relativeIRI: String)
  : AddOntologyAnnotation =
    new AddOntologyAnnotation(
      o,
      owlDataFactory
        .getOWLAnnotation( ANNOTATION_HAS_RELATIVE_IRI, owlDataFactory.getOWLLiteral( relativeIRI ) ) )

  lazy val ANNOTATION_HAS_IRI_HASH_PREFIX: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasIRIHashPrefix)

  def createAddOntologyHasIRIHashPrefixAnnotation
  (o: OWLOntology,
   iriHashPrefix: String)
  : AddOntologyAnnotation =
    new AddOntologyAnnotation(
      o,
      owlDataFactory
        .getOWLAnnotation( ANNOTATION_HAS_IRI_HASH_PREFIX, owlDataFactory.getOWLLiteral( iriHashPrefix ) ) )

  lazy val ANNOTATION_HAS_IRI_HASH_SUFFIX: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasIRIHashSuffix)

  def createAddOntologyHasIRIHashSuffixAnnotation
  (o: OWLOntology,
   iriHashSuffix: String)
  : AddOntologyAnnotation =
    new AddOntologyAnnotation(
      o,
      owlDataFactory
        .getOWLAnnotation( ANNOTATION_HAS_IRI_HASH_SUFFIX, owlDataFactory.getOWLLiteral( iriHashSuffix ) ) )

  lazy val ANNOTATION_HAS_CONTEXT: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasContext)

  lazy val ANNOTATION_HAS_GRAPH: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasGraph)

  lazy val ANNOTATION_HAS_RESTRICTED_SOURCE_PROPERTY: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasRestrictedSourceProperty)

  lazy val ANNOTATION_HAS_RESTRICTED_TARGET_PROPERTY: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasRestrictedTargetProperty)

  // OMF Metadata.

  protected var omfMetadata: Option[OWLOntology] = None

  def setOMFMetadataOntology(o: OWLOntology): Unit = {
    omfMetadata = Some(o)
    loadBuiltinDatatypeMap()
    ()
  }

  // OMF model.

  // ModelTermAxiom
  lazy val OMF_ENTITY_DEFINITION_EXISTENTIAL_RESTRICTION_AXIOM =
    omfModelClasses("EntityDefinitionExistentialRestrictionAxiom")
  protected val OMF_ENTITY_DEFINITION_EXISTENTIAL_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityDefinitionExistentialRestrictionAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_DEFINITION_UNIVERSAL_RESTRICTION_AXIOM =
    omfModelClasses("EntityDefinitionUniversalRestrictionAxiom")
  protected val OMF_ENTITY_DEFINITION_UNIVERSAL_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityDefinitionUniversalRestrictionAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM =
    omfModelClasses("EntityConceptSubClassAxiom")
  protected val OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityConceptSubClassAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_CONCEPT_DESIGNATION_TERMINOLOGY_GRAPH_AXIOM =
    omfModelClasses("EntityConceptDesignationTerminologyGraphAxiom")
  protected val OMF_ENTITY_CONCEPT_DESIGNATION_TERMINOLOGY_GRAPH_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityConceptDesignationTerminologyGraphAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM =
    omfModelClasses("EntityDefinitionAspectSubClassAxiom")
  protected val OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityDefinitionAspectSubClassAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM =
    omfModelClasses("EntityReifiedRelationshipSubClassAxiom")
  protected val OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityReifiedRelationshipSubClassAxiom, OWLNamedIndividual]()

  lazy val OMF_SCALAR_DATA_TYPE_FACET_RESTRICTION_AXIOM =
    omfModelClasses("ScalarDataTypeFacetRestrictionAxiom")
  protected val OMF_SCALAR_DATA_TYPE_FACET_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.ScalarDataTypeFacetRestrictionAxiom, OWLNamedIndividual]()

  // TerminologyGraphAxiom
  lazy val OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM =
    omfModelClasses("TerminologyGraphDirectExtensionAxiom")
  protected val OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.TerminologyGraphDirectExtensionAxiom, OWLNamedIndividual]()

  lazy val OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM =
    omfModelClasses("TerminologyGraphDirectNestingAxiom")
  protected val OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.TerminologyGraphDirectNestingAxiom, OWLNamedIndividual]()

  lazy val OMF_TERMINOLOGY_KIND_DATATYPE =
    omfModelDataTypes("TerminologyKind")
  //lazy val OMF_TERMINOLOGY_KIND_DESIGNATION_VALUE =

  lazy val OMF_MODEL_TERMINOLOGY_GRAPH =
    omfModelClasses("ModelTerminologyGraph")
  protected val OMF_MODEL_TERMINOLOGY_GRAPH2Instance =
    scala.collection.mutable.HashMap[types.ModelTerminologyGraph, OWLNamedIndividual]()

  // ModelTypeTerm  
  // ModelDataRelationship
  lazy val OMF_HAS_MODEL_DATA_RELATIONSHIP_FROM_ENTITY =
    omfModelObjectProperties("hasModelDataRelationshipFromEntity")
  lazy val OMF_HAS_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE =
    omfModelObjectProperties("hasModelDataRelationshipFromStructure")
  lazy val OMF_HAS_MODEL_DATA_RELATIONSHIP_TO_SCALAR =
    omfModelObjectProperties("hasModelDataRelationshipToScalar")
  lazy val OMF_HAS_MODEL_DATA_RELATIONSHIP_TO_STRUCTURE =
    omfModelObjectProperties("hasModelDataRelationshipToStructure")

  lazy val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR =
    omfModelClasses("ModelDataRelationshipFromEntityToScalar")
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR2Instance =
    scala.collection.mutable.HashMap[types.ModelDataRelationshipFromEntityToScalar, OWLNamedIndividual]()

  lazy val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_STRUCTURE =
    omfModelClasses("ModelDataRelationshipFromEntityToStructure")
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_STRUCTURE2Instance =
    scala.collection.mutable.HashMap[types.ModelDataRelationshipFromEntityToStructure, OWLNamedIndividual]()

  lazy val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_SCALAR =
    omfModelClasses("ModelDataRelationshipFromStructureToScalar")
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_SCALAR2Instance =
    scala.collection.mutable.HashMap[types.ModelDataRelationshipFromStructureToScalar, OWLNamedIndividual]()

  lazy val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_STRUCTURE =
    omfModelClasses("ModelDataRelationshipFromStructureToStructure")
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_STRUCTURE2Instance =
    scala.collection.mutable.HashMap[types.ModelDataRelationshipFromStructureToStructure, OWLNamedIndividual]()

  // ModelDataTypeDefinition
  protected val OMF_MODEL_DATA_TYPE_DEFINITION2Instance =
    scala.collection.mutable.HashMap[types.ModelDataTypeDefinition, OWLNamedIndividual]()

  lazy val OMF_MODEL_SCALAR_DATA_TYPE = omfModelClasses("ModelScalarDataType")
  protected val OMF_MODEL_SCALAR_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[types.ModelScalarDataType, OWLNamedIndividual]()

  lazy val OMF_MODEL_STRUCTURED_DATA_TYPE = omfModelClasses("ModelStructuredDataType")
  protected val OMF_MODEL_STRUCTURED_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[types.ModelStructuredDataType, OWLNamedIndividual]()

  protected val OMF_MODEL_TYPE_TERM2Instance =
    scala.collection.mutable.HashMap[types.ModelTypeTerm, OWLNamedIndividual]()

  protected val OMF_MODEL_ENTITY_DEFINITION2Instance =
    scala.collection.mutable.HashMap[types.ModelEntityDefinition, OWLNamedIndividual]()

  lazy val OMF_MODEL_ENTITY_ASPECT = omfModelClasses("ModelEntityAspect")
  protected val OMF_MODEL_ENTITY_ASPECT2Instance =
    scala.collection.mutable.HashMap[types.ModelEntityAspect, OWLNamedIndividual]()

  lazy val OMF_MODEL_ENTITY_CONCEPT = omfModelClasses("ModelEntityConcept")
  protected val OMF_MODEL_ENTITY_CONCEPT2Instance =
    scala.collection.mutable.HashMap[types.ModelEntityConcept, OWLNamedIndividual]()

  lazy val OMF_MODEL_ENTITY_RELATIONSHIP = omfModelClasses("ModelEntityReifiedRelationship")
  protected val OMF_MODEL_ENTITY_RELATIONSHIP2Instance =
    scala.collection.mutable.HashMap[types.ModelEntityReifiedRelationship, OWLNamedIndividual]()

  // Object Properties
  lazy val OMF_DIRECTLY_ASSERTS_AXIOM = omfModelObjectProperties("directlyAssertsAxiom")
  lazy val OMF_DIRECTLY_DEFINES_TYPE_TERM = omfModelObjectProperties("directlyDefinesTypeTerm")
  lazy val OMF_DIRECTLY_NESTS = omfModelObjectProperties("directlyNests")
  lazy val OMF_DIRECTLY_IMPORTS = omfModelObjectProperties("directlyImports")

  lazy val OMF_HAS_GENERAL_ASPECT = omfModelObjectProperties("hasGeneralAspect")
  lazy val OMF_HAS_SPECIFIC_ENTITY = omfModelObjectProperties("hasSpecificEntity")

  lazy val OMF_HAS_GENERAL_CONCEPT = omfModelObjectProperties("hasGeneralConcept")
  lazy val OMF_HAS_SPECIFIC_CONCEPT = omfModelObjectProperties("hasSpecificConcept")

  lazy val OMF_HAS_GENERAL_REIFIED_RELATIONSHIP = omfModelObjectProperties("hasGeneralReifiedRelationship")
  lazy val OMF_HAS_SPECIFIC_REIFIED_RELATIONSHIP = omfModelObjectProperties("hasSpecificReifiedRelationship")

  lazy val OMF_HAS_RESTRICTED_RANGE = omfModelObjectProperties("hasRestrictedRange")
  lazy val OMF_RESTRICTS_RELATIONSHIP = omfModelObjectProperties("restrictsReifiedRelationship")

  lazy val OMF_HAS_SOURCE = omfModelObjectProperties("hasSource")
  lazy val OMF_HAS_TARGET = omfModelObjectProperties("hasTarget")

  lazy val OMF_HAS_TERMINOLOGY_KIND = omfModelObjectProperties("hasTerminologyKind")

  lazy val OMF_HAS_DIRECT_EXTENDED_PARENT = omfModelObjectProperties("hasDirectExtendedParent")
  lazy val OMF_HAS_DIRECT_EXTENSIONING_CHILD = omfModelObjectProperties("hasDirectExtendingChild")

  lazy val OMF_HAS_DIRECT_NESTING_PARENT = omfModelObjectProperties("hasDirectNestingParent")
  lazy val OMF_HAS_DIRECT_NESTING_CONTEXT = omfModelObjectProperties("hasDirectNestingContext")
  lazy val OMF_HAS_DIRECT_NESTED_CHILD = omfModelObjectProperties("hasDirectNestedChild")

  lazy val OMF_HAS_DESIGNATION_TERMINOLOGY_GRAPH = omfModelObjectProperties("hasDesignationTerminologyGraph")
  lazy val OMF_HAS_ENTITY_CONCEPT_DESIGNATION = omfModelObjectProperties("hasEntityConceptDesignation")

  lazy val OMF_HAS_RESTRICTED_SCALAR_DATATYPE = omfModelObjectProperties("hasRestrictedScalarDataType")
  lazy val OMF_HAS_RESTRICTING_SCALAR_DATATYPE = omfModelObjectProperties("hasRestrictingScalarDataType")

  // Named Individuals
  lazy val OMF_TOPLEVEL_DEFINITION_TBOX = omfNamedIndividuals("ToplevelDefinitionTBox")
  lazy val OMF_DEFINITION_TBOX = omfNamedIndividuals("DefinitionTBox")
  lazy val OMF_TOPLEVEL_DESIGNATION_TBOX = omfNamedIndividuals("ToplevelDesignationTBox")
  lazy val OMF_DESIGNATION_TBOX = omfNamedIndividuals("DesignationTBox")

  // Data Properties
  lazy val OMF_HAS_IRI = omfModelDataProperties("hasIRI")

  // tbox:ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral

  lazy val OMF_MODEL_SCALAR_DATA_RELATIONSHIP_RESTRICTION_AXIOM_FROM_ENTITY_TO_LITERAL =
    omfModelClasses("ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral")
  protected val OMF_MODEL_SCALAR_DATA_RELATIONSHIP_RESTRICTION_AXIOM_FROM_ENTITY_TO_LITERAL2Instance =
    scala.collection.mutable.HashMap[types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral, OWLNamedIndividual]()

  lazy val OMF_HAS_RESTRICTED_ENTITY_DOMAIN = omfModelObjectProperties("hasRestrictedEntityDomain")
  lazy val OMF_HAS_RESTRICTING_SCALAR_DATA_RELATIONSHIP = omfModelObjectProperties("hasRestrictingScalarDataRelationship")
  lazy val OMF_HAS_LITERAL_RESTRICTION = omfModelDataProperties("hasLiteralRestriction")

  lazy val OMF_HAS_RESTRICTED_REIFIED_RELATIONSHIP = omfModelObjectProperties("hasRestrictedReifiedRelationship")

  lazy val OMF_ENTITY_REIFIED_RELATIONSHIP_EXISTENTIAL_RESTRICTION_AXIOM = omfModelClasses("EntityReifiedRelationshipExistentialRestrictionAxiom")
  protected val OMF_ENTITY_REIFIED_RELATIONSHIP_EXISTENTIAL_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityReifiedRelationshipExistentialRestrictionAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_REIFIED_RELATIONSHIP_UNIVERSAL_RESTRICTION_AXIOM = omfModelClasses("EntityReifiedRelationshipUniversalRestrictionAxiom")
  protected val OMF_ENTITY_REIFIED_RELATIONSHIP_UNIVERSAL_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityReifiedRelationshipUniversalRestrictionAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_REIFIED_RELATIONSHIP_CONTEXTUALIZATION_AXIOM = omfModelClasses("EntityReifiedRelationshipContextualizationAxiom")
  protected val OMF_ENTITY_REIFIED_RELATIONSHIP_CONTEXTUALIZATION_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityReifiedRelationshipContextualizationAxiom, OWLNamedIndividual]()

  // Datatypes
  lazy val OWL_REAL: OWLDatatype = ontManager.getOWLDataFactory.getDoubleOWLDatatype

  /**
    * The ontology IRI relative path.
    * If unspecified, it is computed by removing the IMCE catalog URI prefix: http://imce.jpl.nasa.gov
    *
    * Note that in the OMF metadata, there are two variants of an ontology: Mutable, Immutable.
    * These are distinguished with a suffix _Grw, _Gro; however, the suffix is not included in the relative IRI path.
    */
  lazy val OMF_HAS_RELATIVE_IRI_PATH = omfModelDataProperties( "hasRelativeIRIPath" )

  /**
    * If specified, the prefix splits the relative IRI path in two:
    * - hashPrefix (not hashed), the value of this property, and
    * - hashSuffix (hashed with SHA-256)
    */
  lazy val OMF_HAS_RELATIVE_IRI_HASH_PREFIX = omfModelDataProperties( "hasRelativeIRIHashPrefix" )

  /**
    * When there is both IRI relative path and IRI hash prefix,
    * this property has the SHA-256 hash of the IRI relative path stripped of the IRI hash prefix.
    */
  lazy val OMF_HAS_RELATIVE_IRI_HASH_SUFFIX = omfModelDataProperties( "hasRelativeIRIHashSuffix" )

  /**
    * If specified, the filename of the ontology relative to the IMCE catalog without the .owl extension
    */
  lazy val OMF_HAS_RELATIVE_FILENAME = omfModelDataProperties( "hasRelativeFilename" )

  lazy val OMF_HAS_SHORT_NAME = omfModelDataProperties("hasShortName")
  lazy val OMF_HAS_UUID = omfModelDataProperties("hasUUID")
  lazy val OMF_HAS_ID = omfModelDataProperties("hasOTIToolSpecificID")
  lazy val OMF_HAS_URL = omfModelDataProperties("hasOTIToolSpecificURL")
  lazy val OMF_IS_ABSTRACT = omfModelDataProperties("isAbstract")
  lazy val OMF_IS_ASYMMETRIC = omfModelDataProperties("isAsymmetric")
  lazy val OMF_IS_FUNCTIONAL = omfModelDataProperties("isFunctional")
  lazy val OMF_IS_INVERSE_FUNCTIONAL = omfModelDataProperties("isInverseFunctional")
  lazy val OMF_IS_IRREFLEXIVE = omfModelDataProperties("isIrreflexive")
  lazy val OMF_IS_REFLEXIVE = omfModelDataProperties("isReflexive")
  lazy val OMF_IS_SYMMETRIC = omfModelDataProperties("isSymmetric")
  lazy val OMF_IS_TRANSITIVE = omfModelDataProperties("isTransitive")

  lazy val OMF_MODEL_TERMINOLOGY_GRAPH_KIND =
    omfModelDataProperties("kind")

  lazy val OMF_MODEL_TERMINOLOGY_GRAPH_EXPORTED_OTI_PACKAGE_URI_PROVENANCE =
    omfModelDataProperties("exportedOTIPackageURIProvenance")

  lazy val OMF_MODEL_TERMINOLOGY_GRAPH_EXPORTED_OTI_PACKAGE_KIND_PROVENANCE =
    omfModelDataProperties("exportedOTIPackageKindProvenance")

  def createOntologyChangesForOMFModelTerminologyGraphProvenanceMetadata
  ( omfGraph: types.ModelTerminologyGraph,
    graphI: OWLNamedIndividual )
  : Seq[OWLOntologyChange] =
    new AddAxiom(omfMetadata.get,
      owlDataFactory
        .getOWLDataPropertyAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH_KIND,
          graphI, omfGraph.mutabilityKind)) +:
      omfGraph.extraProvenanceMetadata.fold[Seq[OWLOntologyChange]](Seq()) { info =>
      Seq(
        new AddAxiom(omfMetadata.get,
          owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH_EXPORTED_OTI_PACKAGE_KIND_PROVENANCE,
              graphI, info.provenanceKind.literal)),
        new AddAxiom(omfMetadata.get,
          owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH_EXPORTED_OTI_PACKAGE_URI_PROVENANCE,
              graphI, info.provenanceURI))
      )
    }

  // @todo report errors if the graph isn't there...
  def getModelTerminologyGraphRelativeIRIPath
  (g: types.ModelTerminologyGraph)
  : Option[String] = {
    val i_g = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(g)
    val i_dataValues: Set[OWLDataPropertyAssertionAxiom] =
      omfMetadata.get.dataPropertyAssertionAxioms(i_g).toScala[Set]

    val i_g_relativePath_dataValue = i_dataValues.find { ax =>
      ax.getProperty match {
        case dp: OWLDataProperty =>
          dp == OMF_HAS_RELATIVE_IRI_PATH
        case _ =>
          false
      }
    }

    i_g_relativePath_dataValue.map(_.getObject.getLiteral)
  }

  // @todo report errors if the graph isn't there...
  def getModelTerminologyGraphIRIHashPrefix
  (g: types.ModelTerminologyGraph)
  : Option[String] = {
    val i_g = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(g)
    val i_dataValues: Set[OWLDataPropertyAssertionAxiom] =
      omfMetadata.get.dataPropertyAssertionAxioms(i_g).toScala[Set]

    val i_g_iriHashPrefix_dataValue = i_dataValues.find { ax =>
      ax.getProperty match {
        case dp: OWLDataProperty =>
          dp == OMF_HAS_RELATIVE_IRI_HASH_PREFIX
        case _ =>
          false
      }
    }

    i_g_iriHashPrefix_dataValue.map(_.getObject.getLiteral)
  }

  // @todo report errors if the graph isn't there...
  def getModelTerminologyGraphIRIHashSuffix
  (g: types.ModelTerminologyGraph)
  : Option[String] = {
    val i_g = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(g)
    val i_dataValues: Set[OWLDataPropertyAssertionAxiom] =
      omfMetadata.get.dataPropertyAssertionAxioms(i_g).toScala[Set]

    val i_g_iriHashSuffix_dataValue = i_dataValues.find { ax =>
      ax.getProperty match {
        case dp: OWLDataProperty =>
          dp == OMF_HAS_RELATIVE_IRI_HASH_SUFFIX
        case _ =>
          false
      }
    }

    i_g_iriHashSuffix_dataValue.map(_.getObject.getLiteral)
  }

  def calculateRelativeIRIUnhashedPrefixHashedSuffix
  (relativeIRIPath: String,
   relativeIRIHashPrefix: Option[String])
  : Option[(String,String)] =
    relativeIRIHashPrefix.flatMap { prefix =>
      val prefixSlash = if (prefix.endsWith("/")) prefix else prefix+'/'
      if (!relativeIRIPath.startsWith(prefixSlash))
        Option.empty[(String,String)]
      else
        Tuple2(prefixSlash, hashMessage(relativeIRIPath.stripPrefix(prefixSlash))).some
    }

  // @todo report errors if the graph isn't there...
  def getModelTerminologyGraphRelativeFilename
  (g: types.ModelTerminologyGraph)
  : Option[String] = {
    val i_g = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(g)
    val i_dataValues: Set[OWLDataPropertyAssertionAxiom] =
      omfMetadata.get.dataPropertyAssertionAxioms(i_g).toScala[Set]

    val i_g_relativeFilename_dataValue = i_dataValues.find { ax =>
      ax.getProperty match {
        case dp: OWLDataProperty =>
          dp == OMF_HAS_RELATIVE_FILENAME
        case _ =>
          false
      }
    }

    i_g_relativeFilename_dataValue.map(_.getObject.getLiteral)
  }

  protected val immutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, types.ImmutableModelTerminologyGraph]()
  protected val mutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, types.MutableModelTerminologyGraph]()

  type ImmutableModelTerminologyGraphConversionMap =
  (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap)

  private var builtInDatatypeMap
  : Option[ImmutableModelTerminologyGraphConversionMap]
  = None

  def makeW3CTerminologyGraphDefinition
  (iri: IRI)
  : Set[java.lang.Throwable] \/ types.MutableModelTerminologyGraph
  = ops.makeTerminologyGraphWithPath(
    iri,
    relativeIRIPath=Option.empty[String],
    relativeIRIHashPrefix=Option.empty[String],
    isDefinition,
    extraProvenanceMetadata=
      OTI2OMFModelTerminologyGraphProvenance(
        provenanceKind=OMFModelTerminologyGraphW3CProvenanceKind,
        provenanceURI=iri.toString))(this)

  def loadBuiltinDatatypeMap
  ()
  : Set[java.lang.Throwable] \/ ImmutableModelTerminologyGraphConversionMap
  = builtInDatatypeMap
    .fold[Set[java.lang.Throwable] \/ ImmutableModelTerminologyGraphConversionMap]{
    BuiltInDatatypeMaps
      .createBuiltInDatatypeMaps[OWLAPIOMF](makeW3CTerminologyGraphDefinition)(ops, this)
      .map { builtInMap =>
        builtInDatatypeMap = Some(builtInMap)
        builtInMap._2.values foreach { builtInG =>
          immutableTBoxGraphs += (builtInG.iri -> builtInG)
        }
        builtInMap
      }
  }{ builtInMap =>
    \/-(builtInMap)
  }

  def getBuiltinDatatypeMapTerminologyGraph
  : types.ImmutableModelTerminologyGraph
  = {
    val result = loadBuiltinDatatypeMap()
    require(result.isRight)
    result.toOption.get._1
  }

  def lookupTerminologyGraph
  (iri: IRI)
  : Option[types.ModelTerminologyGraph]
  = {
    val result
    : Option[types.ModelTerminologyGraph]
    = immutableTBoxGraphs.get(iri)
      .orElse(mutableTBoxGraphs.get(iri))

    result
  }

  protected val directExtensionAxioms = scala.collection.mutable.HashSet[types.TerminologyGraphDirectExtensionAxiom]()

  /**
    * TerminologyGraphDirectNestingAxiom(nestingParent=G1, nestingContext=C, nestedChild=G2)
    *
    * key = nestedChild
    * value = (nestingParent, axiom)
    */
  protected val directNestingAxioms =
    scala.collection.mutable.HashMap[
      types.ModelTerminologyGraph,
      (types.ModelTerminologyGraph, types.TerminologyGraphDirectNestingAxiom)]()

  protected val directNestedAxioms = scala.collection.mutable.HashMap[
      types.ModelTerminologyGraph,
      scala.collection.mutable.HashSet[types.TerminologyGraphDirectNestingAxiom]]()
      .withDefaultValue(scala.collection.mutable.HashSet[types.TerminologyGraphDirectNestingAxiom]())

  protected val extendingChild2ExtendedParents =
    scala.collection.mutable.HashMap[
      types.ModelTerminologyGraph,
      scala.collection.mutable.HashSet[types.ModelTerminologyGraph]]()
      .withDefaultValue(scala.collection.mutable.HashSet[types.ModelTerminologyGraph]())

  def applyModelTermAxiomChanges[AX <: types.ModelTermAxiom]
  (ax: AX,
   title: String,
   changes: Seq[OWLAxiomChange])
  : Set[java.lang.Throwable] \/ Unit
  = changes.foldLeft[Set[java.lang.Throwable] \/ Unit](\/-(())) { case (acc, change) =>

    acc.flatMap { _ =>
      val result = ontManager.applyChange(change)
      if (result == ChangeApplied.UNSUCCESSFULLY)
        -\/(Set(OMFError.omfError(s"$title\naxiom=$ax\nfailed change=$change")))
      else
        \/-(())
    }
  }

  /**
    * Find the axiom TerminologyGraphDirectNestingAxiom(nestedChild==nestedG), if any.
    */
  def lookupNestingAxiomForNestedChildIfAny
  (nestedG: types.ModelTerminologyGraph)
  : Option[types.TerminologyGraphDirectNestingAxiom]
  = directNestingAxioms.get(nestedG).map(_._2)

  /**
    * Find the axioms TerminologyGraphDirectNestingAxiom(nestingContext=nestingC)
    */
  def lookupNestingAxiomsForNestingContext
  (nestingC: types.ModelEntityConcept)
  : Set[types.TerminologyGraphDirectNestingAxiom]
  = directNestingAxioms.values.map(_._2).filter(_.nestingContext == nestingC).to[Set]

  /**
    * Find the axioms TerminologyGraphDirectNestingAxiom(nestingParent=nestingG)
    */
  def lookupNestingAxiomsForNestingParent
  (nestingG: types.ModelTerminologyGraph)
  : Set[types.TerminologyGraphDirectNestingAxiom]
  = directNestedAxioms
    .getOrDefault(nestingG, scala.collection.mutable.HashSet[types.TerminologyGraphDirectNestingAxiom]())
    .to[Set]

  def getNestingParentGraphOfAxiom
  (axiom: types.TerminologyGraphDirectNestingAxiom)
  : types.ModelTerminologyGraph
  = {
    val nestingParent = directNestingAxioms.find(_._2._2 == axiom).map(_._2._1)
    require(nestingParent.isDefined)
    nestingParent.get
  }

  def getNestingContextConceptOfAxiom
  (axiom: types.TerminologyGraphDirectNestingAxiom)
  : types.ModelEntityConcept
  = axiom.nestingContext

  def getNestedChildGraphOfAxiom
  (axiom: types.TerminologyGraphDirectNestingAxiom)
  : types.ModelTerminologyGraph
  = axiom.nestedChild

  def createTerminologyGraphDirectNestingAxiom
  (parentG: types.ModelTerminologyGraph,
   parentC: types.ModelEntityConcept,
   childG: types.ModelTerminologyGraph)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectNestingAxiom = {

    val axiom =
      lookupNestingAxiomsForNestingParent(parentG)
      .find(ax => ax.nestingContext == parentC && ax.nestedChild == childG)

    val pGAxioms =
      lookupNestingAxiomsForNestingParent(parentG)
      .filter(ax => ax.nestingContext==parentC && ax.nestedChild==childG)

    val pCAxiom =
      lookupNestingAxiomsForNestingContext(parentC)
      .filter(ax => ax.nestingParent == parentG && ax.nestedChild == childG)

    val cGAxiom =
      lookupNestingAxiomForNestedChildIfAny(childG)
      .filter(ax => ax.nestingParent == parentG && ax.nestedChild == childG)

    axiom
      .fold[Set[java.lang.Throwable] \/ types.TerminologyGraphDirectNestingAxiom] {

      if (pGAxioms.nonEmpty || pCAxiom.nonEmpty || cGAxiom.nonEmpty)

        Set(
          OMFError
            .omfOpsError(ops, s"createTerminologyGraphDirectNestingAxiom inconsistency")
        ).left

      else {

        val axiom = types.TerminologyGraphDirectNestingAxiom(
          nestingParent = parentG,
          nestingContext = parentC,
          nestedChild = childG)
        val parentIG = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(parentG)
        val parentIC = OMF_MODEL_ENTITY_CONCEPT2Instance(parentC)
        val nestedIG = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(childG)
        for {
          directNestingIRI <-
          makeMetadataInstanceIRI(omfMetadata.get, "DN", parentG.kindIRI, parentC.iri, childG.kindIRI)
          directNestingI =
          owlDataFactory.getOWLNamedIndividual(directNestingIRI)
        } yield {

          for {
            change <- Seq(
              new AddAxiom(omfMetadata.get,
                owlDataFactory.getOWLDeclarationAxiom(directNestingI)),
              new AddAxiom(omfMetadata.get,
                owlDataFactory
                  .getOWLClassAssertionAxiom(OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM,
                    directNestingI)),
              new AddAxiom(omfMetadata.get,
                owlDataFactory
                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_DIRECT_NESTING_PARENT,
                    directNestingI, parentIG)),
              new AddAxiom(omfMetadata.get,
                owlDataFactory
                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_DIRECT_NESTING_CONTEXT,
                    directNestingI, parentIC)),
              new AddAxiom(omfMetadata.get,
                owlDataFactory
                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_DIRECT_NESTED_CHILD,
                    directNestingI, nestedIG)))
          } {
            val result = ontManager.applyChange(change)
            require(
              result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
              s"\ncreateTerminologyGraphDirectNestingAxiom (result=$result)"+
                s"\nnesting parent=${parentG.iri}"+
                s"\nnested child=${childG.iri}"+
                s"\n$change")
          }

          directNestingAxioms += (childG -> Tuple2(parentG, axiom))
          directNestedAxioms
            .getOrElseUpdate(
              parentG,
              scala.collection.mutable.HashSet[types.TerminologyGraphDirectNestingAxiom]()) += axiom
          OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM2Instance += (axiom -> directNestingI)
          axiom
        }
      }

    } { ax =>

      if (pGAxioms.contains(ax) && pCAxiom.contains(ax) && cGAxiom.contains(ax))

        \/-(ax)

      else

        Set(
          OMFError
            .omfOpsError(ops, s"createTerminologyGraphDirectNestingAxiom inconsistency")
        ).left

    }
  }

  def getDirectlyExtendingGraphsOfExtendedParentGraph
  (extendedParentG: types.ModelTerminologyGraph)
  : Iterable[types.TerminologyGraphDirectExtensionAxiom] = {
    val result
    : Iterable[types.TerminologyGraphDirectExtensionAxiom]
    = directExtensionAxioms.filter(_.extendedParent.kindIRI == extendedParentG.kindIRI).to[Iterable]

    result
  }

  def getDirectlyExtendedGraphsOfExtendingChildGraph
  (extendingChildG: types.ModelTerminologyGraph)
  : Iterable[types.TerminologyGraphDirectExtensionAxiom] = {
    val result
    : Iterable[types.TerminologyGraphDirectExtensionAxiom]
    = directExtensionAxioms.filter(_.extendingChild.kindIRI == extendingChildG.kindIRI).to[Iterable]

    result
  }

  def createTerminologyGraphDirectExtensionAxiom
  (extendingG: types.ModelTerminologyGraph,
   extendedG: types.ModelTerminologyGraph)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom = {
    val extendedParents =
      extendingChild2ExtendedParents
      .getOrElseUpdate(extendingG,
                       scala.collection.mutable.HashSet[types.ModelTerminologyGraph]())

    val result
    : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom
    = if (extendedParents.contains(extendedG)) {
      directExtensionAxioms.find { ax =>
        ax.extendingChild.kindIRI == extendingG.kindIRI &&
          ax.extendedParent.kindIRI == extendedG.kindIRI
      }
        .fold[Set[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom]({
        System.out.println(s"directExtensionAxioms: ${directExtensionAxioms.size}")
        directExtensionAxioms.foreach { ax =>
          System.out.println(s"=> extending: ${ax.extendingChild.kindIRI} extended: ${ax.extendedParent.kindIRI}")
        }
        System.out.println(s"extendingChild2ExtendedParents: ${extendingChild2ExtendedParents.size}")
        extendingChild2ExtendedParents.foreach { case (child, parents) =>
          System.out.println(s"=> child: ${child.kindIRI} parents: ${parents.size}")
          parents.foreach { parent =>
            System.out.println(s"==> parent: ${parent.kindIRI}")
          }
        }
        Set(
          OMFError
            .omfOpsError(ops, "Duplicate TerminologyGraphDirectExtensionAxiom not in directExtensionAxioms")
        ).left
      }) { ax =>
        \/-(ax)
      }
    } else {

      val axiom =
        types
        .TerminologyGraphDirectExtensionAxiom(extendingChild = extendingG, extendedParent = extendedG)

      for {
        added <- Seq(
          directExtensionAxioms.add(axiom),
          extendedParents.add(extendedG))
      } require(added)

      val extendingI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(axiom.extendingChild)
      val extendedI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(axiom.extendedParent)
      for {
        directImportingIRI <-
        makeMetadataInstanceIRI(omfMetadata.get, "DI", axiom.extendingChild.kindIRI, axiom.extendedParent.kindIRI)

        directImportingI = owlDataFactory.getOWLNamedIndividual(directImportingIRI)
      } yield {

        if (LOG) {
          System.out.println(
            s"""|## createTerminologyGraphDirectExtensionAxiom:
                |extending: ${axiom.extendingChild.kindIRI}
                |extended: ${axiom.extendedParent.kindIRI}
                |result: $directImportingI"""
              .stripMargin)
        }
        for {
          change <- Seq(
            new AddAxiom(omfMetadata.get,
              owlDataFactory
                .getOWLDeclarationAxiom(directImportingI)),
            new AddAxiom(omfMetadata.get,
              owlDataFactory
                .getOWLClassAssertionAxiom(
                  OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM,
                  directImportingI)),
            new AddAxiom(omfMetadata.get,
              owlDataFactory
                .getOWLObjectPropertyAssertionAxiom(
                  OMF_HAS_DIRECT_EXTENDED_PARENT,
                  directImportingI, extendedI)),
            new AddAxiom(omfMetadata.get,
              owlDataFactory
                .getOWLObjectPropertyAssertionAxiom(
                  OMF_HAS_DIRECT_EXTENSIONING_CHILD,
                  directImportingI, extendingI)))
        } {
          val result = ontManager.applyChange(change)
          require(
            result == ChangeApplied.SUCCESSFULLY,
            s"\ncreateTerminologyGraphDirectExtensionAxiom:\n$change")
        }
        OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM2Instance += (axiom -> directImportingI)
        axiom
      }
    }

    result
  }

  def getNestingGraph
  (g: types.ModelTerminologyGraph)
  : Option[types.ModelTerminologyGraph]
  = directNestingAxioms.get(g).map(_._1)

  def getNestedGraphs
  (g: types.ModelTerminologyGraph)
  : Iterable[types.ModelTerminologyGraph]
  = lookupNestingAxiomsForNestingParent(g).map(_.nestedChild)

  def fromTerminologyGraph
  (g: types.ModelTerminologyGraph)
  : OWLAPITerminologyGraphSignature = {
    val result
    : OWLAPITerminologyGraphSignature
    = g.fromTerminologyGraph(
      extendingChild2ExtendedParents(g).to[Iterable],
      lookupNestingAxiomForNestedChildIfAny(g)
        .map(ax => (ax.nestingContext, ax.nestingParent)))

    result
  }

  // OMF Ontology Instance Model Constructors

  val owlDataFactory = ontManager.getOWLDataFactory

  val mDigest = java.security.MessageDigest.getInstance("SHA-256")

  def hashMessage(message: String): String = {
    val result
    : String
    = mDigest.digest(message.getBytes("UTF-8")).map("%02x".format(_)).mkString

    result
  }

  def makeMetadataInstanceIRI
  (o: OWLOntology, instanceKind: String, iri: IRI*)
  : Set[java.lang.Throwable] \/ IRI = {
    require(iri.nonEmpty)

    val result
    : Set[java.lang.Throwable] \/ IRI
    = omfModule.ops.withFragment(
      o.getOntologyID.getOntologyIRI.get,
      instanceKind + "-" + hashMessage(iri.mkString("", ",", "")))

    result
  }

  def createOMFModelTerminologyGraph
  (o: OWLOntology,
   iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   tboxOnt: OWLOntology,
   kind: TerminologyKind.TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  : Set[java.lang.Throwable] \/ types.MutableModelTerminologyGraph = {

    val result
    : Set[java.lang.Throwable] \/ types.MutableModelTerminologyGraph
    = Backbone
      .createBackbone(tboxOnt, kind, ops)
      .flatMap { backbone =>
        val graphT = new types.MutableModelTerminologyGraph(
          kind = kind, ont = tboxOnt,
          extraProvenanceMetadata = extraProvenanceMetadata,
          backbone = backbone)

        val aRelativeIRIPath =
          relativeIRIPath.fold[String](
            iri.toString.stripPrefix("http://")
          ) {
            identity
          }
        System.out.println(s"\n*** createOMFModelTerminologyGraph\n=> iri=$iri\n=> rel=$aRelativeIRIPath")

        for {
          graphIRI <- makeMetadataInstanceIRI(o, "Grw", iri)
          graphI = owlDataFactory.getOWLNamedIndividual(graphIRI)
        } yield {
          val okind = kind match {
            case TerminologyKind.isToplevelDefinition =>
              OMF_TOPLEVEL_DEFINITION_TBOX
            case TerminologyKind.isDefinition =>
              OMF_DEFINITION_TBOX
            case TerminologyKind.isToplevelDesignation =>
              OMF_TOPLEVEL_DESIGNATION_TBOX
            case TerminologyKind.isDesignation =>
              OMF_DESIGNATION_TBOX
          }
          for {
            change <- Seq[OWLOntologyChange](
              new AddAxiom(o,
                owlDataFactory.getOWLDeclarationAxiom(graphI)),
              new AddAxiom(o,
                owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH, graphI)),
              new AddAxiom(o,
                owlDataFactory
                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_TERMINOLOGY_KIND, graphI,
                    okind)),
              new AddAxiom(o,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, graphI,
                    graphT.iri.toString)),
              new AddAxiom(o,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_PATH, graphI,
                    aRelativeIRIPath)),
              createAddOntologyHasRelativeIRIAnnotation(tboxOnt, aRelativeIRIPath)
            ) ++
              calculateRelativeIRIUnhashedPrefixHashedSuffix(aRelativeIRIPath, relativeIRIHashPrefix)
                .fold[Seq[OWLOntologyChange]](Seq(
                new AddAxiom(o,
                  owlDataFactory
                    .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_FILENAME, graphI,
                      aRelativeIRIPath + "_Grw"))
              )) { case (unhashedPrefix, hashedSuffix) =>
                Seq(
                  new AddAxiom(o,
                    owlDataFactory
                      .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_HASH_PREFIX, graphI,
                        unhashedPrefix)),
                  createAddOntologyHasIRIHashPrefixAnnotation(tboxOnt, unhashedPrefix),
                  new AddAxiom(o,
                    owlDataFactory
                      .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_HASH_SUFFIX, graphI,
                        hashedSuffix)),
                  createAddOntologyHasIRIHashSuffixAnnotation(tboxOnt, hashedSuffix),
                  new AddAxiom(o,
                    owlDataFactory
                      .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_FILENAME, graphI,
                        unhashedPrefix + hashedSuffix + "_Grw"))
                )
              } ++
              createOntologyChangesForOMFModelTerminologyGraphProvenanceMetadata(graphT, graphI)
          } {
            val result = ontManager.applyChange(change)
            require(
              result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
              s"\ncreateOMFModelTerminologyGraph:\n$change")
          }
          mutableTBoxGraphs.put(iri, graphT)
          OMF_MODEL_TERMINOLOGY_GRAPH2Instance += (graphT -> graphI)
          graphT
        }
      }

    result
  }

  def setTerminologyGraphShortName
  (tbox: types.ModelTerminologyGraph,
   label: String)
  : Set[java.lang.Throwable] \/ Unit = {

    val result
    : Set[java.lang.Throwable] \/ Unit
    = nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set(
            OMFError.omfException(
              s"setTerminologyGraphShortName failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        for {
          change <-
          Seq(new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_SHORT_NAME,
                OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                owlDataFactory.getOWLLiteral(label)))
          )
        } {
          val result = ontManager.applyChange(change)
          require(
            result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
            s"\nsetTerminologyGraphShortName:\n$change")
        }
        \/-(())
      })

    result
  }

  def setTerminologyGraphUUID
  (tbox: types.ModelTerminologyGraph,
   id: String)
  : Set[java.lang.Throwable] \/ Unit = {

    val result
    : Set[java.lang.Throwable] \/ Unit
    = nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set(
            OMFError.omfException(
              s"setTerminologyGraphUUID failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        for {
          change <-
          Seq(new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID,
                OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                owlDataFactory.getOWLLiteral(id)))
          )
        } {
          val result = ontManager.applyChange(change)
          require(
            result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
            s"\nsetTerminologyGraphUUID:\n$change")
        }
        \/-(())
      })

    result
  }

  def setTermShortName
  (tbox: types.MutableModelTerminologyGraph,
   termT: types.ModelTypeTerm,
   label: String)
  : Set[java.lang.Throwable] \/ Unit = {

    val result
    : Set[java.lang.Throwable] \/ Unit
    = OMF_MODEL_TYPE_TERM2Instance.get(termT)
      .fold[Set[java.lang.Throwable] \/ Unit] {
      Set(
        OMFError
          .omfError(s"setTermShortName: no definition for $termT to set label=$label")
      ).left
    } { termI =>
      nonFatalCatch[Unit]
        .withApply {
          (cause: java.lang.Throwable) =>
            Set(
              OMFError.omfException(
                s"setTermShortName failed: ${cause.getMessage}",
                cause)
            ).left
        }
        .apply({
          for {
            change <- Seq(
              new AddAxiom(omfMetadata.get,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_SHORT_NAME,
                    termI,
                    owlDataFactory.getOWLLiteral(label)))
            )
          } {
            val result = ontManager.applyChange(change)
            require(
              result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
              s"\nsetTermShortName:\n$change")
          }
          \/-(())
        })
    }

    result
  }

  def setTermUUID
  (tbox: types.MutableModelTerminologyGraph,
   termT: types.ModelTypeTerm,
   id: String)
  : Set[java.lang.Throwable] \/ Unit
  = OMF_MODEL_TYPE_TERM2Instance.get(termT)
    .fold[Set[java.lang.Throwable] \/ Unit]{
      Set(
        OMFError
          .omfError(s"setTermUUID: no definition for $termT to set UUID=$id")
      ).left
    } { termI =>
      nonFatalCatch[Unit]
        .withApply {
          (cause: java.lang.Throwable) =>
            Set(
              OMFError.omfException(
                s"setTermUUID failed: ${cause.getMessage}",
                cause)
            ).left
        }
        .apply({
          for {
            change <- Seq(
              new AddAxiom(omfMetadata.get,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID,
                    termI,
                    owlDataFactory.getOWLLiteral(id)))
            )
          } {
            val result = ontManager.applyChange(change)
            require(
              result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
              s"\nsetTermUUID:\n$change")
          }
          \/-(())
        })
    }

  def setTermID
  (tbox: types.MutableModelTerminologyGraph,
   termT: types.ModelTypeTerm,
   id: String)
  : Set[java.lang.Throwable] \/ Unit
  = OMF_MODEL_TYPE_TERM2Instance.get(termT)
    .fold[Set[java.lang.Throwable] \/ Unit]{
    Set(
      OMFError
        .omfError(s"setTermUUID: no definition for $termT to set ID=$id")
    ).left
  } { termI =>
    applyOntologyChangeOrNoOp(
      ontManager,
      new AddAxiom(
        omfMetadata.get,
        owlDataFactory
          .getOWLDataPropertyAssertionAxiom(OMF_HAS_ID,
            termI,
            owlDataFactory.getOWLLiteral(id))),
      ifError = {
        "Failed to set a tbox term 'id' data property axiom"
      })
  }

  def setTermURL
  (tbox: types.MutableModelTerminologyGraph,
   termT: types.ModelTypeTerm,
   url: String)
  : Set[java.lang.Throwable] \/ Unit
  = OMF_MODEL_TYPE_TERM2Instance.get(termT)
    .fold[Set[java.lang.Throwable] \/ Unit]{
    Set(
      OMFError
        .omfError(s"setTermURL: no definition for $termT to set URL=$url")
    ).left
  } { termI =>
    applyOntologyChangeOrNoOp(
      ontManager,
      new AddAxiom(
        omfMetadata.get,
        owlDataFactory
          .getOWLDataPropertyAssertionAxiom(OMF_HAS_URL,
            termI,
            owlDataFactory.getOWLLiteral(url))),
      ifError = {
        "Failed to set a tbox term 'url' data property axiom"
      })
  }

  def createOMFModelEntityAspectInstance
  (tbox: types.ModelTerminologyGraph,
   aspectT: types.ModelEntityAspect)
  : Set[java.lang.Throwable] \/ OWLNamedIndividual =
    OMF_MODEL_ENTITY_ASPECT2Instance.get(aspectT)
    .fold[Set[java.lang.Throwable] \/ OWLNamedIndividual] {
      for {
        aspectIRI <- makeMetadataInstanceIRI(omfMetadata.get, "A", aspectT.iri)
        aspectI = owlDataFactory.getOWLNamedIndividual(aspectIRI)
      } yield {
        for {
          change <- Seq(
            new AddAxiom(omfMetadata.get,
              owlDataFactory.getOWLDeclarationAxiom(aspectI)),
            new AddAxiom(omfMetadata.get,
              owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_ENTITY_ASPECT, aspectI)),
            new AddAxiom(omfMetadata.get,
              owlDataFactory
                .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, aspectI, aspectT.iri.toString))
          )
        } {
          val result = ontManager.applyChange(change)
          require(
            result == ChangeApplied.SUCCESSFULLY,
            s"\ncreateOMFModelEntityAspectInstance:\n$change")
        }
        OMF_MODEL_TYPE_TERM2Instance += (aspectT -> aspectI)
        OMF_MODEL_ENTITY_DEFINITION2Instance += (aspectT -> aspectI)
        OMF_MODEL_ENTITY_ASPECT2Instance += (aspectT -> aspectI)
        aspectI
      }
    }{ aspectI =>
        \/-(aspectI)
    }

  def registerOMFModelEntityAspectInstance
  (tbox: types.ModelTerminologyGraph,
   aspectT: types.ModelEntityAspect)
  : Set[java.lang.Throwable] \/ OWLNamedIndividual =
    for {
      aspectI <- createOMFModelEntityAspectInstance(tbox, aspectT)
    } yield {
      for {
        change <- Seq(
                       new AddAxiom(omfMetadata.get,
                                    owlDataFactory
                                    .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_DEFINES_TYPE_TERM,
                                                                        OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                                        aspectI))
                     )
      } {
        val result = ontManager.applyChange(change)
        require(
                 result == ChangeApplied.SUCCESSFULLY,
                 s"\nregisterOMFModelEntityAspectInstance:\n$change")
      }
      aspectI
    }

  def createOMFModelEntityConceptInstance
  (tbox: types.ModelTerminologyGraph,
   conceptT: types.ModelEntityConcept)
  : Set[java.lang.Throwable] \/ OWLNamedIndividual =
    OMF_MODEL_ENTITY_CONCEPT2Instance
    .get(conceptT)
    .fold[Set[java.lang.Throwable] \/ OWLNamedIndividual]{
      for {
        conceptIRI <- makeMetadataInstanceIRI(omfMetadata.get, "C", conceptT.iri)
        conceptI = owlDataFactory.getOWLNamedIndividual(conceptIRI)
      } yield {
        for {
          change <- Seq(
            new AddAxiom(omfMetadata.get,
              owlDataFactory
                .getOWLDeclarationAxiom(conceptI)),
            new AddAxiom(omfMetadata.get,
              owlDataFactory
                .getOWLClassAssertionAxiom(OMF_MODEL_ENTITY_CONCEPT, conceptI)),
            new AddAxiom(omfMetadata.get,
              owlDataFactory
                .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, conceptI, conceptT.iri.toString))
          )
        } {
          val result = ontManager.applyChange(change)
          require(
            result == ChangeApplied.SUCCESSFULLY,
            s"\ncreateOMFModelEntityConceptInstance:\n$change")
        }
        OMF_MODEL_TYPE_TERM2Instance += (conceptT -> conceptI)
        OMF_MODEL_ENTITY_DEFINITION2Instance += (conceptT -> conceptI)
        OMF_MODEL_ENTITY_CONCEPT2Instance += (conceptT -> conceptI)
        conceptI
      }
    } { conceptI =>
      \/-(conceptI)
    }

  def registerOMFModelEntityConceptInstance
  (tbox: types.ModelTerminologyGraph,
   conceptT: types.ModelEntityConcept)
  : Set[java.lang.Throwable] \/ OWLNamedIndividual =
    for {
      conceptI <- createOMFModelEntityConceptInstance(tbox, conceptT)
    } yield {
      for {
        change <- Seq(
                       new AddAxiom(omfMetadata.get,
                                    owlDataFactory
                                    .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_DEFINES_TYPE_TERM,
                                                                        OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                                        conceptI))
                     )
      } {
        val result = ontManager.applyChange(change)
        require(
                 result == ChangeApplied.SUCCESSFULLY,
                 s"\nregisterOMFModelEntityConceptInstance:\n$change")
      }
      conceptI
    }

  def createOMFModelEntityReifiedRelationshipInstance
  (tbox: types.ModelTerminologyGraph,
   relationshipT: types.ModelEntityReifiedRelationship)
  : Set[java.lang.Throwable] \/ OWLNamedIndividual =
    OMF_MODEL_ENTITY_RELATIONSHIP2Instance
    .get(relationshipT)
      .fold[Set[java.lang.Throwable] \/ OWLNamedIndividual]{
      for {
        relationshipIRI <- makeMetadataInstanceIRI(omfMetadata.get, "R", relationshipT.iri)
        relationshipI = owlDataFactory.getOWLNamedIndividual(relationshipIRI)
      } yield {
        val sourceI = OMF_MODEL_ENTITY_DEFINITION2Instance(relationshipT.source)
        val targetI = OMF_MODEL_ENTITY_DEFINITION2Instance(relationshipT.target)
        for {
          change <- Seq(
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory.getOWLDeclarationAxiom(relationshipI)),
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .getOWLClassAssertionAxiom(OMF_MODEL_ENTITY_RELATIONSHIP, relationshipI)),
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI,
                                                                        relationshipI,
                                                                        relationshipT.iri.toString)),
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .getOWLObjectPropertyAssertionAxiom(OMF_HAS_SOURCE, relationshipI, sourceI)),
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .getOWLObjectPropertyAssertionAxiom(OMF_HAS_TARGET, relationshipI, targetI))
                       )
        } {
          val result = ontManager.applyChange(change)
          require(
                   result == ChangeApplied.SUCCESSFULLY,
                   s"\ncreateOMFModelEntityReifiedRelationshipInstance:\n$change")
        }
        OMF_MODEL_TYPE_TERM2Instance += (relationshipT -> relationshipI)
        OMF_MODEL_ENTITY_DEFINITION2Instance += (relationshipT -> relationshipI)
        OMF_MODEL_ENTITY_RELATIONSHIP2Instance += (relationshipT -> relationshipI)
        relationshipI
      }
    }{ relationshipI =>
      \/-(relationshipI)
    }

  def registerOMFModelEntityReifiedRelationshipInstance
  (tbox: types.ModelTerminologyGraph,
   relationshipT: types.ModelEntityReifiedRelationship)
  : Set[java.lang.Throwable] \/ OWLNamedIndividual =
    for {
      relationshipI <- createOMFModelEntityReifiedRelationshipInstance(tbox, relationshipT)
    } yield {
      for {
        change <- Seq(
                       new AddAxiom(omfMetadata.get,
                                    owlDataFactory
                                    .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_DEFINES_TYPE_TERM,
                                                                        OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                                        relationshipI))
                     )
      } {
        val result = ontManager.applyChange(change)
        require(
                 result == ChangeApplied.SUCCESSFULLY,
                 s"\nregisterOMFModelEntityReifiedRelationshipInstance:\n$change")
      }
      relationshipI
    }

  def createOMFModelScalarDataTypeInstance
  (tbox: types.ModelTerminologyGraph,
   scalarDT: types.ModelScalarDataType)
  : Set[java.lang.Throwable] \/ OWLNamedIndividual =
    OMF_MODEL_SCALAR_DATA_TYPE2Instance
      .get(scalarDT)
      .fold[Set[java.lang.Throwable] \/ OWLNamedIndividual]{
      for {
        scalarDIRI <- makeMetadataInstanceIRI(omfMetadata.get, "SC", tbox.iri, scalarDT.iri)
        scalarDI = owlDataFactory.getOWLNamedIndividual(scalarDIRI)
      } yield {
        for {
          change <- Seq(
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .getOWLDeclarationAxiom(scalarDI)),
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .getOWLClassAssertionAxiom(OMF_MODEL_SCALAR_DATA_TYPE, scalarDI)),
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, scalarDI, scalarDT.iri.toString))
                       )
        } {
          val result = ontManager.applyChange(change)
          require(
                   result == ChangeApplied.SUCCESSFULLY,
                   s"\ncreateOMFModelScalarDataTypeInstance:\n$change")
        }
        OMF_MODEL_TYPE_TERM2Instance += (scalarDT -> scalarDI)
        OMF_MODEL_DATA_TYPE_DEFINITION2Instance += (scalarDT -> scalarDI)
        OMF_MODEL_SCALAR_DATA_TYPE2Instance += (scalarDT -> scalarDI)
        scalarDI
      }
    }{ scalarDI =>
      \/-(scalarDI)
    }

  def registerOMFModelScalarDataTypeInstance
  (tbox: types.ModelTerminologyGraph,
   scalarDT: types.ModelScalarDataType)
  : Set[java.lang.Throwable] \/ OWLNamedIndividual =
    for {
      scalarDI <- createOMFModelScalarDataTypeInstance(tbox, scalarDT)
    } yield {
      for {
        change <- Seq(
                       new AddAxiom(omfMetadata.get,
                                    owlDataFactory
                                    .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_DEFINES_TYPE_TERM,
                                                                        OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                                        scalarDI))
                     )
      } {
        val result = ontManager.applyChange(change)
        require(
                 result == ChangeApplied.SUCCESSFULLY,
                 s"\nregisterOMFModelScalarDataTypeInstance:\n$change")
      }
      scalarDI
    }

  def createOMFModelStructuredDataTypeInstance
  (tbox: types.ModelTerminologyGraph,
   structuredDT: types.ModelStructuredDataType)
  : Set[java.lang.Throwable] \/ OWLNamedIndividual =
    OMF_MODEL_STRUCTURED_DATA_TYPE2Instance
      .get(structuredDT)
      .fold[Set[java.lang.Throwable] \/ OWLNamedIndividual]{
      for {
        structuredDIRI <- makeMetadataInstanceIRI(omfMetadata.get, "ST", tbox.iri, structuredDT.iri)
        structuredDI = owlDataFactory.getOWLNamedIndividual(structuredDIRI)
      } yield {
        for {
          change <- Seq(
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory.getOWLDeclarationAxiom(structuredDI)),
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .getOWLClassAssertionAxiom(OMF_MODEL_STRUCTURED_DATA_TYPE, structuredDI)),
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, structuredDI, structuredDT.iri
                                                                                                   .toString))
                       )
        } {
          val result = ontManager.applyChange(change)
          require(
                   result == ChangeApplied.SUCCESSFULLY,
                   s"\ncreateOMFModelStructuredDataTypeInstance:\n$change")
        }
        OMF_MODEL_TYPE_TERM2Instance += (structuredDT -> structuredDI)
        OMF_MODEL_DATA_TYPE_DEFINITION2Instance += (structuredDT -> structuredDI)
        OMF_MODEL_STRUCTURED_DATA_TYPE2Instance += (structuredDT -> structuredDI)
        structuredDI
      }
    }{ structuredDI =>
      \/-(structuredDI)
    }

  def registerOMFModelStructuredDataTypeInstance
  (tbox: types.ModelTerminologyGraph,
   structuredDT: types.ModelStructuredDataType)
  : Set[java.lang.Throwable] \/ OWLNamedIndividual =
    for {
      structuredDI <- createOMFModelStructuredDataTypeInstance(tbox, structuredDT)
    } yield {
      for {
        change <- Seq(
                       new AddAxiom(omfMetadata.get,
                                    owlDataFactory
                                    .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_DEFINES_TYPE_TERM,
                                                                        OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                                        structuredDI))
                     )
      } {
        val result = ontManager.applyChange(change)
        require(
                 result == ChangeApplied.SUCCESSFULLY,
                 s"\nregisterOMFModelStructuredDataTypeInstance:\n$change")
      }
      structuredDI
    }

  /**
    * Create an OMF EntityDefinitionAspectSubClassAxiom.
    *
    * @param tbox OMF metadata graph where the EntityDefinitionAspectSubClassAxiomI will be added, if it isn't there
    * @param axiomT The OMF axiom to represent in the OMF metadata graph, tbox.
    * @return
    */
  def createOMFEntityDefinitionAspectSubClassAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityDefinitionAspectSubClassAxiom)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionAspectSubClassAxiom = {
    val subI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.sub)
    val supI = OMF_MODEL_ENTITY_ASPECT2Instance(axiomT.sup)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get, "DefinitionAspectSubClass", tbox.iri, axiomT.sub.iri, axiomT.sup.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
    } yield {
    for {
      change <- Seq(
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLDeclarationAxiom(axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
                                                                      OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                                      axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLClassAssertionAxiom(OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM, axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_GENERAL_ASPECT, axiomI, supI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_SPECIFIC_ENTITY, axiomI, subI)))
    //    ontManager.applyChange( new AddAxiom(
    //   o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //     OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } {
      val result = ontManager.applyChange(change)
      require(
               result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
               s"\ncreateOMFEntityDefinitionAspectSubClassAxiomInstance:\n$change")
    }
    OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  /**
    * Create an OMF EntityConceptSubClassAxiom.
    *
    * @param tbox OMF metadata graph where the EntityConceptSubClassAxiom will be added, if it isn't there
    * @param axiomT The OMF axiom to represent in the OMF metadata graph, tbox.
    * @return
    */
  def createOMFEntityConceptSubClassAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityConceptSubClassAxiom)
  : Set[java.lang.Throwable] \/ types.EntityConceptSubClassAxiom = {
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.sub)
    val supI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.sup)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
        "ConceptSubClass",
        tbox.iri,
        axiomT.sub.iri,
        axiomT.sup.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
    } yield {
    for {
      change <- Seq(
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLDeclarationAxiom(axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
                                                                      OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                                      axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLClassAssertionAxiom(OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM, axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_GENERAL_CONCEPT, axiomI, supI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_SPECIFIC_CONCEPT, axiomI, subI))
                     //    ontManager.applyChange( new AddAxiom(
                     //   o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
                     //    OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
                   )
    } {
      val result = ontManager.applyChange(change)
      require(
               result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
               s"\ncreateOMFEntityConceptSubClassAxiomInstance:\n$change")
    }
    OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  def createDataRelationshipFromEntityToScalar
  (tbox: types.ModelTerminologyGraph,
   e2sc: types.ModelDataRelationshipFromEntityToScalar)
  : Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar
  = {
    val entityI = OMF_MODEL_ENTITY_DEFINITION2Instance(e2sc.source)
    val scalarI = OMF_MODEL_SCALAR_DATA_TYPE2Instance(e2sc.target)
    for {
      termIRI <- makeMetadataInstanceIRI(
        omfMetadata.get,
        "EntityToScalar",
        tbox.iri,
        e2sc.source.iri,
        e2sc.target.iri)
      termI = owlDataFactory.getOWLNamedIndividual(termIRI)
    } yield {
      for {
        change <- Seq(
          new AddAxiom(omfMetadata.get,
            owlDataFactory
            .getOWLDeclarationAxiom(termI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
            .getOWLClassAssertionAxiom(OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR, termI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_MODEL_DATA_RELATIONSHIP_FROM_ENTITY, termI, entityI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_MODEL_DATA_RELATIONSHIP_TO_SCALAR, termI, scalarI))
        )
      } {
        val result = ontManager.applyChange(change)
        require(
          result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
          s"\ncreateDataRelationshipFromEntityToScalar:\n$change")
      }
      OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR2Instance += (e2sc -> termI)
      e2sc
    }
  }

  /**
    * Create an OMF EntityDefinitionUniversalRestrictionAxiom.
    *
    * @param tbox OMF metadata graph where the EntityDefinitionUniversalRestrictionAxiom will be added, if it isn't there
    * @param axiomT The OMF axiom to represent in the OMF metadata graph, tbox.
    * @return
    */
  def createOMFEntityDefinitionUniversalRestrictionAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityDefinitionUniversalRestrictionAxiom)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionUniversalRestrictionAxiom = {
    val subI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.sub)
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.rel)
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.range)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
        "UniversalDefinitionRestriction",
        tbox.iri,
        axiomT.sub.iri,
        axiomT.rel.iri,
        axiomT.range.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
    } yield {
    for {
      change <- Seq(
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLDeclarationAxiom(axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
                                                                      OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                                      axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLClassAssertionAxiom(OMF_ENTITY_DEFINITION_UNIVERSAL_RESTRICTION_AXIOM, axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_ENTITY_DOMAIN, axiomI, subI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_RESTRICTS_RELATIONSHIP, axiomI, relI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_RANGE, axiomI, rangeI)))
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //    OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } {
      val result = ontManager.applyChange(change)
      require(
               result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
               s"\ncreateOMFEntityDefinitionUniversalRestrictionAxiomInstance:\n$change")
    }
    OMF_ENTITY_DEFINITION_UNIVERSAL_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  /**
    * Create an OMF EntityDefinitionExistentialRestrictionAxiom.
    *
    * @param tbox OMF metadata graph where the EntityDefinitionExistentialRestrictionAxiom will be added, if it isn't there
    * @param axiomT The OMF axiom to represent in the OMF metadata graph, tbox.
    * @return
    */
  def createOMFEntityDefinitionExistentialRestrictionAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityDefinitionExistentialRestrictionAxiom)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionExistentialRestrictionAxiom = {
    val subI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.sub)
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.rel)
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.range)
    for {
      axiomIRI <-makeMetadataInstanceIRI(omfMetadata.get,
        "ExistentialDefinitionRestriction",
        tbox.iri,
        axiomT.sub.iri,
        axiomT.rel.iri,
        axiomT.range.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
    } yield {
    for {
      change <- Seq(
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLDeclarationAxiom(axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
                                                                      OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                                      axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLClassAssertionAxiom(OMF_ENTITY_DEFINITION_EXISTENTIAL_RESTRICTION_AXIOM, axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_ENTITY_DOMAIN, axiomI, subI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_RESTRICTS_RELATIONSHIP, axiomI, relI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_RANGE, axiomI, rangeI)))
    //    ontManager.applyChange( new AddAxiom(
    //    o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //      OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } {
      val result = ontManager.applyChange(change)
      require(
               result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
               s"\ncreateOMFEntityDefinitionExistentialRestrictionAxiomInstance:\n$change")
    }
    OMF_ENTITY_DEFINITION_EXISTENTIAL_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  def createOMFEntityConceptDesignationTerminologyGraphAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityConceptDesignationTerminologyGraphAxiom)
  : Set[java.lang.Throwable] \/ types.EntityConceptDesignationTerminologyGraphAxiom = {
    val cI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.entityConceptDesignation)
    val gI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(axiomT.designationTerminologyGraph)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
                                                                "ConceptDesignationTerminologyGraph",
                                                                tbox.iri,
                                                                axiomT.entityConceptDesignation.iri,
                                                                axiomT.designationTerminologyGraph.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
    } yield {
    for {
      change <- Seq(new AddAxiom(omfMetadata.get,
                                 owlDataFactory
                                 .getOWLDeclarationAxiom(axiomI)),
                    new AddAxiom(omfMetadata.get,
                                 owlDataFactory
                                 .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
                                                                     OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                                     axiomI)),
                    new AddAxiom(omfMetadata.get,
                                 owlDataFactory
                                 .getOWLClassAssertionAxiom(OMF_ENTITY_CONCEPT_DESIGNATION_TERMINOLOGY_GRAPH_AXIOM,
                                                            axiomI)),
                    new AddAxiom(omfMetadata.get,
                                 owlDataFactory
                                 .getOWLObjectPropertyAssertionAxiom(OMF_HAS_ENTITY_CONCEPT_DESIGNATION,
                                                                     axiomI, cI)),
                    new AddAxiom(omfMetadata.get,
                                 owlDataFactory
                                 .getOWLObjectPropertyAssertionAxiom(OMF_HAS_DESIGNATION_TERMINOLOGY_GRAPH,
                                                                     axiomI, gI))
                   )
    } {
      val result = ontManager.applyChange(change)
      require(
               result == ChangeApplied.SUCCESSFULLY,
               s"\ncreateOMFEntityConceptDesignationTerminologyGraphAxiomInstance:\n$change")
    }
    OMF_ENTITY_CONCEPT_DESIGNATION_TERMINOLOGY_GRAPH_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  /**
    * Create an OMF EntityReifiedRelationshipSubClassAxiom.
    *
    * @param tbox OMF metadata graph where the EntityReifiedRelationshipSubClassAxiom will be added, if it isn't there
    * @param axiomT The OMF axiom to represent in the OMF metadata graph, tbox.
    * @return
    */
  def createOMFEntityReifiedRelationshipSubClassAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityReifiedRelationshipSubClassAxiom)
  : Set[java.lang.Throwable] \/ types.EntityReifiedRelationshipSubClassAxiom = {
    val subI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.sub)
    val supI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.sup)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
                                                                "RelationshipSubClass",
                                                                tbox.iri,
                                                                axiomT.sub.iri,
                                                                axiomT.sup.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
    } yield {
    for {
      change <- Seq(
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLDeclarationAxiom(axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
                                                                      OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                                      axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLClassAssertionAxiom(OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM, axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_GENERAL_REIFIED_RELATIONSHIP,
                                                                      axiomI, supI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_SPECIFIC_REIFIED_RELATIONSHIP,
                                                                      axiomI, subI)))
    //    ontManager.applyChange( new AddAxiom(
    //    o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //      OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } {
      val result = ontManager.applyChange(change)
      require(
               result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
               s"\ncreateOMFEntityReifiedRelationshipSubClassAxiomInstance:\n$change")
    }
    OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  def createOMFEntityReifiedRelationshipContextualizationAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityReifiedRelationshipContextualizationAxiom)
  : Set[java.lang.Throwable] \/ types.EntityReifiedRelationshipContextualizationAxiom = {

    val domainI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.domain)
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.rel)
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.range)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
        "EntityReifiedRelationshipContextualization",
        tbox.iri,
        axiomT.domain.iri,
        axiomT.rel.iri,
        axiomT.range.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyModelTermAxiomChanges(
        axiomT,
        "createOMFEntityReifiedRelationshipContextualizationAxiomInstance",
        Seq(
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
                OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                axiomI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLClassAssertionAxiom(OMF_ENTITY_REIFIED_RELATIONSHIP_CONTEXTUALIZATION_AXIOM, axiomI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_ENTITY_DOMAIN,
                axiomI, domainI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_REIFIED_RELATIONSHIP,
                axiomI, relI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_RANGE,
                axiomI, rangeI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_SHORT_NAME,
                axiomI,
                owlDataFactory.getOWLLiteral(axiomT.contextName)))
        ))
    } yield {
      OMF_ENTITY_REIFIED_RELATIONSHIP_CONTEXTUALIZATION_AXIOM2Instance += (axiomT -> axiomI)
      axiomT
    }
  }

  def createOMFEntityReifiedRelationshipExistentialRestrictionAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityReifiedRelationshipExistentialRestrictionAxiom)
  : Set[java.lang.Throwable] \/ types.EntityReifiedRelationshipExistentialRestrictionAxiom = {

    val domainI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.domain)
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.rel)
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.range)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
        "EntityReifiedRelationshipExistentialRestriction",
        tbox.iri,
        axiomT.domain.iri,
        axiomT.rel.iri,
        axiomT.range.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyModelTermAxiomChanges(
        axiomT,
        "createOMFEntityReifiedRelationshipExistentialRestrictionAxiomInstance",
        Seq(
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
                OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                axiomI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLClassAssertionAxiom(OMF_ENTITY_REIFIED_RELATIONSHIP_EXISTENTIAL_RESTRICTION_AXIOM, axiomI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_ENTITY_DOMAIN,
                axiomI, domainI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_REIFIED_RELATIONSHIP,
                axiomI, relI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_RANGE,
                axiomI, rangeI))))
      } yield {
      OMF_ENTITY_REIFIED_RELATIONSHIP_EXISTENTIAL_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
      axiomT
    }
  }

  def createOMFEntityReifiedRelationshipUniversalRestrictionAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityReifiedRelationshipUniversalRestrictionAxiom)
  : Set[java.lang.Throwable] \/ types.EntityReifiedRelationshipUniversalRestrictionAxiom = {

    val domainI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.domain)
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.rel)
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.range)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
        "EntityReifiedRelationshipUniversalRestriction",
        tbox.iri,
        axiomT.domain.iri,
        axiomT.rel.iri,
        axiomT.range.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyModelTermAxiomChanges(
        axiomT,
        "createOMFEntityReifiedRelationshipUniversalRestrictionAxiomInstance",
        Seq(
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
                OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                axiomI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLClassAssertionAxiom(OMF_ENTITY_REIFIED_RELATIONSHIP_UNIVERSAL_RESTRICTION_AXIOM, axiomI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_ENTITY_DOMAIN,
                axiomI, domainI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_REIFIED_RELATIONSHIP,
                axiomI, relI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_RANGE,
                axiomI, rangeI))))
      } yield {
      OMF_ENTITY_REIFIED_RELATIONSHIP_UNIVERSAL_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
      axiomT
    }
  }

  def createOMFScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral)
  : Set[java.lang.Throwable] \/ types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  = {
    val entityI = OMF_MODEL_TYPE_TERM2Instance(axiomT.restrictedEntity)
    val dpropI = OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR2Instance(axiomT.restrictingDataProperty)
    for {
      axiomIRI <- makeMetadataInstanceIRI(
        omfMetadata.get,
        "ScalarDataRelationshipRestrictionAxiomFromEntityToLiteral",
        tbox.iri,
        axiomT.restrictedEntity.iri,
        axiomT.restrictingDataProperty.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
    } yield {
      for {
        change <- Seq(
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
            .getOWLClassAssertionAxiom(OMF_MODEL_SCALAR_DATA_RELATIONSHIP_RESTRICTION_AXIOM_FROM_ENTITY_TO_LITERAL,
              axiomI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_ENTITY_DOMAIN, axiomI, entityI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTING_SCALAR_DATA_RELATIONSHIP, axiomI, dpropI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_LITERAL_RESTRICTION, axiomI, axiomT.literalRestriction))
        )
      } {
        val result = ontManager.applyChange(change)
        require(
          result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
          s"\ncreateOMFScalarDataRelationshipRestrictionAxiomFromEntityToLiteral:\n$change")
      }
      OMF_MODEL_SCALAR_DATA_RELATIONSHIP_RESTRICTION_AXIOM_FROM_ENTITY_TO_LITERAL2Instance += (axiomT -> axiomI)
      axiomT
    }
  }

  /**
    * Create an OMF ScalarDataTypeFacetRestrictionAxiom.
    *
    * @param tbox OMF metadata graph where the ScalarDataTypeFacetRestrictionAxiom will be added, if it isn't there
    * @param axiomT The OMF axiom to represent in the OMF metadata graph, tbox.
    * @return
    */
  def createOMFScalarDataTypeFacetRestrictionAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.ScalarDataTypeFacetRestrictionAxiom)
  : Set[java.lang.Throwable] \/ types.ScalarDataTypeFacetRestrictionAxiom = {
    val subI = OMF_MODEL_SCALAR_DATA_TYPE2Instance(axiomT.sub)
    val supI = OMF_MODEL_SCALAR_DATA_TYPE2Instance(axiomT.sup)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
        "ScalarDataTypeFacetRestriction",
        tbox.iri,
        axiomT.sub.iri,
        axiomT.sup.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
    } yield {
    for {
      change <-
      Seq(new AddAxiom(omfMetadata.get,
                       owlDataFactory
                       .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(omfMetadata.get,
                       owlDataFactory
                       .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
                                                           OMF_MODEL_TERMINOLOGY_GRAPH2Instance(tbox),
                                                           axiomI)),
          new AddAxiom(omfMetadata.get,
                       owlDataFactory
                       .getOWLClassAssertionAxiom(OMF_SCALAR_DATA_TYPE_FACET_RESTRICTION_AXIOM, axiomI)),
          new AddAxiom(omfMetadata.get,
                       owlDataFactory
                       .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_SCALAR_DATATYPE,
                                                           axiomI, supI)),
          new AddAxiom(omfMetadata.get,
                       owlDataFactory
                       .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTING_SCALAR_DATATYPE,
                                                           axiomI, subI)))
    } {
      val result = ontManager.applyChange(change)
      require(
               result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
               s"\ncreateOMFScalarDataTypeFacetRestrictionAxiomInstance:\n$change")
    }
    OMF_SCALAR_DATA_TYPE_FACET_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  object Conversions {

    def convert1
    (acc: types.Mutable2IMutableTerminologyMap,
     mg: types.MutableModelTerminologyGraph)
    : Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap = {

      immutableTBoxGraphs
        .get(mg.iri)
        .fold[Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap](
        convert1New(acc, mg)
      ) {
        ig =>
          val acc1 =
            if (acc.contains(mg))
              acc
            else
              acc + (mg -> ig)

          \/-(acc1)
      }
    }

    def convert1New
    (acc: types.Mutable2IMutableTerminologyMap,
     mg: types.MutableModelTerminologyGraph)
    : Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap
    = {
      require(!acc.contains(mg), s"convert1: acc=${acc.size}, m=${mg.kindIRI}")
      val tgraph = fromTerminologyGraph(mg)

      val ti0
      : Set[java.lang.Throwable] \/ Vector[types.ImmutableModelTerminologyGraph]
      = Vector().right

      val tiN
      : Set[java.lang.Throwable] \/ Vector[types.ImmutableModelTerminologyGraph]
      = (ti0 /: tgraph.imports) { (tii, ni) =>
        tii +++
          (ni match {
            case g: types.ImmutableModelTerminologyGraph =>
              Vector(g).right
            case g: types.MutableModelTerminologyGraph =>
              acc
                .get(g)
                .fold[Set[java.lang.Throwable] \/ Vector[types.ImmutableModelTerminologyGraph]](
                Set(OMFError.omfError(
                  s"""No Immutable graph available for an imported mutable graph:
                      |mutable graph to convert:
                      |$mg
                      |imported mutable graph that should have been converted:
                      |$g
                      |""".stripMargin)).left) { g =>
                Vector(g).right
              }
          })
      }

      tiN.flatMap { is =>

        val ins
        : Set[java.lang.Throwable] \/ Option[(OWLAPIOMF#ModelEntityConcept, OWLAPIOMF#ModelTerminologyGraph)]
        = tgraph
          .nesting
          .fold[Set[java.lang.Throwable] \/ Option[(OWLAPIOMF#ModelEntityConcept, OWLAPIOMF#ModelTerminologyGraph)]](
          \/-(None)
        ){ ns =>
          immutableTBoxGraphs
            .get(ns._2.iri)
            .fold[Set[java.lang.Throwable] \/ Option[(OWLAPIOMF#ModelEntityConcept, OWLAPIOMF#ModelTerminologyGraph)]](
            -\/(Set(OMFError.omfError(
              s"Cannot convert mutable graph because there is no immutable graph for the nesting parent\n"+
              s"nesting parent: ${ns._2.iri}\n"+
              s"mutable graph: ${mg.iri}"
            )))
          ){ iparent =>
            \/-(Some((ns._1, iparent)))
          }
        }

        ins.flatMap { inesting =>

          val itgraph = tgraph.copy(imports = is, nesting = inesting)

          // @todo IMCEI-128 can we already pass the imports & nesting?
          // to do that, the import & nesting objects must not refer to the extending graph or to the nested graph respectively.
          val ig =
            types
              .ImmutableModelTerminologyGraph(
                kind = mg.kind,
                ont = mg.ont,
                extraProvenanceMetadata = mg.extraProvenanceMetadata,
                tgraph.aspects.toVector,
                tgraph.concepts.toVector,
                tgraph.reifiedRelationships.toVector,
                tgraph.unreifiedRelationships.toVector,
                tgraph.scalarDataTypes.toVector,
                tgraph.structuredDataTypes.toVector,
                tgraph.entity2scalarDataRelationships.toVector,
                tgraph.entity2structureDataRelationships.toVector,
                tgraph.structure2scalarDataRelationships.toVector,
                tgraph.structure2structureDataRelationships.toVector,
                tgraph.axioms.toVector)(mg.ops)

          val i_mg_relativePath_dataValue = getModelTerminologyGraphRelativeIRIPath(mg)
          require(i_mg_relativePath_dataValue.isDefined)

          val i_mg_relativePath_value: String = i_mg_relativePath_dataValue.get
          require(!i_mg_relativePath_value.endsWith("_Gro"))
          require(!i_mg_relativePath_value.endsWith("_Grw"))

          val i_mg_iriHashPrefix_value = getModelTerminologyGraphIRIHashPrefix(mg)

          val m2i: types.Mutable2IMutableTerminologyMap = Map(mg -> ig) ++ acc

          val result =
            register(
              ig, itgraph, m2i,
              mg.getTerminologyGraphShortName,
              mg.getTerminologyGraphUUID,
              i_mg_relativePath_value, i_mg_iriHashPrefix_value)

          result
        }
      }
    }

    @scala.annotation.tailrec
    def convert
    (acc: types.Mutable2IMutableTerminologyMap,
     queue: Seq[types.MutableModelTerminologyGraph],
     visited: Seq[types.MutableModelTerminologyGraph])
    (implicit store: OWLAPIOMFGraphStore)
    : Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap = {
      if (queue.isEmpty) {
        if (visited.isEmpty)
          \/-(acc)
        else {
          val mg = visited.head

          val nestedQueue =
            lookupNestingAxiomsForNestingParent(mg)
              .map(_.nestedChild)
              .flatMap {
                case _: types.ImmutableModelTerminologyGraph =>
                  None

                case mn: types.MutableModelTerminologyGraph   =>
                  if (queue.contains(mn))
                    None
                  else if (acc.contains(mn))
                    None
                  else if (visited.contains(mn))
                    None
                  else
                    Some(mn)
              }
              .to[Seq]

          if (acc.contains(mg))
            convert(acc, nestedQueue, visited.tail)
          else
            convert1(acc, mg) match {
              case -\/(nels) =>
                -\/(nels)
              case \/-(acc1) =>
                convert(acc1, nestedQueue, visited.tail)
            }
        }
      } else {
        val mg = queue.head
        val mgInfo = fromTerminologyGraph(mg)

        val extendedQueue =
          mgInfo
          .imports
          .flatMap {
            case _: types.ImmutableModelTerminologyGraph =>
              None
            case me: types.MutableModelTerminologyGraph =>
              if (queue.contains(me))
                None
              else if (acc.contains(me))
                None
              else
                Some(me)
          }
          .to[Seq]

        convert(
          acc,
          extendedQueue ++ queue.tail,
          queue.head +: visited)
      }
    }

  }

  // OMF API
  def asImmutableTerminologyGraph
  (m2i: types.Mutable2IMutableTerminologyMap,
   g: types.MutableModelTerminologyGraph)
  : Set[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap)
  = for {
      m2i <- Conversions.convert(m2i, Seq(g), Seq())(this)
    } yield {
      require(m2i.contains(g))
      (m2i(g), m2i)
    }

  /**
    * Registers an immutable TBox graph in the store's OMF Metadata graph.
    *
    * @note postcondition: `lookupTerminologyGraph(g.iri)` should be `\/-(g)`
    * @param g The immutable TBox graph to register in the store's current OMF Metadata graph
    * @param info The TBox signature of `g`
    * @param m2i The current map of mutable to immtable TBox graphs
    * @param name The name of `g`
    * @param uuid The uuid of `g`
    * @param relativeIRIPath The relativeIRIPath of `g`
    * @param relativeIRIHashPrefix The relativeIRIHashPrefix of `g`
    * @return `m2i`
    */
  def register
  (g: types.ImmutableModelTerminologyGraph,
   info: OWLAPITerminologyGraphSignature,
   m2i: types.Mutable2IMutableTerminologyMap,
   name: Option[String],
   uuid: Option[String],
   relativeIRIPath: String,
   relativeIRIHashPrefix: Option[String])
  : Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap = {

    val ok1 = immutableTBoxGraphs.put(g.iri, g)
    if (ok1.nonEmpty)
      require(
        ok1.isEmpty,
        s"register g: ${g.iri}")

    makeMetadataInstanceIRI(omfMetadata.get, "Gro", g.iri)
    .flatMap { graphIRI =>

      val graphI = owlDataFactory.getOWLNamedIndividual(graphIRI)
      val ok2 = OMF_MODEL_TERMINOLOGY_GRAPH2Instance.put(g, graphI)
      require(ok2.isEmpty, s"register g: ${g.kindIRI}")

      val okind = g.kind match {
        case TerminologyKind.isToplevelDefinition =>
          OMF_TOPLEVEL_DEFINITION_TBOX
        case TerminologyKind.isDefinition =>
          OMF_DEFINITION_TBOX
        case TerminologyKind.isToplevelDesignation =>
          OMF_TOPLEVEL_DESIGNATION_TBOX
        case TerminologyKind.isDesignation =>
          OMF_DESIGNATION_TBOX
      }

      val changes1
      : types.UnitNES
      = (for {
        change <- Seq(
          new AddAxiom(omfMetadata.get,
            owlDataFactory.getOWLDeclarationAxiom(graphI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH,
              graphI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_TERMINOLOGY_KIND, graphI,
                okind)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, graphI,
                g.iri.toString)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_PATH, graphI,
                relativeIRIPath))
        ) ++
          calculateRelativeIRIUnhashedPrefixHashedSuffix(relativeIRIPath, relativeIRIHashPrefix)
          .fold[Seq[OWLOntologyChange]](Seq(
            new AddAxiom(omfMetadata.get,
              owlDataFactory
                .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_FILENAME, graphI,
                  relativeIRIPath + "_Gro"))
          )){ case (unhashedPrefix, hashedSuffix) =>
              Seq (
                new AddAxiom(omfMetadata.get,
                  owlDataFactory
                    .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_HASH_PREFIX, graphI,
                      unhashedPrefix)),
                new AddAxiom(omfMetadata.get,
                  owlDataFactory
                    .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_HASH_SUFFIX, graphI,
                      hashedSuffix)),
                new AddAxiom(omfMetadata.get,
                  owlDataFactory
                    .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_FILENAME, graphI,
                      unhashedPrefix+hashedSuffix+"_Gro"))
              )
          } ++
          createOntologyChangesForOMFModelTerminologyGraphProvenanceMetadata(g, graphI)
      } yield {
        val result = ontManager.applyChange(change)
        if (result == ChangeApplied.SUCCESSFULLY)
          types.rightUnitNES
        else
          -\/(Set(OMFError.omfError(s"\nregister:\n$change")))
      })
        .foldLeft(types.rightUnitNES)( _ +++ _ )

      changes1.flatMap { _ =>

        val changes2
        : types.UnitNES
        = (for {
          _ <-
          name
            .fold[Set[java.lang.Throwable] \/ Unit](
            \/-(())
          ) { label =>
            setTerminologyGraphShortName(g, label)
          }

          _ <-
          uuid
            .fold[Set[java.lang.Throwable] \/ Unit](
            \/-(())
          ) { id =>
            setTerminologyGraphUUID(g, id)
          }

          _ <- info.nesting.fold[Set[java.lang.Throwable] \/ Unit](\/-(())) {
            case (nestingC, nestingG) =>
            createTerminologyGraphDirectNestingAxiom(
              parentG = nestingG,
              parentC = nestingC,
              childG = g).map(_ => ())
          }

          // @todo IMCEI-128 move to the constructor
          _ <- {
            (().right[Set[java.lang.Throwable]] /: info.imports) {
              (acc: Set[java.lang.Throwable] \/ Unit, i: types.ModelTerminologyGraph) =>
                acc +++ createTerminologyGraphDirectExtensionAxiom(extendingG = g, extendedG = i).map(_ => ())
            }
          }

          // @todo IMCEI-128 move to the constructor
          _ <- {
            (().right[Set[java.lang.Throwable]] /: info.aspects) {
              (acc: Set[java.lang.Throwable] \/ Unit, a: types.ModelEntityAspect) =>
                acc +++ registerOMFModelEntityAspectInstance(g, a).map(_ => ())
            }
          }

          _ <- {
            (().right[Set[java.lang.Throwable]] /: info.concepts) {
              (acc: Set[java.lang.Throwable] \/ Unit, c: types.ModelEntityConcept) =>
                acc +++ registerOMFModelEntityConceptInstance(g, c).map(_ => ())
            }
          }

          _ <- {
            (().right[Set[java.lang.Throwable]] /: info.reifiedRelationships) {
              (acc: Set[java.lang.Throwable] \/ Unit, r: types.ModelEntityReifiedRelationship) =>
                acc +++ registerOMFModelEntityReifiedRelationshipInstance(g, r).map(_ => ())
            }
          }

          _ <- {
            (().right[Set[java.lang.Throwable]] /: info.scalarDataTypes) {
              (acc: Set[java.lang.Throwable] \/ Unit, sc: types.ModelScalarDataType) =>
                acc +++ registerOMFModelScalarDataTypeInstance(g, sc).map(_ => ())
            }
          }

          _ <- {
            (().right[Set[java.lang.Throwable]] /: info.structuredDataTypes) {
              (acc: Set[java.lang.Throwable] \/ Unit, st: types.ModelStructuredDataType) =>
                acc +++ registerOMFModelStructuredDataTypeInstance(g, st).map(_ => ())
            }
          }

          _ <- {
            (().right[Set[java.lang.Throwable]] /: info.axioms) {
              (acc: Set[java.lang.Throwable] \/ Unit, axiom: types.ModelTermAxiom) =>
                acc +++ (axiom match {
                  case ax: types.EntityConceptSubClassAxiom =>
                    createOMFEntityConceptSubClassAxiomInstance(g, ax).map(_ => ())
                  case ax: types.EntityReifiedRelationshipSubClassAxiom =>
                    createOMFEntityReifiedRelationshipSubClassAxiomInstance(g, ax).map(_ => ())
                  case ax: types.EntityDefinitionAspectSubClassAxiom =>
                    createOMFEntityDefinitionAspectSubClassAxiomInstance(g, ax).map(_ => ())
                  case ax: types.EntityConceptDesignationTerminologyGraphAxiom =>
                    System.out.println(
                      s"""*** EntityConceptDesignationTerminologyGraphAxiom:
                          |entity:${ax.entityConceptDesignation.iri}
                          |des. g:${ax.designationTerminologyGraph.kindIRI}""".stripMargin)
                    \/-(())
                case ax:
                  types.EntityDefinitionUniversalRestrictionAxiom =>
                  createOMFEntityDefinitionUniversalRestrictionAxiomInstance(g, ax).map(_ => ())
                case ax: types.EntityDefinitionExistentialRestrictionAxiom =>
                  createOMFEntityDefinitionExistentialRestrictionAxiomInstance(g, ax).map(_ => ())
                case ax: types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral =>
                  createOMFScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(g, ax).map(_ => ())
                // case ax: types.ScalarDataTypeFacetRestriction =>
                case ax =>
                  Set(
                    OMFError
                      .omfError(s"Unrecognized axiom: $ax")).left
              })}
          }

        } yield types.rightUnitNES)
          .foldLeft(types.rightUnitNES)( _ +++ _ )

        changes2.flatMap { _ =>
          \/-(m2i)
        }
      }
    }
  }

  def loadTerminologyGraph
  (iri: IRI)
  (implicit ops: OWLAPIOMFOps)
  : Set[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap)
  = loadBuiltinDatatypeMap()
    .flatMap { _ =>
      immutableTBoxGraphs
        .get(iri)
        .fold[Set[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap)](
        OWLAPIOMFLoader.loadTerminologyGraph(iri)(ops, this)
      ){ ig =>
        \/-((ig, Map()))
      }
    }

  def convertTerminologyGraphFromOntologyDocument
  (s: OntologyLoadedState,
   m2i: types.Mutable2IMutableTerminologyMap,
   ontIRI: IRI,
   ont: OWLOntology)
  (implicit ops: OWLAPIOMFOps)
  : Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap
  = {

    val extendedGraphs
    : types.ImmutableModelTerminologyGraphsNES
    = s
      .extensions
      .filter(_.extendingG == ontIRI)
      .foldLeft(types.emptyImmutableTerminologyGraphsNES) {
        ( acc: types.ImmutableModelTerminologyGraphsNES,
          ext: ExtendingOntologyToExtendedGraphIRI) =>
          acc +++
            immutableTBoxGraphs
              .get(ext.extendedG)
              .fold[types.ImmutableModelTerminologyGraphsNES](
              -\/(Set(OMFError.omfError(
                s"Failed to resolve immutable OMF Terminology Graph for $ext"
              )))
            ){ extG =>
              \/-(Set(extG))
            }
      }

    val nestingContextAndGraphIfAny
    : types.NestingConceptAndGraphOptionNES
    = s
      .nesting2nested
      .find(_.nestedG == ontIRI)
      .fold[types.NestingConceptAndGraphOptionNES](types.emptyNestingConceptAndGraphNES) { n2n =>
      immutableTBoxGraphs
        .get(n2n.nestingG)
        .fold[types.NestingConceptAndGraphOptionNES](
        -\/(Set(OMFError.omfError(
          s"Failued to resolve immutable OMF Terminology Graph for $n2n")))
      ) { nestingG =>

        ops
          .lookupEntityConcept(nestingG, n2n.nestingC.getIRI, recursively = false)(this)
          .fold[types.NestingConceptAndGraphOptionNES](
          -\/(Set(OMFError.omfError(
            s"Failed to resolve nesting concept for $n2n in $nestingG")))
        ) { nestingC =>
          \/-(Some((nestingC, nestingG)))
        }
      }
    }

    val result
    : Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap
    = for {
      extensions <- extendedGraphs
      nesting <- nestingContextAndGraphIfAny
      resolver <- types
        .immutableModelTerminologyGraphResolver(omfMetadata.get, s, ont, extensions, nesting, m2i, this)
      resolved <- resolver.resolve()
    } yield {
      immutableTBoxGraphs.put(ontIRI, resolved._1)
      resolved._2
    }

    result
  }

  def saveOMFMetadataOntology( saveIRI: IRI ): Set[java.lang.Throwable] \/ Unit =
    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set(
            OMFError.omfException(
              s"saving OMF Metadata Ontology failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply(
        omfMetadata
            .fold[Set[java.lang.Throwable] \/ Unit](
          Set(
            OMFError.omfError(
              s"cannot save OMF Metadata Ontology because it's not yet created.")
          ).left
        ) { ontM =>
          ontManager.saveOntology(ontM, saveIRI).right
        })

  def saveTerminologyGraph
  (g: types.ModelTerminologyGraph)
  (implicit ops: OWLAPIOMFOps)
  : Set[java.lang.Throwable] \/ Unit = {
    val iri = catalogIRIMapper.resolveIRI(g.iri, catalogIRIMapper.saveResolutionStrategy)
    g.save(iri)
  }

  def saveTerminologyGraph
  (g: types.ModelTerminologyGraph, os: java.io.OutputStream)
  (implicit ops: OWLAPIOMFOps)
  : Set[java.lang.Throwable] \/ Unit =
    g.save(os)

  def makeTerminologyGraph
  (iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  (implicit ops: OWLAPIOMFOps)
  : Set[java.lang.Throwable] \/ types.MutableModelTerminologyGraph =
    mutableTBoxGraphs
    .get(iri)
    .fold[Set[java.lang.Throwable] \/ types.MutableModelTerminologyGraph](
      if (ontManager.contains(iri))
        Set(
          OMFError
          .omfOpsError(ops, s"makeTerminologyGraph(iri='$iri') --  already exists!")
        ).left
      else
      // not yet registered.
        createOMFModelTerminologyGraph(
          omfMetadata.get,
          iri, relativeIRIPath, relativeIRIHashPrefix,
          ontManager.createOntology(iri), kind, extraProvenanceMetadata)
    ) { g =>
      g.right
    }

  def loadInstanceGraph
  (iri: IRI)
  : Set[java.lang.Throwable] \/ instances.ImmutableModelInstanceGraph =
    ???

  def asImmutableInstanceGraph
  (g: instances.MutableModelInstanceGraph)
  : Set[java.lang.Throwable] \/ instances.ImmutableModelInstanceGraph =
    ???

  def makeInstanceGraph
  (iri: IRI,
   instantiatedTGraphs: Iterable[types.ImmutableModelTerminologyGraph],
   extendedIGraphs: Iterable[instances.ImmutableModelInstanceGraph])
  : Set[java.lang.Throwable] \/ instances.MutableModelInstanceGraph =
    ???

}