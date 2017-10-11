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

import java.lang.System
import java.util.UUID

import gov.nasa.jpl.omf.scala.binding.owlapi.common.{ImmutableModule, Module}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms.{BundledTerminologyAxiom, ConceptDesignationTerminologyAxiom, TerminologyExtensionAxiom, TerminologyNestingAxiom}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.core.{OMFError, TerminologyKind}
import gov.nasa.jpl.omf.scala.core.OMLString.LocalName
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.{Boolean, None, Option, Some, StringContext, Tuple2, Unit}
import scala.Predef.{ArrowAssoc, String, augmentString, byteArrayOps, refArrayOps, require}
import scalaz._
import Scalaz._

/**
  * This is a legacy OWL-based metadata for OMF models.
  * It is refactored out of the OWLAPIOMFGraphStore.
  * It will be eliminated when this legacy metadata is no longer needed
  * for the ontology processing workflow.
  *
  * @param omfModule
  * @param ontManager
  */
abstract class OWLAPIOMFGraphStoreMetadata(omfModule: OWLAPIOMFModule, ontManager: OWLOntologyManager) {

  private val LOG: Boolean =
    "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.GraphStoreMetadata")

  private val mDigest = java.security.MessageDigest.getInstance("SHA-256")

  private def hashMessage(message: String): String = {
    val result
    : String
    = mDigest.digest(message.getBytes("UTF-8")).map("%02x".format(_)).mkString

    result
  }

  private val owlDataFactory = ontManager.getOWLDataFactory

  // OMF Metadata.

  @scala.volatile
  protected var omfMetadata: Option[OWLOntology] = None

  def setOMFMetadataOntology(o: OWLOntology): Unit = {
    omfMetadata = Some(o)
  }

  private lazy val omfModelOntology
  : Option[OWLOntology]
  = omfModule.omfOntologyIRI.map { omfIRI =>
    val o = ontManager.loadOntology(omfIRI)
    require(o != null, s"Could not find the OMF metadata ontology: ${omfModule.omfOntologyIRI}")
    o
  }

  private lazy val omfModelClasses
  : Map[String, OWLClass]
  = omfModelOntology.fold[Map[String, OWLClass]](Map.empty) { mo =>
    mo.classesInSignature(Imports.EXCLUDED).toScala[Set]
      .map { c => c.getIRI.getRemainder.get -> c }
      .toMap
  }

  private lazy val omfModelObjectProperties
  : Map[String, OWLObjectProperty]
  = omfModelOntology.fold[Map[String, OWLObjectProperty]](Map.empty) { mo =>
    mo.objectPropertiesInSignature(Imports.EXCLUDED).toScala[Set]
      .map { op => op.getIRI.getRemainder.get -> op }
      .toMap
  }

  private lazy val omfModelDataTypes
  : Map[String, OWLDatatype]
  = omfModelOntology.fold[Map[String, OWLDatatype]](Map.empty) { mo =>
    mo.datatypesInSignature(Imports.EXCLUDED).toScala[Set]
      .map { dt => dt.getIRI.getRemainder.get -> dt }
      .toMap
  }

  private lazy val omfModelDataProperties
  : Map[String, OWLDataProperty]
  = omfModelOntology.fold[Map[String, OWLDataProperty]](Map.empty) { mo =>
    mo.dataPropertiesInSignature(Imports.EXCLUDED).toScala[Set]
      .map { dp => dp.getIRI.getRemainder.get -> dp }
      .toMap
  }

  private lazy val omfNamedIndividuals
  : Map[String, OWLNamedIndividual]
  = omfModelOntology.fold[Map[String, OWLNamedIndividual]](Map.empty) { mo =>
    mo.individualsInSignature(Imports.EXCLUDED).toScala[Set]
      .map { dp => dp.getIRI.getRemainder.get -> dp }
      .toMap
  }

  private lazy val allAnnotationProperties
  : Map[String, OWLAnnotationProperty]
  = omfModelOntology.fold[Map[String, OWLAnnotationProperty]](Map.empty) { mo =>
    mo.annotationPropertiesInSignature(Imports.INCLUDED).toScala[Set]
      .map { ap => ap.getIRI.getShortForm -> ap }
      .toMap
  }


  // OMF model.

  // ModelTermAxiom
  lazy val OMF_ENTITY_DEFINITION_EXISTENTIAL_RESTRICTION_AXIOM =
    omfModelClasses("EntityDefinitionExistentialRestrictionAxiom")
  protected val OMF_ENTITY_DEFINITION_EXISTENTIAL_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[EntityExistentialRestrictionAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_DEFINITION_UNIVERSAL_RESTRICTION_AXIOM =
    omfModelClasses("EntityDefinitionUniversalRestrictionAxiom")
  protected val OMF_ENTITY_DEFINITION_UNIVERSAL_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[EntityUniversalRestrictionAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM =
    omfModelClasses("EntityConceptSubClassAxiom")
  protected val OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance =
    scala.collection.mutable.HashMap[ConceptSpecializationAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_CONCEPT_DESIGNATION_TERMINOLOGY_GRAPH_AXIOM =
    omfModelClasses("EntityConceptDesignationTerminologyGraphAxiom")
  protected val OMF_ENTITY_CONCEPT_DESIGNATION_TERMINOLOGY_GRAPH_AXIOM2Instance =
    scala.collection.mutable.HashMap[ConceptDesignationTerminologyAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM =
    omfModelClasses("EntityDefinitionAspectSubClassAxiom")
  protected val OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM2Instance =
    scala.collection.mutable.HashMap[AspectSpecializationAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM =
    omfModelClasses("EntityReifiedRelationshipSubClassAxiom")
  protected val OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM2Instance =
    scala.collection.mutable.HashMap[ReifiedRelationshipSpecializationAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_SCALAR_DATA_PROPERTY_EXISTENTIAL_RESTRICTION_AXIOM =
    omfModelClasses("EntityScalarDataPropertyExistentialRestrictionAxiom")
  protected val OMF_ENTITY_SCALAR_DATA_PROPERTY_EXISTENTIAL_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[EntityScalarDataPropertyExistentialRestrictionAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_SCALAR_DATA_PROPERTY_UNIVERSAL_RESTRICTION_AXIOM =
    omfModelClasses("EntityScalarDataPropertyUniversalRestrictionAxiom")
  protected val OMF_ENTITY_SCALAR_DATA_PROPERTY_UNIVERSAL_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[EntityScalarDataPropertyUniversalRestrictionAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_SCALAR_DATA_PROPERTY_PARTICULAR_RESTRICTION_AXIOM =
    omfModelClasses("EntityScalarDataPropertyParticularRestrictionAxiom")
  protected val OMF_ENTITY_SCALAR_DATA_PROPERTY_PARTICULAR_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[EntityScalarDataPropertyParticularRestrictionAxiom, OWLNamedIndividual]()

  // TerminologyGraphAxiom

  lazy val OMF_BUNDLED_TERMINOLOGY_AXIOM =
    omfModelClasses("BundledTerminologyAxiom")
  protected val OMF_BUNDLED_TERMINOLOGY_AXIOM2Instance =
    scala.collection.mutable.HashMap[BundledTerminologyAxiom, OWLNamedIndividual]()

  lazy val OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM =
    omfModelClasses("TerminologyGraphDirectExtensionAxiom")
  protected val OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM2Instance =
    scala.collection.mutable.HashMap[TerminologyExtensionAxiom, OWLNamedIndividual]()

  lazy val OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM =
    omfModelClasses("TerminologyGraphDirectNestingAxiom")
  protected val OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM2Instance =
    scala.collection.mutable.HashMap[TerminologyNestingAxiom, OWLNamedIndividual]()

  lazy val OMF_TERMINOLOGY_KIND_DATATYPE =
    omfModelClasses("TerminologyKind")

  lazy val OMF_MODEL_TERMINOLOGY_GRAPH =
    omfModelClasses("ModelTerminologyGraph")
  protected val omfModule2Instance =
    scala.collection.mutable.HashMap[Module, OWLNamedIndividual]()

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
    scala.collection.mutable.HashMap[EntityScalarDataProperty, OWLNamedIndividual]()

  lazy val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_STRUCTURE =
    omfModelClasses("ModelDataRelationshipFromEntityToStructure")
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_STRUCTURE2Instance =
    scala.collection.mutable.HashMap[EntityStructuredDataProperty, OWLNamedIndividual]()

  lazy val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_SCALAR =
    omfModelClasses("ModelDataRelationshipFromStructureToScalar")
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_SCALAR2Instance =
    scala.collection.mutable.HashMap[ScalarDataProperty, OWLNamedIndividual]()

  lazy val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_STRUCTURE =
    omfModelClasses("ModelDataRelationshipFromStructureToStructure")
  protected val OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_STRUCTURE2Instance =
    scala.collection.mutable.HashMap[StructuredDataProperty, OWLNamedIndividual]()

  // ModelDataTypeDefinition
  protected val OMF_MODEL_DATA_TYPE_DEFINITION2Instance =
    scala.collection.mutable.HashMap[Datatype, OWLNamedIndividual]()

  lazy val OMF_MODEL_SCALAR_DATA_TYPE = omfModelClasses("ModelScalarDataType")
  protected val OMF_MODEL_SCALAR_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[DataRange, OWLNamedIndividual]()

  lazy val OMF_MODEL_STRUCTURED_DATA_TYPE = omfModelClasses("ModelStructuredDataType")
  protected val OMF_MODEL_STRUCTURED_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[Structure, OWLNamedIndividual]()

  lazy val OMF_MODEL_SCALAR_ONE_OF_RESTRICTION_DATA_TYPE = omfModelClasses("ScalarOneOfRestriction")
  protected val OMF_MODEL_SCALAR_ONE_OF_RESTRICTION_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[ScalarOneOfRestriction, OWLNamedIndividual]()

  lazy val OMF_MODEL_BINARY_SCALAR_RESTRICTION_DATA_TYPE = omfModelClasses("BinaryScalarRestriction")
  protected val OMF_MODEL_BINARY_SCALAR_RESTRICTION_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[BinaryScalarRestriction, OWLNamedIndividual]()

  lazy val OMF_MODEL_IRI_SCALAR_RESTRICTION_DATA_TYPE = omfModelClasses("IRIScalarRestriction")
  protected val OMF_MODEL_IRI_SCALAR_RESTRICTION_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[IRIScalarRestriction, OWLNamedIndividual]()

  lazy val OMF_MODEL_NUMERIC_SCALAR_RESTRICTION_DATA_TYPE = omfModelClasses("NumericScalarRestriction")
  protected val OMF_MODEL_NUMERIC_SCALAR_RESTRICTION_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[NumericScalarRestriction, OWLNamedIndividual]()

  lazy val OMF_MODEL_PLAIN_LITERAL_SCALAR_RESTRICTION_DATA_TYPE = omfModelClasses("PlainLiteralScalarRestriction")
  protected val OMF_MODEL_PLAIN_LITERAL_SCALAR_RESTRICTION_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[PlainLiteralScalarRestriction, OWLNamedIndividual]()

  lazy val OMF_MODEL_STRING_SCALAR_RESTRICTION_DATA_TYPE = omfModelClasses("StringScalarRestriction")
  protected val OMF_MODEL_STRING_SCALAR_RESTRICTION_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[StringScalarRestriction, OWLNamedIndividual]()

  lazy val OMF_MODEL_SYNONYM_SCALAR_RESTRICTION_DATA_TYPE = omfModelClasses("SynonymScalarRestriction")
  protected val OMF_MODEL_SYNONYM_SCALAR_RESTRICTION_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[SynonymScalarRestriction, OWLNamedIndividual]()

  lazy val OMF_MODEL_TIME_SCALAR_RESTRICTION_DATA_TYPE = omfModelClasses("TimeScalarRestriction")
  protected val OMF_MODEL_TIME_SCALAR_RESTRICTION_DATA_TYPE2Instance =
    scala.collection.mutable.HashMap[TimeScalarRestriction, OWLNamedIndividual]()

  protected val OMF_MODEL_TYPE_TERM2Instance =
    scala.collection.mutable.HashMap[types.Term, OWLNamedIndividual]()

  protected val OMF_MODEL_ENTITY_DEFINITION2Instance =
    scala.collection.mutable.HashMap[Entity, OWLNamedIndividual]()

  lazy val OMF_MODEL_ENTITY_ASPECT = omfModelClasses("ModelEntityAspect")
  protected val OMF_MODEL_ENTITY_ASPECT2Instance =
    scala.collection.mutable.HashMap[Aspect, OWLNamedIndividual]()

  lazy val OMF_MODEL_ENTITY_CONCEPT = omfModelClasses("ModelEntityConcept")
  protected val OMF_MODEL_ENTITY_CONCEPT2Instance =
    scala.collection.mutable.HashMap[Concept, OWLNamedIndividual]()

  lazy val OMF_MODEL_ENTITY_RELATIONSHIP = omfModelClasses("ModelEntityReifiedRelationship")
  protected val OMF_MODEL_ENTITY_RELATIONSHIP2Instance =
    scala.collection.mutable.HashMap[EntityRelationship, OWLNamedIndividual]()

  lazy val OMF_MODEL_ENTITY_UNREIFIED_RELATIONSHIP = omfModelClasses("ModelEntityUnreifiedRelationship")
  protected val OMF_MODEL_ENTITY_UNREIFIED_RELATIONSHIP2Instance =
    scala.collection.mutable.HashMap[UnreifiedRelationship, OWLNamedIndividual]()

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

  lazy val OMF_HAS_TERMINOLOGY_BUNDLE = omfModelObjectProperties("hasTerminologyBundle")
  lazy val OMF_HAS_BUNDLED_TERMINOLOGY = omfModelObjectProperties("hasBundledTerminology")

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
  lazy val OMF_BUNDLE_TBOX = omfNamedIndividuals("BundleTBox")
  lazy val OMF_TOPLEVEL_DEFINITION_TBOX = omfNamedIndividuals("ToplevelDefinitionTBox")
  lazy val OMF_DEFINITION_TBOX = omfNamedIndividuals("DefinitionTBox")
  lazy val OMF_TOPLEVEL_DESIGNATION_TBOX = omfNamedIndividuals("ToplevelDesignationTBox")
  lazy val OMF_DESIGNATION_TBOX = omfNamedIndividuals("DesignationTBox")

  // Data Properties
  lazy val OMF_HAS_IRI = omfModelDataProperties("hasIRI")
  lazy val OMF_HAS_UUID = omfModelDataProperties("hasUUID")
  lazy val OMF_HAS_LOCAL_NAME = omfModelDataProperties("hasShortName")

  lazy val OMF_HAS_RESTRICTED_ENTITY_DOMAIN = omfModelObjectProperties("hasRestrictedEntityDomain")
  lazy val OMF_HAS_RESTRICTING_SCALAR_DATA_RELATIONSHIP = omfModelObjectProperties("hasRestrictingScalarDataRelationship")
  lazy val OMF_HAS_LITERAL_RESTRICTION = omfModelDataProperties("hasLiteralRestriction")

  // Datatypes
  lazy val OWL_REAL: OWLDatatype = ontManager.getOWLDataFactory.getDoubleOWLDatatype

  /**
    * The ontology IRI relative path.
    * If unspecified, it is computed by removing the IMCE catalog URI prefix: http://imce.jpl.nasa.gov
    *
    * Note that in the OMF metadata, there are two variants of an ontology: Mutable, Immutable.
    * These are distinguished with a suffix _Grw, _Gro; however, the suffix is not included in the relative IRI path.
    */
  lazy val OMF_HAS_RELATIVE_IRI_PATH = omfModelDataProperties("hasRelativeIRIPath")

  /**
    * If specified, the prefix splits the relative IRI path in two:
    * - hashPrefix (not hashed), the value of this property, and
    * - hashSuffix (hashed with SHA-256)
    */
  lazy val OMF_HAS_RELATIVE_IRI_HASH_PREFIX = omfModelDataProperties("hasRelativeIRIHashPrefix")

  /**
    * When there is both IRI relative path and IRI hash prefix,
    * this property has the SHA-256 hash of the IRI relative path stripped of the IRI hash prefix.
    */
  lazy val OMF_HAS_RELATIVE_IRI_HASH_SUFFIX = omfModelDataProperties("hasRelativeIRIHashSuffix")

  /**
    * If specified, the filename of the ontology relative to the IMCE catalog without the .owl extension
    */
  lazy val OMF_HAS_RELATIVE_FILENAME = omfModelDataProperties("hasRelativeFilename")

  lazy val OMF_HAS_SHORT_NAME = omfModelDataProperties("hasShortName")
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

  lazy val OMF_MODEL_TERMINOLOGY_GRAPH_KIND
  = omfModelDataProperties("kind")

  lazy val OMF_MODEL_TERMINOLOGY_GRAPH_EXPORTED_OTI_PACKAGE_URI_PROVENANCE
  = omfModelDataProperties("exportedOTIPackageURIProvenance")

  lazy val OMF_MODEL_TERMINOLOGY_GRAPH_EXPORTED_OTI_PACKAGE_KIND_PROVENANCE
  = omfModelDataProperties("exportedOTIPackageKindProvenance")

  // ========================

  def createOntologyChangesForOMFModelTerminologyGraphProvenanceMetadata
  (mo: OWLOntology,
   omfGraph: TerminologyBox,
   graphI: OWLNamedIndividual)
  : Seq[OWLOntologyChange]
  = {
    new AddAxiom(mo, owlDataFactory
      .getOWLDataPropertyAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH_KIND,
        graphI, omfGraph.mutabilityKind)) +:
      omfGraph.extraProvenanceMetadata.fold[Seq[OWLOntologyChange]](Seq.empty) { info =>
        Seq(
          new AddAxiom(mo,
            owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH_EXPORTED_OTI_PACKAGE_KIND_PROVENANCE,
                graphI, info.provenanceKind.literal)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH_EXPORTED_OTI_PACKAGE_URI_PROVENANCE,
              graphI, info.provenanceURI))
        )
      }
  }

  def getModuleRelativeIRIPath
  (m: Module)
  : Option[String]
  = omfMetadata.fold[Option[String]](None) { mo =>
    val i_g = omfModule2Instance(m)
    val i_dataValues: Set[OWLDataPropertyAssertionAxiom] =
      mo.dataPropertyAssertionAxioms(i_g).toScala[Set]

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

  def getModuleIRIHashPrefix
  (m: Module)
  : Option[String]
  = omfMetadata.fold[Option[String]](None) { mo =>
    val i_g = omfModule2Instance(m)
    val i_dataValues: Set[OWLDataPropertyAssertionAxiom] =
      mo.dataPropertyAssertionAxioms(i_g).toScala[Set]

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

  def getModuleIRIHashSuffix
  (m: Module)
  : Option[String]
  = omfMetadata.fold[Option[String]](None) { mo =>
    val i_g = omfModule2Instance(m)
    val i_dataValues: Set[OWLDataPropertyAssertionAxiom] =
      mo.dataPropertyAssertionAxioms(i_g).toScala[Set]

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
  (relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String])
  : Option[(String, String)]
  = relativeIRIPath.flatMap { relPath =>
    relativeIRIHashPrefix.flatMap { prefix =>
      val prefixSlash = if (prefix.endsWith("/")) prefix else prefix + '/'
      if (!relPath.startsWith(prefixSlash))
        Option.empty[(String, String)]
      else
        Some(Tuple2(prefixSlash, hashMessage(relPath.stripPrefix(prefixSlash))))
    }
  }

  // @todo report errors if the graph isn't there...
  def getModelTerminologyGraphRelativeFilename
  (g: TerminologyBox)
  : Option[String]
  = omfMetadata.fold[Option[String]](None) { mo =>
    val i_g = omfModule2Instance(g)
    val i_dataValues: Set[OWLDataPropertyAssertionAxiom] =
      mo.dataPropertyAssertionAxioms(i_g).toScala[Set]

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

  // Metadata individuals

  protected def makeMetadataInstanceIRI
  (o: OWLOntology, instanceKind: String, iri: IRI*)
  : Set[java.lang.Throwable] \/ IRI = {
    require(iri.nonEmpty)

    val result
    : Set[java.lang.Throwable] \/ IRI
    = omfModule.ops.withFragment(
      o.getOntologyID.getOntologyIRI.get,
      LocalName.apply(instanceKind + "-" + hashMessage(iri.mkString("", ",", ""))))

    result
  }

  // Terminologies

  def registerMetadata
  (im: ImmutableModule,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String])
  : OMFError.Throwables \/ Boolean
  = omfMetadata.fold(false.right[OMFError.Throwables]) { mo =>
    if (omfModule2Instance.contains(im))
      false.right[OMFError.Throwables]
    else
      for {
        graphIRI <- makeMetadataInstanceIRI(mo, "Gro", im.iri)
        graphI = owlDataFactory.getOWLNamedIndividual(graphIRI)
        ok2 = omfModule2Instance.put(im, graphI)
        _ = require(ok2.isEmpty, s"register g: ${im.iri}")

        okind = im match {
          case it: ImmutableTerminologyBox =>
            Seq(new AddAxiom(mo,
              owlDataFactory
                .getOWLObjectPropertyAssertionAxiom(OMF_HAS_TERMINOLOGY_KIND, graphI,
                  it.sig.kind match {
                    case TerminologyKind.isDefinition =>
                      OMF_DEFINITION_TBOX
                    case TerminologyKind.isDesignation =>
                      OMF_TOPLEVEL_DESIGNATION_TBOX
                  }))) ++
              createOntologyChangesForOMFModelTerminologyGraphProvenanceMetadata(mo, it, graphI)
          case _ =>
            Seq.empty
        }

        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(mo,
              owlDataFactory.getOWLDeclarationAxiom(graphI)),
            new AddAxiom(mo,
              owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH,
                graphI)),
            new AddAxiom(mo,
              owlDataFactory
                .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, graphI,
                  im.iri.toString))
          ) ++ okind ++
            relativeIRIPath.fold[Seq[OWLOntologyChange]](Seq.empty) { relIRIPath =>
              Seq(
                new AddAxiom(mo,
                  owlDataFactory
                    .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_FILENAME, graphI, relIRIPath + "_Gro")),
                new AddAxiom(mo,
                  owlDataFactory
                    .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_PATH, graphI, relIRIPath))
              )
            } ++
            calculateRelativeIRIUnhashedPrefixHashedSuffix(relativeIRIPath, relativeIRIHashPrefix)
              .fold[Seq[OWLOntologyChange]](Seq.empty) { case (unhashedPrefix, hashedSuffix) =>
              Seq(
                new AddAxiom(mo,
                  owlDataFactory
                    .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_HASH_PREFIX, graphI,
                      unhashedPrefix)),
                new AddAxiom(mo,
                  owlDataFactory
                    .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_HASH_SUFFIX, graphI,
                      hashedSuffix))
              )
            },
          "register error")
      } yield true
  }

  protected def createOMFTerminologyGraphMetadata
  (iri: IRI,
   aRelativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind,
   graphT: MutableTerminologyGraph)
  : Set[java.lang.Throwable] \/ Unit
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    for {
      graphIRI <- makeMetadataInstanceIRI(mo, "Grw", iri)
      graphI = owlDataFactory.getOWLNamedIndividual(graphIRI)
      okind = kind match {
        case TerminologyKind.isDefinition =>
          OMF_DEFINITION_TBOX
        case TerminologyKind.isDesignation =>
          OMF_DESIGNATION_TBOX
      }
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq[OWLOntologyChange](
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(graphI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH, graphI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_TERMINOLOGY_KIND, graphI, okind)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, graphI, graphT.iri.toString))
        ) ++
          aRelativeIRIPath.fold[Seq[OWLOntologyChange]](Seq.empty) { relIRIPath =>
            Seq(
              new AddAxiom(mo,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_FILENAME, graphI, relIRIPath + "_Grw")),
              new AddAxiom(mo,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_PATH, graphI, relIRIPath))
            )
          } ++
          calculateRelativeIRIUnhashedPrefixHashedSuffix(aRelativeIRIPath, relativeIRIHashPrefix)
            .fold[Seq[OWLOntologyChange]](Seq.empty) { case (unhashedPrefix, hashedSuffix) =>
            Seq(
              new AddAxiom(mo,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_HASH_PREFIX, graphI,
                    unhashedPrefix)),
              new AddAxiom(mo,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_HASH_SUFFIX, graphI,
                    hashedSuffix))
            )
          } ++
          createOntologyChangesForOMFModelTerminologyGraphProvenanceMetadata(mo, graphT, graphI),
        "createOMFTerminologyGraphMetadata error")
    } yield {
      omfModule2Instance += (graphT -> graphI)
      ()
    }
  }

  protected def createOMFBundleMetadata
  (iri: IRI,
   aRelativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind,
   graphT: MutableBundle)
  : Set[java.lang.Throwable] \/ Unit
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    for {
      graphIRI <- makeMetadataInstanceIRI(mo, "Grw", iri)
      graphI = owlDataFactory.getOWLNamedIndividual(graphIRI)
      okind = kind match {
        case TerminologyKind.isDefinition =>
          OMF_DEFINITION_TBOX
        case TerminologyKind.isDesignation =>
          OMF_DESIGNATION_TBOX
      }
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq[OWLOntologyChange](
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(graphI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH, graphI)), // OMF_BUNDLE ?
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_TERMINOLOGY_KIND, graphI, okind)), // OMF_BUNDLE_TBOX ?
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, graphI, graphT.iri.toString))
        ) ++
          aRelativeIRIPath.fold[Seq[OWLOntologyChange]](Seq.empty) { relIRIPath =>
            Seq(
              new AddAxiom(mo,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_FILENAME, graphI, relIRIPath + "_Grw")),
              new AddAxiom(mo,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_PATH, graphI, relIRIPath))
            )
          } ++
          calculateRelativeIRIUnhashedPrefixHashedSuffix(aRelativeIRIPath, relativeIRIHashPrefix)
            .fold[Seq[OWLOntologyChange]](Seq.empty) { case (unhashedPrefix, hashedSuffix) =>
            Seq(
              new AddAxiom(mo,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_HASH_PREFIX, graphI,
                    unhashedPrefix)),
              new AddAxiom(mo,
                owlDataFactory
                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_HASH_SUFFIX, graphI,
                    hashedSuffix))
//              new AddAxiom(mo,
//                owlDataFactory
//                  .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_FILENAME, graphI,
//                    unhashedPrefix + hashedSuffix + "_Grw"))
            )
          } ++
          createOntologyChangesForOMFModelTerminologyGraphProvenanceMetadata(mo, graphT, graphI),
        "createOMFBundleMetadata error")
    } yield {
      omfModule2Instance += (graphT -> graphI)
      ()
    }
  }

  // Terminology Axioms

  protected def registerTerminologyGraphDirectNestingAxiom
  (childG: TerminologyBox,
   axiom: TerminologyNestingAxiom)
  : Set[java.lang.Throwable] \/ TerminologyNestingAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ TerminologyNestingAxiom](axiom.right) { mo =>
    val parentIC = OMF_MODEL_ENTITY_CONCEPT2Instance(axiom.nestingContext)
    val parentIG = omfModule2Instance(axiom.nestingTerminology)
    val nestedIG = omfModule2Instance(childG)

    for {
      directNestingIRI <- makeMetadataInstanceIRI(mo, "DN", axiom.nestingContext.iri, childG.kindIRI)
      directNestingI = owlDataFactory.getOWLNamedIndividual(directNestingIRI)
      _ <- applyOntologyChangesOrNoOp(
        ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(directNestingI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM,
              directNestingI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_DIRECT_NESTING_PARENT,
              directNestingI, parentIG)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_DIRECT_NESTING_CONTEXT,
              directNestingI, parentIC)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_DIRECT_NESTED_CHILD,
              directNestingI, nestedIG))),
        "createTerminologyGraphDirectNestingAxiom error")
    } yield {

      OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM2Instance += (axiom -> directNestingI)
      axiom
    }
  }

  protected def registerTerminologyGraphDirectExtensionAxiom
  (extendingG: TerminologyBox,
   axiom: TerminologyExtensionAxiom)
  : Set[java.lang.Throwable] \/ TerminologyExtensionAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ TerminologyExtensionAxiom](axiom.right) { mo =>
    val extendingI = omfModule2Instance(extendingG)
    omfModule2Instance
      .get(axiom.extendedTerminology)
      .fold[Set[java.lang.Throwable] \/ TerminologyExtensionAxiom] {
      \/-(axiom)
    } { extendedI =>
      for {
        directImportingIRI <-
        makeMetadataInstanceIRI(mo, "DI", extendingG.kindIRI, axiom.extendedTerminology.kindIRI)

        directImportingI = owlDataFactory.getOWLNamedIndividual(directImportingIRI)
        _ = if (LOG) {
          System.out.println(
            s"""|## createTerminologyGraphDirectExtensionAxiom:
                |extending: ${extendingG.kindIRI}
                |extended: ${axiom.extendedTerminology.kindIRI}
                |result: $directImportingI""".stripMargin
          )
        }
        _ <-
        applyOntologyChangesOrNoOp(ontManager,
          Seq(
            new AddAxiom(mo,
              owlDataFactory.getOWLDeclarationAxiom(directImportingI)),
            new
                AddAxiom(mo,
                  owlDataFactory.getOWLClassAssertionAxiom(
                    OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM,
                    directImportingI)),
            new AddAxiom(mo,
              owlDataFactory.getOWLObjectPropertyAssertionAxiom(
                OMF_HAS_DIRECT_EXTENDED_PARENT,
                directImportingI,
                extendedI)),
            new AddAxiom(mo,
              owlDataFactory.getOWLObjectPropertyAssertionAxiom(
                OMF_HAS_DIRECT_EXTENSIONING_CHILD,
                directImportingI, extendingI))),
          "createTerminologyGraphDirectExtensionAxiom errors")
      } yield {
        OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM2Instance += (axiom -> directImportingI)
        axiom
      }
    }
  }

  def registerEntityConceptDesignationTerminologyGraphAxiom
  (tbox: MutableTerminologyBox,
   axiomT: ConceptDesignationTerminologyAxiom)
  : Set[java.lang.Throwable] \/ ConceptDesignationTerminologyAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ ConceptDesignationTerminologyAxiom](axiomT.right) { mo =>
    val cI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.designatedConcept)
    val gI = omfModule2Instance(axiomT.designatedTerminology)
    for {
      axiomIRI <- makeMetadataInstanceIRI(mo,
        "ConceptDesignationTerminologyGraph",
        tbox.iri,
        axiomT.designatedConcept.iri,
        axiomT.designatedTerminology.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
              omfModule2Instance(tbox), axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_ENTITY_CONCEPT_DESIGNATION_TERMINOLOGY_GRAPH_AXIOM, axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, axiomI, axiomT.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_ENTITY_CONCEPT_DESIGNATION, axiomI, cI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_DESIGNATION_TERMINOLOGY_GRAPH, axiomI, gI))
        ),
        "createOMFEntityConceptDesignationTerminologyGraphAxiomInstance error")
    } yield {
      OMF_ENTITY_CONCEPT_DESIGNATION_TERMINOLOGY_GRAPH_AXIOM2Instance += (axiomT -> axiomI)
      axiomT
    }
  }

  // Bundle Axioms

  protected def registerBundledTerminologyAxiom
  (axiom: BundledTerminologyAxiom,
   terminologyBundle: MutableBundle)
  : Set[java.lang.Throwable] \/ BundledTerminologyAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ BundledTerminologyAxiom](axiom.right) { mo =>
    val tboxBundleI = omfModule2Instance(terminologyBundle)
    val bundledTboxI = omfModule2Instance(axiom.bundledTerminology)
    for {
      directBundleIRI <-
      makeMetadataInstanceIRI(mo, "BT", terminologyBundle.kindIRI, axiom.bundledTerminology.kindIRI)

      bundleI = owlDataFactory.getOWLNamedIndividual(directBundleIRI)
      _ = if (LOG) {
        System.out.println(
          s"""|## createOMFBundledTerminologyAxiom:
              |terminologyBundle: ${terminologyBundle.kindIRI}
              |bundledTerminology: ${axiom.bundledTerminology.kindIRI}
              |result: $bundleI"""
            .stripMargin)
      }
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(bundleI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(
              OMF_BUNDLED_TERMINOLOGY_AXIOM,
              bundleI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(
              OMF_HAS_TERMINOLOGY_BUNDLE,
              bundleI, tboxBundleI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(
              OMF_HAS_BUNDLED_TERMINOLOGY,
              bundleI, bundledTboxI))),
        "createOMFBundledTerminologyAxiom errors")
    } yield {
      OMF_BUNDLED_TERMINOLOGY_AXIOM2Instance += (axiom -> bundleI)
      axiom
    }
  }

  // Identification

  def setTerminologyName
  (tbox: MutableTerminologyBox,
   label: String)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    applyOntologyChangeOrNoOp(ontManager,
      new AddAxiom(mo, owlDataFactory
        .getOWLDataPropertyAssertionAxiom(OMF_HAS_SHORT_NAME,
          omfModule2Instance(tbox),
          owlDataFactory.getOWLLiteral(label))),
      "setTerminologyName error")
  }

  def setTerminologyUUID
  (tbox: MutableTerminologyBox,
   uuid: UUID)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    applyOntologyChangeOrNoOp(ontManager,
      new AddAxiom(mo, owlDataFactory
        .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID,
          omfModule2Instance(tbox),
          owlDataFactory.getOWLLiteral(uuid.toString))),
      "setTerminologyUUID error")
  }

  def setTermName
  (tbox: MutableTerminologyBox,
   termT: types.Term,
   label: String)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    OMF_MODEL_TYPE_TERM2Instance
      .get(termT)
      .fold[Set[java.lang.Throwable] \/ Unit] {
      Set(
        OMFError
          .omfError(s"setTermShortName: no definition for $termT to set label=$label")
      ).left
    } { termI =>
      applyOntologyChange(ontManager,
        new AddAxiom(mo, owlDataFactory
          .getOWLDataPropertyAssertionAxiom(OMF_HAS_SHORT_NAME,
            termI,
            owlDataFactory.getOWLLiteral(label))),
        "setTermShortName error")
    }
  }

  def setTermUUID
  (tbox: MutableTerminologyBox,
   termT: types.Term,
   id: String)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    OMF_MODEL_TYPE_TERM2Instance.get(termT)
      .fold[Set[java.lang.Throwable] \/ Unit] {
      Set(
        OMFError
          .omfError(s"setTermUUID: no definition for $termT to set UUID=$id")
      ).left
    } { termI =>
      applyOntologyChangeOrNoOp(ontManager,
        new AddAxiom(mo, owlDataFactory
          .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID,
            termI,
            owlDataFactory.getOWLLiteral(id))),
        "setTermUUID error")
    }
  }

  def setTermID
  (tbox: MutableTerminologyBox,
   termT: types.Term,
   id: String)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    OMF_MODEL_TYPE_TERM2Instance
      .get(termT)
      .fold[Set[java.lang.Throwable] \/ Unit] {
      Set(OMFError
        .omfError(s"setTermUUID: no definition for $termT to set ID=$id")
      ).left
    } { termI =>
      applyOntologyChangeOrNoOp(ontManager,
        new AddAxiom(mo, owlDataFactory
          .getOWLDataPropertyAssertionAxiom(OMF_HAS_ID,
            termI,
            owlDataFactory.getOWLLiteral(id))),
        "Failed to set a tbox term 'id' data property axiom")
    }
  }

  def setTermURL
  (tbox: MutableTerminologyBox,
   termT: types.Term,
   url: String)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    OMF_MODEL_TYPE_TERM2Instance
      .get(termT)
      .fold[Set[java.lang.Throwable] \/ Unit] {
      Set(OMFError
        .omfError(s"setTermURL: no definition for $termT to set URL=$url")
      ).left
    } { termI =>
      applyOntologyChangeOrNoOp(ontManager,
        new AddAxiom(mo, owlDataFactory
          .getOWLDataPropertyAssertionAxiom(OMF_HAS_URL,
            termI,
            owlDataFactory.getOWLLiteral(url))),
        "Failed to set a tbox term 'url' data property axiom")
    }
  }

  // Terms

  def registerOMFModelEntityAspectInstance
  (tbox: TerminologyBox,
   aspectT: Aspect)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    OMF_MODEL_ENTITY_ASPECT2Instance
      .get(aspectT)
      .fold[types.UnitNES] {
      for {
        aspectIRI <- makeMetadataInstanceIRI(mo, "A", aspectT.iri)
        aspectI = owlDataFactory.getOWLNamedIndividual(aspectIRI)
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(mo, owlDataFactory
              .getOWLDeclarationAxiom(aspectI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLClassAssertionAxiom(OMF_MODEL_ENTITY_ASPECT, aspectI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, aspectI, aspectT.iri.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, aspectI, aspectT.uuid.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_LOCAL_NAME, aspectI, aspectT.name)),
            new AddAxiom(mo, owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_DEFINES_TYPE_TERM,
                omfModule2Instance(tbox),
                aspectI))
          ),
          "Create Aspect error")
      } yield {
        OMF_MODEL_TYPE_TERM2Instance += (aspectT -> aspectI)
        OMF_MODEL_ENTITY_DEFINITION2Instance += (aspectT -> aspectI)
        OMF_MODEL_ENTITY_ASPECT2Instance += (aspectT -> aspectI)
        ()
      }
    }{ _ =>
      types.rightUnitNES
    }
  }

  def registerOMFModelEntityConceptInstance
  (tbox: TerminologyBox,
   conceptT: Concept)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    OMF_MODEL_ENTITY_CONCEPT2Instance
      .get(conceptT)
      .fold[types.UnitNES] {
      for {
        conceptIRI <- makeMetadataInstanceIRI(mo, "C", conceptT.iri)
        conceptI = owlDataFactory.getOWLNamedIndividual(conceptIRI)
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(mo, owlDataFactory
              .getOWLDeclarationAxiom(conceptI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLClassAssertionAxiom(OMF_MODEL_ENTITY_CONCEPT, conceptI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, conceptI, conceptT.iri.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, conceptI, conceptT.uuid.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_LOCAL_NAME, conceptI, conceptT.name)),
            new AddAxiom(mo, owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_DEFINES_TYPE_TERM,
                omfModule2Instance(tbox),
                conceptI))
          ),
          "Create Concept error")
      } yield {
        OMF_MODEL_TYPE_TERM2Instance += (conceptT -> conceptI)
        OMF_MODEL_ENTITY_DEFINITION2Instance += (conceptT -> conceptI)
        OMF_MODEL_ENTITY_CONCEPT2Instance += (conceptT -> conceptI)
        ()
      }
    } {  _ =>
      types.rightUnitNES
    }
  }

  def registerOMFModelEntityReifiedRelationshipInstance
  (tbox: TerminologyBox,
   relationshipT: ReifiedRelationship)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    OMF_MODEL_ENTITY_RELATIONSHIP2Instance
      .get(relationshipT)
      .fold[types.UnitNES] {
      for {
        relationshipIRI <- makeMetadataInstanceIRI(mo, "R", relationshipT.iri)
        relationshipI = owlDataFactory.getOWLNamedIndividual(relationshipIRI)
        sourceI = OMF_MODEL_ENTITY_DEFINITION2Instance(relationshipT.source)
        targetI = OMF_MODEL_ENTITY_DEFINITION2Instance(relationshipT.target)
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(mo, owlDataFactory
              .getOWLDeclarationAxiom(relationshipI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLClassAssertionAxiom(OMF_MODEL_ENTITY_RELATIONSHIP, relationshipI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, relationshipI, relationshipT.iri.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, relationshipI, relationshipT.uuid.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_LOCAL_NAME, relationshipI, relationshipT.name)),
            new AddAxiom(mo, owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_SOURCE, relationshipI, sourceI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_TARGET, relationshipI, targetI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_DEFINES_TYPE_TERM,
                omfModule2Instance(tbox),
                relationshipI))
          ),
          "Create ReifiedRelationship error")
      } yield {
        OMF_MODEL_TYPE_TERM2Instance += (relationshipT -> relationshipI)
        OMF_MODEL_ENTITY_DEFINITION2Instance += (relationshipT -> relationshipI)
        OMF_MODEL_ENTITY_RELATIONSHIP2Instance += (relationshipT -> relationshipI)
        ()
      }
    } { _ =>
      types.rightUnitNES
    }
  }

  def registerOMFModelEntityUnreifiedRelationshipInstance
  (tbox: TerminologyBox,
   relationshipT: UnreifiedRelationship)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    OMF_MODEL_ENTITY_UNREIFIED_RELATIONSHIP2Instance
      .get(relationshipT)
      .fold[types.UnitNES] {
      for {
        relationshipIRI <- makeMetadataInstanceIRI(mo, "U", relationshipT.iri)
        relationshipI = owlDataFactory.getOWLNamedIndividual(relationshipIRI)
        sourceI = OMF_MODEL_ENTITY_DEFINITION2Instance(relationshipT.source)
        targetI = OMF_MODEL_ENTITY_DEFINITION2Instance(relationshipT.target)
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(mo, owlDataFactory
              .getOWLDeclarationAxiom(relationshipI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLClassAssertionAxiom(OMF_MODEL_ENTITY_RELATIONSHIP, relationshipI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, relationshipI, relationshipT.iri.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, relationshipI, relationshipT.uuid.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_LOCAL_NAME, relationshipI, relationshipT.name)),
            new AddAxiom(mo, owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_SOURCE, relationshipI, sourceI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_TARGET, relationshipI, targetI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_DEFINES_TYPE_TERM,
                omfModule2Instance(tbox),
                relationshipI))
          ),
          "Create UnreifiedRelationship error")
      } yield {
        OMF_MODEL_TYPE_TERM2Instance += (relationshipT -> relationshipI)
        OMF_MODEL_ENTITY_UNREIFIED_RELATIONSHIP2Instance += (relationshipT -> relationshipI)
        ()
      }
    } { _ =>
      types.rightUnitNES
    }
  }

  def registerOMFModelScalarDataTypeInstance
  (tbox: TerminologyBox,
   scalarDT: DataRange)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    OMF_MODEL_SCALAR_DATA_TYPE2Instance
      .get(scalarDT)
      .fold[types.UnitNES] {
      for {
        scalarDIRI <- makeMetadataInstanceIRI(mo, "SC", tbox.iri, scalarDT.iri)
        scalarDI = owlDataFactory.getOWLNamedIndividual(scalarDIRI)
        _ <- applyOntologyChangesOrNoOp(ontManager,
          Seq(
            new AddAxiom(mo, owlDataFactory
              .getOWLDeclarationAxiom(scalarDI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLClassAssertionAxiom(OMF_MODEL_SCALAR_DATA_TYPE, scalarDI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, scalarDI, scalarDT.iri.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, scalarDI, scalarDT.uuid.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_LOCAL_NAME, scalarDI, LocalName.unwrap(scalarDT.name))),
            new AddAxiom(mo, owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_DEFINES_TYPE_TERM,
                omfModule2Instance(tbox),
                scalarDI))
          ),
          "Create ScalarDataType error")
      } yield {
        OMF_MODEL_TYPE_TERM2Instance += (scalarDT -> scalarDI)
        OMF_MODEL_DATA_TYPE_DEFINITION2Instance += (scalarDT -> scalarDI)
        OMF_MODEL_SCALAR_DATA_TYPE2Instance += (scalarDT -> scalarDI)
        ()
      }
    } { _ =>
      types.rightUnitNES
    }
  }

  def registerOMFModelStructuredDataTypeInstance
  (tbox: TerminologyBox,
   structuredDT: Structure)
  : types.UnitNES
  = omfMetadata.fold[types.UnitNES](types.rightUnitNES) { mo =>
    OMF_MODEL_STRUCTURED_DATA_TYPE2Instance
      .get(structuredDT)
      .fold[types.UnitNES] {
      for {
        structuredDIRI <- makeMetadataInstanceIRI(mo, "ST", tbox.iri, structuredDT.iri)
        structuredDI = owlDataFactory.getOWLNamedIndividual(structuredDIRI)
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(mo, owlDataFactory
              .getOWLDeclarationAxiom(structuredDI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLClassAssertionAxiom(OMF_MODEL_STRUCTURED_DATA_TYPE, structuredDI)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, structuredDI, structuredDT.iri.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, structuredDI, structuredDT.uuid.toString)),
            new AddAxiom(mo, owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_LOCAL_NAME, structuredDI, structuredDT.name)),
            new AddAxiom(mo, owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_DEFINES_TYPE_TERM,
                omfModule2Instance(tbox),
                structuredDI))
          ),
          "Create StructuredDatatype error")
      } yield {
        OMF_MODEL_TYPE_TERM2Instance += (structuredDT -> structuredDI)
        OMF_MODEL_DATA_TYPE_DEFINITION2Instance += (structuredDT -> structuredDI)
        OMF_MODEL_STRUCTURED_DATA_TYPE2Instance += (structuredDT -> structuredDI)
        ()
      }
    } { _ =>
      types.rightUnitNES
    }
  }

  def registerDataRelationshipFromEntityToScalar
  (tbox: MutableTerminologyBox,
   e2sc: EntityScalarDataProperty)
  : Set[java.lang.Throwable] \/ EntityScalarDataProperty
  = omfMetadata.fold[Set[java.lang.Throwable] \/ EntityScalarDataProperty](e2sc.right) { mo =>
    val entityI = OMF_MODEL_ENTITY_DEFINITION2Instance(e2sc.domain)
    val scalarI = OMF_MODEL_SCALAR_DATA_TYPE2Instance(e2sc.range)
    for {
      termIRI <- makeMetadataInstanceIRI(
        mo,
        "EntityToScalar",
        tbox.iri,
        e2sc.domain.iri,
        e2sc.range.iri)
      termI = owlDataFactory.getOWLNamedIndividual(termIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(termI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR, termI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, termI, e2sc.iri.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, termI, e2sc.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_LOCAL_NAME, termI, e2sc.name)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_MODEL_DATA_RELATIONSHIP_FROM_ENTITY, termI, entityI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_MODEL_DATA_RELATIONSHIP_TO_SCALAR, termI, scalarI))
        ),
        "createDataRelationshipFromEntityToScalar error")
    } yield {
      OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR2Instance += (e2sc -> termI)
      e2sc
    }
  }

  def registerDataRelationshipFromEntityToStructure
  (tbox: MutableTerminologyBox,
   e2sc: EntityStructuredDataProperty)
  : Set[java.lang.Throwable] \/ EntityStructuredDataProperty
  = omfMetadata.fold[Set[java.lang.Throwable] \/ EntityStructuredDataProperty](e2sc.right) { mo =>
    val entityI = OMF_MODEL_ENTITY_DEFINITION2Instance(e2sc.domain)
    val scalarI = OMF_MODEL_STRUCTURED_DATA_TYPE2Instance(e2sc.range)
    for {
      termIRI <- makeMetadataInstanceIRI(
        mo,
        "EntityToStructure",
        tbox.iri,
        e2sc.domain.iri,
        e2sc.range.iri)
      termI = owlDataFactory.getOWLNamedIndividual(termIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(termI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_STRUCTURE, termI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, termI, e2sc.iri.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, termI, e2sc.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_LOCAL_NAME, termI, e2sc.name)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_MODEL_DATA_RELATIONSHIP_FROM_ENTITY, termI, entityI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_MODEL_DATA_RELATIONSHIP_TO_STRUCTURE, termI, scalarI))
        ),
        "createDataRelationshipFromEntityToStructure error")
    } yield {
      OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_STRUCTURE2Instance += (e2sc -> termI)
      e2sc
    }
  }

  def registerDataRelationshipFromStructureToScalar
  (tbox: MutableTerminologyBox,
   e2sc: ScalarDataProperty)
  : Set[java.lang.Throwable] \/ ScalarDataProperty
  = omfMetadata.fold[Set[java.lang.Throwable] \/ ScalarDataProperty](e2sc.right) { mo =>
    val domainI = OMF_MODEL_STRUCTURED_DATA_TYPE2Instance(e2sc.domain)
    val rangeI = OMF_MODEL_SCALAR_DATA_TYPE2Instance(e2sc.range)
    for {
      termIRI <- makeMetadataInstanceIRI(
        mo,
        "StructureToScalar",
        tbox.iri,
        e2sc.domain.iri,
        e2sc.range.iri)
      termI = owlDataFactory.getOWLNamedIndividual(termIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(termI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_SCALAR, termI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, termI, e2sc.iri.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, termI, e2sc.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_LOCAL_NAME, termI, e2sc.name)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE, termI, domainI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_MODEL_DATA_RELATIONSHIP_TO_SCALAR, termI, rangeI))
        ),
        "createDataRelationshipFromStructureToScalar error")
    } yield {
      OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_SCALAR2Instance += (e2sc -> termI)
      e2sc
    }
  }

  def registerDataRelationshipFromStructureToStructure
  (tbox: MutableTerminologyBox,
   e2sc: StructuredDataProperty)
  : Set[java.lang.Throwable] \/ StructuredDataProperty
  = omfMetadata.fold[Set[java.lang.Throwable] \/ StructuredDataProperty](e2sc.right) { mo =>
    val domainI = OMF_MODEL_STRUCTURED_DATA_TYPE2Instance(e2sc.domain)
    val rangeI = OMF_MODEL_STRUCTURED_DATA_TYPE2Instance(e2sc.range)
    for {
      termIRI <- makeMetadataInstanceIRI(
        mo,
        "StructureToStructure",
        tbox.iri,
        e2sc.domain.iri,
        e2sc.range.iri)
      termI = owlDataFactory.getOWLNamedIndividual(termIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(termI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_STRUCTURE, termI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, termI, e2sc.iri.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, termI, e2sc.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_LOCAL_NAME, termI, e2sc.name)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE, termI, domainI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_MODEL_DATA_RELATIONSHIP_TO_STRUCTURE, termI, rangeI))
        ),
        "createDataRelationshipFromStructureToStructure error")
    } yield {
      OMF_MODEL_DATA_RELATIONSHIP_FROM_STRUCTURE_TO_STRUCTURE2Instance += (e2sc -> termI)
      e2sc
    }
  }

  /**
    * Create an OMF EntityDefinitionAspectSubClassAxiom.
    *
    * @param tbox OMF metadata graph where the EntityDefinitionAspectSubClassAxiomI will be added, if it isn't there
    * @param axiomT The OMF axiom to represent in the OMF metadata graph, tbox.
    * @return
    */
  def registerOMFEntityDefinitionAspectSubClassAxiomInstance
  (tbox: TerminologyBox,
   axiomT: AspectSpecializationAxiom)
  : Set[java.lang.Throwable] \/ AspectSpecializationAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ AspectSpecializationAxiom](axiomT.right) { mo =>
    val subI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.sub)
    val supI = OMF_MODEL_ENTITY_ASPECT2Instance(axiomT.sup)
    for {
      axiomIRI <- makeMetadataInstanceIRI(mo, "DefinitionAspectSubClass", tbox.iri, axiomT.sub.iri, axiomT.sup.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
              omfModule2Instance(tbox),
              axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM, axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, axiomI, axiomT.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_GENERAL_ASPECT, axiomI, supI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_SPECIFIC_ENTITY, axiomI, subI))),
        "createOMFEntityDefinitionAspectSubClassAxiomInstance error")
    //    ontManager.applyChange( new AddAxiom(
    //   o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //     OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } yield {
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
  def registerOMFEntityConceptSubClassAxiomInstance
  (tbox: TerminologyBox,
   axiomT: ConceptSpecializationAxiom)
  : Set[java.lang.Throwable] \/ ConceptSpecializationAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ ConceptSpecializationAxiom](axiomT.right) { mo =>
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.sub)
    val supI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.sup)
    for {
      axiomIRI <- makeMetadataInstanceIRI(mo,
        "ConceptSubClass",
        tbox.iri,
        axiomT.sub.iri,
        axiomT.sup.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
              omfModule2Instance(tbox),
              axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM, axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, axiomI, axiomT.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_GENERAL_CONCEPT, axiomI, supI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_SPECIFIC_CONCEPT, axiomI, subI))
          //    ontManager.applyChange( new AddAxiom(
          //   o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
          //    OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
        ),
        "createOMFEntityConceptSubClassAxiomInstance error")
    } yield {
      OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
      axiomT
    }
  }

  /**
    * Create an OMF EntityDefinitionUniversalRestrictionAxiom.
    *
    * @param tbox OMF metadata graph where the EntityDefinitionUniversalRestrictionAxiom will be added, if it isn't there
    * @param axiomT The OMF axiom to represent in the OMF metadata graph, tbox.
    * @return
    */
  def registerOMFEntityDefinitionUniversalRestrictionAxiomInstance
  (tbox: TerminologyBox,
   axiomT: EntityUniversalRestrictionAxiom)
  : Set[java.lang.Throwable] \/ EntityUniversalRestrictionAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ EntityUniversalRestrictionAxiom](axiomT.right) { mo =>
    val subI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.restrictedDomain)
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.restrictedRelation)
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.restrictedRange)
    for {
      axiomIRI <- makeMetadataInstanceIRI(mo,
        "UniversalDefinitionRestriction",
        tbox.iri,
        axiomT.restrictedDomain.iri,
        axiomT.restrictedRelation.iri,
        axiomT.restrictedRange.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
              omfModule2Instance(tbox), axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_ENTITY_DEFINITION_UNIVERSAL_RESTRICTION_AXIOM, axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, axiomI, axiomT.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_ENTITY_DOMAIN, axiomI, subI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_RESTRICTS_RELATIONSHIP, axiomI, relI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_RANGE, axiomI, rangeI))),
        "createOMFEntityDefinitionUniversalRestrictionAxiomInstance error")
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //    OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } yield {
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
  def registerOMFEntityDefinitionExistentialRestrictionAxiomInstance
  (tbox: TerminologyBox,
   axiomT: EntityExistentialRestrictionAxiom)
  : Set[java.lang.Throwable] \/ EntityExistentialRestrictionAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ EntityExistentialRestrictionAxiom](axiomT.right) { mo =>
    val subI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.restrictedDomain)
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.restrictedRelation)
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.restrictedRange)
    for {
      axiomIRI <- makeMetadataInstanceIRI(mo,
        "ExistentialDefinitionRestriction",
        tbox.iri,
        axiomT.restrictedDomain.iri,
        axiomT.restrictedRelation.iri,
        axiomT.restrictedRange.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
              omfModule2Instance(tbox), axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_ENTITY_DEFINITION_EXISTENTIAL_RESTRICTION_AXIOM, axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, axiomI, axiomT.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_ENTITY_DOMAIN, axiomI, subI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_RESTRICTS_RELATIONSHIP, axiomI, relI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_RANGE, axiomI, rangeI))),
        "createOMFEntityDefinitionExistentialRestrictionAxiomInstance error")
    //    ontManager.applyChange( new AddAxiom(
    //    o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //      OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } yield {
      OMF_ENTITY_DEFINITION_EXISTENTIAL_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
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
  def registerOMFEntityReifiedRelationshipSubClassAxiomInstance
  (tbox: TerminologyBox,
   axiomT: ReifiedRelationshipSpecializationAxiom)
  : Set[java.lang.Throwable] \/ ReifiedRelationshipSpecializationAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ ReifiedRelationshipSpecializationAxiom](axiomT.right) { mo =>
    val subI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.sub)
    val supI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.sup)
    for {
      axiomIRI <- makeMetadataInstanceIRI(mo,
        "RelationshipSubClass",
        tbox.iri,
        axiomT.sub.iri,
        axiomT.sup.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_DIRECTLY_ASSERTS_AXIOM,
              omfModule2Instance(tbox),
              axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM, axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_GENERAL_REIFIED_RELATIONSHIP,
              axiomI, supI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_SPECIFIC_REIFIED_RELATIONSHIP,
              axiomI, subI))),
        "createOMFEntityReifiedRelationshipSubClassAxiomInstance error")
    //    ontManager.applyChange( new AddAxiom(
    //    o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //      OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } yield {
      OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
      axiomT
    }
  }

  def registerOMFEntityScalarDataPropertyExistentialRestrictionAxiomInstance
  (tbox: TerminologyBox,
   axiomT: types.termAxioms.EntityScalarDataPropertyExistentialRestrictionAxiom)
  : Set[java.lang.Throwable] \/ types.termAxioms.EntityScalarDataPropertyExistentialRestrictionAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ types.termAxioms.EntityScalarDataPropertyExistentialRestrictionAxiom](axiomT.right) { mo =>
    val entityI = OMF_MODEL_TYPE_TERM2Instance(axiomT.restrictedEntity)
    val dpropI = OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR2Instance(axiomT.scalarProperty)
    val rangeI = OMF_MODEL_DATA_TYPE_DEFINITION2Instance(axiomT.scalarRestriction)
    for {
      axiomIRI <- makeMetadataInstanceIRI(
        mo,
        "EntityScalarDataPropertyExistentialRestrictionAxiom",
        tbox.iri,
        axiomT.restrictedEntity.iri,
        axiomT.scalarProperty.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR,
              axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, axiomI, axiomT.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_ENTITY_DOMAIN, axiomI, entityI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTING_SCALAR_DATA_RELATIONSHIP, axiomI, dpropI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_RANGE, axiomI, rangeI))
        ),
        "createOMFEntityScalarDataPropertyExistentialRestrictionAxiomInstance error")
    } yield {
      OMF_ENTITY_SCALAR_DATA_PROPERTY_EXISTENTIAL_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
      axiomT
    }
  }

  def registerOMFEntityScalarDataPropertyUniversalRestrictionAxiomInstance
  (tbox: TerminologyBox,
   axiomT: types.termAxioms.EntityScalarDataPropertyUniversalRestrictionAxiom)
  : Set[java.lang.Throwable] \/ types.termAxioms.EntityScalarDataPropertyUniversalRestrictionAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ types.termAxioms.EntityScalarDataPropertyUniversalRestrictionAxiom](axiomT.right) { mo =>
    val entityI = OMF_MODEL_TYPE_TERM2Instance(axiomT.restrictedEntity)
    val dpropI = OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR2Instance(axiomT.scalarProperty)
    val rangeI = OMF_MODEL_DATA_TYPE_DEFINITION2Instance(axiomT.scalarRestriction)
    for {
      axiomIRI <- makeMetadataInstanceIRI(
        mo,
        "EntityScalarDataPropertyUniversalRestrictionAxiom",
        tbox.iri,
        axiomT.restrictedEntity.iri,
        axiomT.scalarProperty.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR,
              axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, axiomI, axiomT.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_ENTITY_DOMAIN, axiomI, entityI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTING_SCALAR_DATA_RELATIONSHIP, axiomI, dpropI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_RANGE, axiomI, rangeI))
        ),
        "createOMFEntityScalarDataPropertyUniversalRestrictionAxiomInstance error")
    } yield {
      OMF_ENTITY_SCALAR_DATA_PROPERTY_UNIVERSAL_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
      axiomT
    }
  }

  def registerOMFEntityScalarDataPropertyParticularRestrictionAxiomInstance
  (tbox: TerminologyBox,
   axiomT: types.termAxioms.EntityScalarDataPropertyParticularRestrictionAxiom)
  : Set[java.lang.Throwable] \/ types.termAxioms.EntityScalarDataPropertyParticularRestrictionAxiom
  = omfMetadata.fold[Set[java.lang.Throwable] \/ types.termAxioms.EntityScalarDataPropertyParticularRestrictionAxiom](axiomT.right) { mo =>
    val entityI = OMF_MODEL_TYPE_TERM2Instance(axiomT.restrictedEntity)
    val dpropI = OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR2Instance(axiomT.scalarProperty)
    for {
      axiomIRI <- makeMetadataInstanceIRI(
        mo,
        "EntityScalarDataPropertyParticularRestrictionAxiom",
        tbox.iri,
        axiomT.restrictedEntity.iri,
        axiomT.scalarProperty.iri)
      axiomI = owlDataFactory.getOWLNamedIndividual(axiomIRI)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(mo, owlDataFactory
            .getOWLDeclarationAxiom(axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLClassAssertionAxiom(OMF_MODEL_DATA_RELATIONSHIP_FROM_ENTITY_TO_SCALAR,
              axiomI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, axiomI, axiomT.uuid.toString)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_ENTITY_DOMAIN, axiomI, entityI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTING_SCALAR_DATA_RELATIONSHIP, axiomI, dpropI)),
          new AddAxiom(mo, owlDataFactory
            .getOWLDataPropertyAssertionAxiom(OMF_HAS_LITERAL_RESTRICTION, axiomI,
              LiteralConversions.toOWLLiteral(axiomT.literalValue, owlDataFactory)))
        ),
        "createOMFEntityScalarDataPropertyParticularRestrictionAxiomInstance error")
    } yield {
      OMF_ENTITY_SCALAR_DATA_PROPERTY_PARTICULAR_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
      axiomT
    }
  }

}
