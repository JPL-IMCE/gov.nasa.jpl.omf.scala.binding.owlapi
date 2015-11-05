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
package gov.nasa.jpl.omf.scala.binding.owlapi

import java.lang.System

import gov.nasa.jpl.omf.scala.binding.owlapi.types.ResolverHelper
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.{Imports, _}
import org.semanticweb.owlapi.util.PriorityCollection

import scala.collection.immutable._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.reflect.internal.FatalError
import scala.util.control.Exception._
import scala.{Boolean,Option,None,Some,StringContext,Unit}
import scala.Predef.{Set=>_,Map=>_,_}
import scalaz._, Scalaz._

case class OWLAPIOMFGraphStore(omfModule: OWLAPIOMFModule, ontManager: OWLOntologyManager) {

  require(null != omfModule)
  require(null != ontManager)

  val LOG: Boolean = true

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
    .getClassesInSignature(Imports.EXCLUDED)
    .map { c => c.getIRI.getRemainder.get -> c }
    .toMap

  protected lazy val omfModelObjectPropertiesMap =
    omfModelOntology
    .getObjectPropertiesInSignature(Imports.EXCLUDED)
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
    .getDatatypesInSignature(Imports.EXCLUDED)
    .map { dt => dt.getIRI.getRemainder.get -> dt }
    .toMap

  protected lazy val omfModelDataProperties =
    omfModelOntology
    .getDataPropertiesInSignature(Imports.EXCLUDED)
    .map { dp => dp.getIRI.getRemainder.get -> dp }
    .toMap

  protected lazy val omfNamedIndividuals =
    omfModelOntology
    .getIndividualsInSignature(Imports.EXCLUDED)
    .map { dp => dp.getIRI.getRemainder.get -> dp }
    .toMap

  protected lazy val allAnnotationProperties =
    omfModelOntology
    .getAnnotationPropertiesInSignature(Imports.INCLUDED)
    .map { ap => ap.getIRI.getShortForm -> ap }
    .toMap

  lazy val RDFS_LABEL: OWLAnnotationProperty = ontManager.getOWLDataFactory.getRDFSLabel

  lazy val ANNOTATION_HAS_UUID: OWLAnnotationProperty =
    ontManager
    .getOWLDataFactory
    .getOWLAnnotationProperty(omfModule.ops.OMF_TBox_DataProperty_HasUUID)

  // OMF Metadata.

  protected var omfMetadata: Option[OWLOntology] = None

  def setOMFMetadataOntology(o: OWLOntology): Unit =
    omfMetadata = Some(o)

  // OMF model.

  // ModelTermAxiom
  lazy val OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM =
    omfModelClasses("EntityConceptExistentialRestrictionAxiom")
  protected val OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityConceptExistentialRestrictionAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM =
    omfModelClasses("EntityConceptUniversalRestrictionAxiom")
  protected val OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityConceptUniversalRestrictionAxiom, OWLNamedIndividual]()

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
  lazy val OMF_RESTRICTS_CONCEPT = omfModelObjectProperties("restrictsConcept")
  lazy val OMF_RESTRICTS_RELATIONSHIP = omfModelObjectProperties("restrictsRelationship")

  lazy val OMF_HAS_SOURCE = omfModelObjectProperties("hasSource")
  lazy val OMF_HAS_TARGET = omfModelObjectProperties("hasTarget")

  lazy val OMF_HAS_TERMINOLOGY_KIND = omfModelObjectProperties("hasTerminologyKind")

  lazy val OMF_HAS_DIRECT_EXTENDED_PARENT = omfModelObjectProperties("hasDirectExtendedParent")
  lazy val OMF_HAS_DIRECT_EXTENSIONING_CHILD = omfModelObjectProperties("hasDirectExtendingChild")

  lazy val OMF_HAS_DIRECT_NESTING = omfModelObjectProperties("hasDirectNestingParent")
  lazy val OMF_HAS_DIRECT_NESTED_CHILD = omfModelObjectProperties("hasDirectNestedChild")

  lazy val OMF_HAS_DESIGNATION_TERMINOLOGY_GRAPH = omfModelObjectProperties("hasDesignationTerminologyGraph")
  lazy val OMF_HAS_ENTITY_CONCEPT_DESIGNATION = omfModelObjectProperties("hasEntityConceptDesignation")

  lazy val OMF_HAS_GENERAL_SCALAR_DATATYPE = omfModelObjectProperties("hasGeneralScalarDataType")
  lazy val OMF_HAS_SPECIFIC_SCALAR_DATATYPE = omfModelObjectProperties("hasSpecificScalarDataType")

  // Named Individuals
  lazy val OMF_TOPLEVEL_DEFINITION_TBOX = omfNamedIndividuals("ToplevelDefinitionTBox")
  lazy val OMF_DEFINITION_TBOX = omfNamedIndividuals("DefinitionTBox")
  lazy val OMF_TOPLEVEL_DESIGNATION_TBOX = omfNamedIndividuals("ToplevelDesignationTBox")
  lazy val OMF_DESIGNATION_TBOX = omfNamedIndividuals("DesignationTBox")

  // Data Properties
  lazy val OMF_HAS_IRI = omfModelDataProperties("hasIRI")
  lazy val OMF_HAS_RELATIVE_IRI_PATH = omfModelDataProperties( "hasRelativeIRIPath" )
  lazy val OMF_HAS_SHORT_NAME = omfModelDataProperties("hasShortName")
  lazy val OMF_HAS_UUID = omfModelDataProperties("hasUUID")
  lazy val OMF_IS_ABSTRACT = omfModelDataProperties("isAbstract")
  lazy val OMF_IS_ASYMMETRIC = omfModelDataProperties("isAsymmetric")
  lazy val OMF_IS_FUNCTIONAL = omfModelDataProperties("isFunctional")
  lazy val OMF_IS_INVERSE_FUNCTIONAL = omfModelDataProperties("isInverseFunctional")
  lazy val OMF_IS_IRREFLEXIVE = omfModelDataProperties("isIrreflexive")
  lazy val OMF_IS_REFLEXIVE = omfModelDataProperties("isReflexive")
  lazy val OMF_IS_SYMMETRIC = omfModelDataProperties("isSymmetric")
  lazy val OMF_IS_TRANSITIVE = omfModelDataProperties("isTransitive")

  protected val immutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, types.ImmutableModelTerminologyGraph]()
  protected val mutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, types.MutableModelTerminologyGraph]()

  protected val directExtensionAxioms = scala.collection.mutable.HashSet[types.TerminologyGraphDirectExtensionAxiom]()
  protected val directNestingAxioms = scala.collection.mutable.HashSet[types.TerminologyGraphDirectNestingAxiom]()

  protected val extendingChild2ExtendedParents =
    scala.collection.mutable.HashMap[
      types.ModelTerminologyGraph,
      scala.collection.mutable.HashSet[types.ModelTerminologyGraph]]().
    withDefaultValue(scala.collection.mutable.HashSet[types.ModelTerminologyGraph]())

  protected val nestedChild2NestingParent =
    scala.collection.mutable.HashMap[types.ModelTerminologyGraph, types.ModelTerminologyGraph]()

  protected val nestingParent2NestedChildren =
    scala.collection.mutable.HashMap[
      types.ModelTerminologyGraph,
      scala.collection.mutable.HashSet[types.ModelTerminologyGraph]]().
    withDefaultValue(scala.collection.mutable.HashSet[types.ModelTerminologyGraph]())

  def createTerminologyGraphDirectNestingAxiom
  (parentG: types.ModelTerminologyGraph,
   childG: types.ModelTerminologyGraph)
  : NonEmptyList[java.lang.Throwable] \/ types.TerminologyGraphDirectNestingAxiom = {
    val nestedChildren = nestingParent2NestedChildren
                         .getOrElseUpdate(parentG,
                                          scala.collection.mutable.HashSet[types.ModelTerminologyGraph]())
    val axiom = directNestingAxioms.find { ax =>
      ax.nestedChild.kindIRI == childG.kindIRI &&
      ax.nestingParent.kindIRI == parentG.kindIRI
                                         }
    (axiom, nestedChild2NestingParent.get(childG)) match {
      case (None, Some(_))                 =>
        NonEmptyList(
          OMFError
          .omfOpsError(ops, s"createTerminologyGraphDirectNestingAxiom inconsistency")
        ).left
      case (Some(_), None)                 =>
        NonEmptyList(
          OMFError
            .omfOpsError(ops, s"createTerminologyGraphDirectNestingAxiom inconsistency")
        ).left
      case (Some(ax), Some(nestingParent)) =>
        if (nestingParent.kindIRI != parentG.kindIRI)
          NonEmptyList(
            OMFError
              .omfOpsError(ops, s"createTerminologyGraphDirectNestingAxiom inconsistency")
          ).left
        else
          \/-(ax)
      case (None, None)                    =>
        val axiom = types.TerminologyGraphDirectNestingAxiom(
                                                              nestedChild = childG,
                                                              nestingParent = parentG)
        val parentI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(parentG)
        val nestedI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(childG)
        for {
          directNestingIRI <-
            makeMetadataInstanceIRI(omfMetadata.get, "DN", childG.kindIRI, parentG.kindIRI)
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
                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_DIRECT_NESTING,
                    directNestingI, parentI)),
              new AddAxiom(omfMetadata.get,
                owlDataFactory
                  .getOWLObjectPropertyAssertionAxiom(OMF_HAS_DIRECT_NESTED_CHILD,
                    directNestingI, nestedI)))
          } {
            val result = ontManager.applyChange(change)
            require(
              result == ChangeApplied.SUCCESSFULLY,
              s"\ncreateTerminologyGraphDirectNestingAxiom:\n$change")
          }

          for {
            added <- Seq(
              directNestingAxioms.add(axiom),
              nestedChild2NestingParent.put(childG, parentG).isEmpty,
              nestedChildren.add(childG))
          } require(added)
          OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM2Instance += (axiom -> directNestingI)
          axiom
        }
    }
  }

  def createTerminologyGraphDirectExtensionAxiom
  (extendingG: types.ModelTerminologyGraph,
   extendedG: types.ModelTerminologyGraph)
  : NonEmptyList[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom = {
    val extendedParents =
      extendingChild2ExtendedParents
      .getOrElseUpdate(extendingG,
                       scala.collection.mutable.HashSet[types.ModelTerminologyGraph]())
    if (extendedParents.contains(extendedG))
      directExtensionAxioms.find { ax =>
        ax.extendingChild.kindIRI == extendingG.kindIRI &&
        ax.extendedParent.kindIRI == extendedG.kindIRI
      }
      .fold[NonEmptyList[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom]({
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
        NonEmptyList(
          OMFError
            .omfOpsError(ops, "Duplicate TerminologyGraphDirectExtensionAxiom not in directExtensionAxioms")
        ).left
      }){ ax =>
        \/-(ax)
      }
    else {

      val axiom = types
                  .TerminologyGraphDirectExtensionAxiom(extendingChild = extendingG,
                                                        extendedParent = extendedG)

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
                                      owlDataFactory.getOWLDeclarationAxiom
                                      (
                                        directImportingI)),
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .getOWLClassAssertionAxiom(
                                        OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM,
                                                                 directImportingI)),
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .getOWLObjectPropertyAssertionAxiom
                                      (
                                        OMF_HAS_DIRECT_EXTENDED_PARENT,
                                        directImportingI, extendedI)),
                         new AddAxiom(omfMetadata.get,
                                      owlDataFactory
                                      .
                                        getOWLObjectPropertyAssertionAxiom(OMF_HAS_DIRECT_EXTENSIONING_CHILD,
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
  }

  def getNestingGraph
  (g: types.ModelTerminologyGraph)
  : Option[types.ModelTerminologyGraph] =
    nestedChild2NestingParent.get(g)

  def getNestedGraphs
  (g: types.ModelTerminologyGraph)
  : Iterable[types.ModelTerminologyGraph] =
    nestingParent2NestedChildren(g).to[Iterable]

  def fromTerminologyGraph
  (g: types.ModelTerminologyGraph)
  : OWLAPITerminologyGraphSignature =
    g.fromTerminologyGraph(
                            nestedChild2NestingParent.get(g),
                            nestingParent2NestedChildren(g).to[Iterable],
                            extendingChild2ExtendedParents(g).to[Iterable])

  // OMF Ontology Instance Model Constructors  

  val owlDataFactory = ontManager.getOWLDataFactory

  val mDigest = java.security.MessageDigest.getInstance("SHA-256")

  def hashMessage(message: String): String =
    mDigest.digest(message.getBytes("UTF-8")).map("%02x".format(_)).mkString

  def makeMetadataInstanceIRI
  (o: OWLOntology, instanceKind: String, iri: IRI*)
  : NonEmptyList[java.lang.Throwable] \/ IRI = {
    require(iri.nonEmpty)
    omfModule.ops.withFragment(
                                o.getOntologyID.getOntologyIRI.get,
                                instanceKind + "-" + hashMessage(iri.mkString("", ",", "")))
  }

  def createOMFModelTerminologyGraph
  (o: OWLOntology,
   iri: IRI,
   relativeIRIPath: Option[String],
   tboxOnt: OWLOntology,
   kind: TerminologyKind.TerminologyKind)
  : NonEmptyList[java.lang.Throwable] \/ types.MutableModelTerminologyGraph =
    Backbone
      .createBackbone(tboxOnt, kind, ops)
      .flatMap { backbone =>
    val graphT = new types.MutableModelTerminologyGraph(kind = kind, ont = tboxOnt, backbone = backbone)
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
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_TERMINOLOGY_KIND, graphI, okind)),
          new AddAxiom(o,
            owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, graphI, graphT.kindIRI.toString))
        ) ++ relativeIRIPath.fold[Seq[OWLOntologyChange]](
          Seq()
        ){ _relativeIRIPath =>
          Seq(
            new AddAxiom(o,
              owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_RELATIVE_IRI_PATH, graphI, _relativeIRIPath))
          )
        }
      } {
        val result = ontManager.applyChange(change)
        require(
          result == ChangeApplied.SUCCESSFULLY,
          s"\ncreateOMFModelTerminologyGraph:\n$change")
      }
      mutableTBoxGraphs.put(iri, graphT)
      OMF_MODEL_TERMINOLOGY_GRAPH2Instance += (graphT -> graphI)
      graphT
    }
  }

  def setTerminologyGraphShortName
  (tbox: types.ModelTerminologyGraph,
   label: String)
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          NonEmptyList(
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
            result == ChangeApplied.SUCCESSFULLY,
            s"\nsetTerminologyGraphShortName:\n$change")
        }
        \/-(())
      })


  def setTerminologyGraphUUID
  (tbox: types.ModelTerminologyGraph,
   id: String)
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          NonEmptyList(
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
            result == ChangeApplied.SUCCESSFULLY,
            s"\nsetTerminologyGraphUUID:\n$change")
        }
        \/-(())
      })

  def setTermShortName
  (tbox: types.MutableModelTerminologyGraph,
   termT: types.ModelTypeTerm,
   label: String)
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    OMF_MODEL_TYPE_TERM2Instance.get(termT)
    .fold[NonEmptyList[java.lang.Throwable] \/ Unit]{
      NonEmptyList(
        OMFError
        .omfError(s"setTermShortName: no definition for $termT to set label=$label")
      ).left
     }{ termI =>
      nonFatalCatch[Unit]
        .withApply {
          (cause: java.lang.Throwable) =>
            NonEmptyList(
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
              result == ChangeApplied.SUCCESSFULLY,
              s"\nsetTermShortName:\n$change")
          }
          \/-(())
        })
    }

  def setTermUUID
  (tbox: types.MutableModelTerminologyGraph,
   termT: types.ModelTypeTerm,
   id: String)
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    OMF_MODEL_TYPE_TERM2Instance.get(termT)
    .fold[NonEmptyList[java.lang.Throwable] \/ Unit]{
      NonEmptyList(
        OMFError
          .omfError(s"setTermUUID: no definition for $termT to set UUID=$id")
      ).left
    } { termI =>
      nonFatalCatch[Unit]
        .withApply {
          (cause: java.lang.Throwable) =>
            NonEmptyList(
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
              result == ChangeApplied.SUCCESSFULLY,
              s"\nsetTermUUID:\n$change")
          }
          \/-(())
        })
    }

  def createOMFModelEntityAspectInstance
  (tbox: types.ModelTerminologyGraph,
   aspectT: types.ModelEntityAspect)
  : NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual =
    OMF_MODEL_ENTITY_ASPECT2Instance.get(aspectT)
    .fold[NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual] {
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
  : NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual =
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
  : NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual =
    OMF_MODEL_ENTITY_CONCEPT2Instance
    .get(conceptT)
    .fold[NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual]{
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
  : NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual =
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
  : NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual =
    OMF_MODEL_ENTITY_RELATIONSHIP2Instance
    .get(relationshipT)
      .fold[NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual]{
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
  : NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual =
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
  : NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual =
    OMF_MODEL_SCALAR_DATA_TYPE2Instance
      .get(scalarDT)
      .fold[NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual]{
      for {
        scalarDIRI <- makeMetadataInstanceIRI(omfMetadata.get, "SC", tbox.kindIRI, scalarDT.iri)
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
  : NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual =
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
  : NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual =
    OMF_MODEL_STRUCTURED_DATA_TYPE2Instance
      .get(structuredDT)
      .fold[NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual]{
      for {
        structuredDIRI <- makeMetadataInstanceIRI(omfMetadata.get, "ST", tbox.kindIRI, structuredDT.iri)
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
  : NonEmptyList[java.lang.Throwable] \/ OWLNamedIndividual =
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

  def createOMFEntityDefinitionAspectSubClassAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityDefinitionAspectSubClassAxiom)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityDefinitionAspectSubClassAxiom = {
    val subI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.sub)
    val supI = OMF_MODEL_ENTITY_ASPECT2Instance(axiomT.sup)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get, "DefinitionAspectSubClass", tbox.kindIRI, axiomT.sub.iri, axiomT.sup.iri)
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
               result == ChangeApplied.SUCCESSFULLY,
               s"\ncreateOMFEntityDefinitionAspectSubClassAxiomInstance:\n$change")
    }
    OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  def createOMFEntityConceptSubClassAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityConceptSubClassAxiom)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityConceptSubClassAxiom = {
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.sub)
    val supI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.sup)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
                                                                "ConceptSubClass",
                                                                tbox.kindIRI,
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
               result == ChangeApplied.SUCCESSFULLY,
               s"\ncreateOMFEntityConceptSubClassAxiomInstance:\n$change")
    }
    OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  def createOMFEntityConceptUniversalRestrictionAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityConceptUniversalRestrictionAxiom)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityConceptUniversalRestrictionAxiom = {
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.sub)
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.rel)
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.range)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
                                                                "UniversalConceptRestriction",
                                                                tbox.kindIRI,
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
                                  .getOWLClassAssertionAxiom(OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM, axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_RESTRICTS_CONCEPT, axiomI, subI)),
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
               result == ChangeApplied.SUCCESSFULLY,
               s"\ncreateOMFEntityConceptUniversalRestrictionAxiomInstance:\n$change")
    }
    OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  def createOMFEntityConceptExistentialRestrictionAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityConceptExistentialRestrictionAxiom)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityConceptExistentialRestrictionAxiom = {
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.sub)
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.rel)
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.range)
    for {
      axiomIRI <-makeMetadataInstanceIRI(omfMetadata.get,
                                                                "ExistentialConceptRestriction",
                                                                tbox.kindIRI,
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
                                  .getOWLClassAssertionAxiom(OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM, axiomI)),
                     new AddAxiom(omfMetadata.get,
                                  owlDataFactory
                                  .getOWLObjectPropertyAssertionAxiom(OMF_RESTRICTS_CONCEPT, axiomI, subI)),
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
               result == ChangeApplied.SUCCESSFULLY,
               s"\ncreateOMFEntityConceptExistentialRestrictionAxiomInstance:\n$change")
    }
    OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  def createOMFEntityConceptDesignationTerminologyGraphAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityConceptDesignationTerminologyGraphAxiom)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityConceptDesignationTerminologyGraphAxiom = {
    val cI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.entityConceptDesignation)
    val gI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(axiomT.designationTerminologyGraph)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
                                                                "ConceptDesignationTerminologyGraph",
                                                                tbox.kindIRI,
                                                                axiomT.entityConceptDesignation.iri,
                                                                axiomT.designationTerminologyGraph.kindIRI)
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

  def createOMFEntityReifiedRelationshipSubClassAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.EntityReifiedRelationshipSubClassAxiom)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityReifiedRelationshipSubClassAxiom = {
    val subI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.sub)
    val supI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(axiomT.sup)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
                                                                "RelationshipSubClass",
                                                                tbox.kindIRI,
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
               result == ChangeApplied.SUCCESSFULLY,
               s"\ncreateOMFEntityReifiedRelationshipSubClassAxiomInstance:\n$change")
    }
    OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  def createOMFScalarDataTypeFacetRestrictionAxiomInstance
  (tbox: types.ModelTerminologyGraph,
   axiomT: types.ScalarDataTypeFacetRestrictionAxiom)
  : NonEmptyList[java.lang.Throwable] \/ types.ScalarDataTypeFacetRestrictionAxiom = {
    val subI = OMF_MODEL_SCALAR_DATA_TYPE2Instance(axiomT.sub)
    val supI = OMF_MODEL_SCALAR_DATA_TYPE2Instance(axiomT.sup)
    for {
      axiomIRI <- makeMetadataInstanceIRI(omfMetadata.get,
                                                                "ScalarDataTypeFacetRestriction",
                                                                tbox.kindIRI,
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
                       .getOWLObjectPropertyAssertionAxiom(OMF_HAS_GENERAL_SCALAR_DATATYPE,
                                                           axiomI, supI)),
          new AddAxiom(omfMetadata.get,
                       owlDataFactory
                       .getOWLObjectPropertyAssertionAxiom(OMF_HAS_SPECIFIC_SCALAR_DATATYPE,
                                                           axiomI, subI)))
    } {
      val result = ontManager.applyChange(change)
      require(
               result == ChangeApplied.SUCCESSFULLY,
               s"\ncreateOMFScalarDataTypeFacetRestrictionAxiomInstance:\n$change")
    }
    OMF_SCALAR_DATA_TYPE_FACET_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
    axiomT
    }
  }

  // OMF API
  def asImmutableTerminologyGraph
  (g: types.MutableModelTerminologyGraph)
  : NonEmptyList[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap) = {

    def convert1
    (acc: types.Mutable2IMutableTerminologyMap,
     mg: types.MutableModelTerminologyGraph)
    : NonEmptyList[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap = {
      System.out
      .println(s"convert1: acc=${acc.size}, m=${mg.kindIRI}")
      for {
        (m, i) <- acc
      } {
        System.out.println(s"acc.m: ${m.kindIRI}")
        System.out.println(s"acc.i: ${i.kindIRI}")
      }

      require(!acc.contains(mg), s"convert1: acc=${acc.size}, m=${mg.kindIRI}")
      val tgraph = fromTerminologyGraph(mg)
      val tn0: NonEmptyList[java.lang.Throwable] \/ List[types.ImmutableModelTerminologyGraph] =
        List().right
      val tnN: NonEmptyList[java.lang.Throwable] \/ List[types.ImmutableModelTerminologyGraph] =
        (tn0 /: tgraph.nested) { (tni, ni) =>
          tni +++
          (ni match {
            case g: types.ImmutableModelTerminologyGraph =>
              List(g)
              .right
            case g: types.MutableModelTerminologyGraph   =>
              acc
                .get(g)
                .fold[NonEmptyList[java.lang.Throwable] \/ List[types.ImmutableModelTerminologyGraph]](
                NonEmptyList(
                  OMFError.omfError(
                       s"""No Immutable graph available for a nested mutable graph:
                           |mutable graph to convert:
                           |$mg
                           |nested mutable graph that should have been converted:
                           |$g
                           |""".stripMargin)
                ).left
              ){ g =>
                List(g)
                .right
              }
          })
        }

      val ti0: NonEmptyList[java.lang.Throwable] \/ List[types.ImmutableModelTerminologyGraph] =
        List().right
      val tiN: NonEmptyList[java.lang.Throwable] \/ List[types.ImmutableModelTerminologyGraph] =
        (ti0 /: tgraph.imports) { (tii, ni) =>
          tii +++
            (ni match {
              case g: types.ImmutableModelTerminologyGraph =>
                List(g)
                  .right
              case g: types.MutableModelTerminologyGraph =>
                acc
                  .get(g)
                  .fold[NonEmptyList[java.lang.Throwable] \/ List[types.ImmutableModelTerminologyGraph]](
                  NonEmptyList(
                    OMFError.omfError(
                      s"""No Immutable graph available for an imported mutable graph:
                           |mutable graph to convert:
                           |$mg
                           |imported mutable graph that should have been converted:
                           |$g

                           |""".
                        stripMargin)
                   ).left
                 ){ g =>
                   List(g)
                   .right
                 }
             })
          }

      tnN.flatMap { ns =>
        tiN.flatMap { is =>

          val itgraph = tgraph.copy(nested = ns, imports = is)
          val ig =
            types
              .ImmutableModelTerminologyGraph(
                tgraph.kind,
                mg.ont,
                tgraph.aspects.toList,
                tgraph.concepts.toList,
                tgraph.reifiedRelationships.toList,
                tgraph.unreifiedRelationships.toList,
                tgraph.scalarDataTypes.toList,
                tgraph.structuredDataTypes.toList,
                tgraph.entity2scalarDataRelationships.toList,
                tgraph.entity2structureDataRelationships.toList,
                tgraph.structure2scalarDataRelationships.toList,
                tgraph.structure2structureDataRelationships.toList,
                tgraph.axioms.toList)(mg.ops)

          System.out
            .println(
              s"""### Mutable TBox:
                        |${mg.kindIRI}
                        |#-- Immutable TBox:
                        |${ig.kindIRI}
                        |#---
                        |""".stripMargin)
          val m2i: types.Mutable2IMutableTerminologyMap = Map(mg -> ig) ++ acc
          register(ig, itgraph, m2i, mg.getTerminologyGraphShortName, mg.getTerminologyGraphUUID)
        }
      }
    }

    def convert
    (acc: types.Mutable2IMutableTerminologyMap,
     queue: Seq[types.MutableModelTerminologyGraph],
     visited: Seq[types.MutableModelTerminologyGraph])
    : NonEmptyList[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap = {
      System.out
      .println(s"convert: acc=${acc.size}, queue=${queue.size}, visited=${visited.size}")
      for {
        (m, i) <- acc
      } {
        System.out.println(s"acc.m: ${m.kindIRI}")
        System.out.println(s"acc.i: ${i.kindIRI}")
      }
      for {qm <- queue} {
        System.out.println(s"q.m: ${qm.kindIRI}")
      }
      for {vm <- visited} {
        System.out.println(s"v.m: ${vm.kindIRI}")
      }
      if (queue.isEmpty) {
        if (visited.isEmpty)
          \/-(acc)
        else if (acc.contains(visited.head))
          convert(acc, Seq(), visited.tail)
        else
          for {
            acc1 <- convert1(acc, visited.head)
            result <- convert(acc1, visited.tail, Seq())
          } yield result
      } else {
        val mg = queue.head
        val mgInfo = fromTerminologyGraph(mg)

        mgInfo.nesting match {
          case Some(mgParent) =>

            val ok1 = visited.contains(mgParent)
            val ok2 = acc.find {
              case (m: types.MutableModelTerminologyGraph, _: types.ImmutableModelTerminologyGraph) =>
                m.equals(mgParent)
            }.nonEmpty
            val ok3 = queue.contains(mgParent)
            require(ok1 || ok2 || ok3,
                    s"queue.head: ${queue.head.kindIRI}, nesting parent: ${mgParent.kindIRI} (ok1=$ok1, ok2=$ok2, ok3=$ok3")
          case None           =>
            ()
        }

        val nestedQueue =
          mgInfo
          .nested
          .flatMap {
                     case _: types.ImmutableModelTerminologyGraph =>
                       None
                     case m: types.MutableModelTerminologyGraph   =>
                       if (queue.contains(m)) None
                       else Some(m)
                   }
          .to[Seq]

        val extendedQueue =
          mgInfo
          .imports
          .flatMap {
                     case _: types.ImmutableModelTerminologyGraph =>
                       None
                     case m: types.MutableModelTerminologyGraph   =>
                       if (queue.contains(m)) None
                       else Some(m)
                   }
          .to[Seq]

        convert(acc,
          extendedQueue ++
          nestedQueue.to[Seq] ++
          queue.tail.to[Seq],
          queue.head +: visited)
      }
    }

    for {
      m2i <- convert(Map(), Seq(g), Seq())
    } yield {
      require(m2i.contains(g))
      (m2i(g), m2i)
    }

  }

  def register
  (g: types.ImmutableModelTerminologyGraph,
   info: OWLAPITerminologyGraphSignature,
   m2i: types.Mutable2IMutableTerminologyMap,
   name: Option[String],
   uuid: Option[String])
  : NonEmptyList[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap = {

    val ok1 = immutableTBoxGraphs.put(g.kindIRI, g)
    require(ok1.isEmpty, s"register g: ${g.kindIRI}")

    makeMetadataInstanceIRI(omfMetadata.get, "Gro", g.kindIRI)
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
      for {
        change <- Seq(
          new AddAxiom(omfMetadata.get,
            owlDataFactory.getOWLDeclarationAxiom(graphI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH, graphI)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLObjectPropertyAssertionAxiom(OMF_HAS_TERMINOLOGY_KIND, graphI, okind)),
          new AddAxiom(omfMetadata.get,
            owlDataFactory
              .getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, graphI, g.kindIRI.toString))
        )
      } {
        val result = ontManager.applyChange(change)
        require(result == ChangeApplied.SUCCESSFULLY, s"\nregister:\n$change")
      }

      for {
        _ <-
        name
          .fold[NonEmptyList[java.lang.Throwable] \/ Unit](
          \/-(())
        ) { label =>
          setTerminologyGraphShortName(g, label)
        }

        _ <-
        uuid
          .fold[NonEmptyList[java.lang.Throwable] \/ Unit](
          \/-(())
        ) { id =>
          setTerminologyGraphUUID(g, id)
        }

        _ <- {
          (().right[NonEmptyList[java.lang.Throwable]] /: info.nested) {
            (acc: NonEmptyList[java.lang.Throwable] \/ Unit, n: types.ModelTerminologyGraph) =>
              acc +++ createTerminologyGraphDirectNestingAxiom(parentG = g, childG = n).map(_ => ())
          }
        }

        _ <- {
          (().right[NonEmptyList[java.lang.Throwable]] /: info.imports) {
            (acc: NonEmptyList[java.lang.Throwable] \/ Unit, i: types.ModelTerminologyGraph) =>
              acc +++ createTerminologyGraphDirectExtensionAxiom(extendingG = g, extendedG = i).map(_ => ())
          }
        }

        _ <- {
          (().right[NonEmptyList[java.lang.Throwable]] /: info.aspects) {
            (acc: NonEmptyList[java.lang.Throwable] \/ Unit, a: types.ModelEntityAspect) =>
              acc +++ registerOMFModelEntityAspectInstance(g, a).map(_ => ())
          }
        }

        _ <- {
          (().right[NonEmptyList[java.lang.Throwable]] /: info.concepts) {
            (acc: NonEmptyList[java.lang.Throwable] \/ Unit, c: types.ModelEntityConcept) =>
              acc +++ registerOMFModelEntityConceptInstance(g, c).map(_ => ())
          }
        }

        _ <- {
          (().right[NonEmptyList[java.lang.Throwable]] /: info.reifiedRelationships) {
            (acc: NonEmptyList[java.lang.Throwable] \/ Unit, r: types.ModelEntityReifiedRelationship) =>
              acc +++ registerOMFModelEntityReifiedRelationshipInstance(g, r).map(_ => ())
          }
        }

        _ <- {
          (().right[NonEmptyList[java.lang.Throwable]] /: info.scalarDataTypes) {
            (acc: NonEmptyList[java.lang.Throwable] \/ Unit, sc: types.ModelScalarDataType) =>
              acc +++ registerOMFModelScalarDataTypeInstance(g, sc).map(_ => ())
          }
        }

        _ <- {
          (().right[NonEmptyList[java.lang.Throwable]] /: info.structuredDataTypes) {
            (acc: NonEmptyList[java.lang.Throwable] \/ Unit, st: types.ModelStructuredDataType) =>
              acc +++ registerOMFModelStructuredDataTypeInstance(g, st).map(_ => ())
          }
        }

        _ <- {
          (().right[NonEmptyList[java.lang.Throwable]] /: info.axioms) {
            (acc: NonEmptyList[java.lang.Throwable] \/ Unit, axiom: types.ModelTermAxiom) =>
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
                case ax: types.EntityConceptUniversalRestrictionAxiom =>
                  createOMFEntityConceptUniversalRestrictionAxiomInstance(g, ax).map(_ => ())
                case ax: types.EntityConceptExistentialRestrictionAxiom =>
                  createOMFEntityConceptExistentialRestrictionAxiomInstance(g, ax).map(_ => ())
                //        case ax: types.ScalarDataTypeFacetRestriction =>
                case ax =>
                  NonEmptyList(
                    OMFError
                      .omfError(s"Unrecognized axiom: $ax")
                  ).left
              })
            }
          }

      } yield ()


      \/-(m2i)
    }
  }

  def loadTerminologyGraph
  (iri: IRI)
  (implicit ops: OWLAPIOMFOps)
  : NonEmptyList[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap) =
    immutableTBoxGraphs
      .get(iri)
      .fold[NonEmptyList[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap)](
      nonFatalCatch[Unit]
        .withApply {
          (cause: java.lang.Throwable) =>
            NonEmptyList(
              OMFError.omfOpsException(
                ops,
                s"setTerminologyGraphShortName failed: ${cause.getMessage}",
                cause)
            ).left
        }
        .apply({
          val o =
            if (ontManager.contains(iri))
              ontManager.getOntology(iri)
            else
              ontManager.loadOntology(iri)
          registerImmutableOntologyAsTerminologyGraph(o)
        })
    ){ tbox =>
      (tbox, Map[types.MutableModelTerminologyGraph, types.ImmutableModelTerminologyGraph]()).right
    }

  protected def registerImmutableOntologyAsTerminologyGraph
  (o: OWLOntology,
   extendedTGraphs: Iterable[types.ImmutableModelTerminologyGraph] = Nil)
  (implicit ops: OWLAPIOMFOps)
  : NonEmptyList[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap) = {
    val iri = o.getOntologyID.getOntologyIRI
    if (!iri.isPresent)
      NonEmptyList(
        OMFError.omfOpsError(ops, "An ontology must have an OntologyID with an Ontology IRI")
      ).left
    else
      immutableTBoxGraphs
        .get(iri.get)
        .fold[NonEmptyList[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap)]({

        val allExtendedTGraphs =
          (extendedTGraphs.to[Set].right[NonEmptyList[java.lang.Throwable]] /: o.getDirectImports) {
            (acc: NonEmptyList[java.lang.Throwable] \/ Set[types.ImmutableModelTerminologyGraph], i: OWLOntology) =>
              acc +++
                registerImmutableOntologyAsTerminologyGraph(i).map {
                  case (eg: types.ImmutableModelTerminologyGraph, _: types.Mutable2IMutableTerminologyMap) =>
                  Set(eg)
                }
          }

        allExtendedTGraphs.flatMap { extendedTGraphs =>
          types
          .immutableModelTerminologyGraphResolver(omfMetadata.get, extendedTGraphs, o, this)
          .flatMap { iMTGR =>
            iMTGR
              .resolve
              .map { case (g, m2i) =>
                immutableTBoxGraphs.put(iri.get, g)
                (g, m2i)
              }
          }
        }

      }) { g =>
        (g, Map[types.MutableModelTerminologyGraph, types.ImmutableModelTerminologyGraph]()).right
      }
  }

  def saveTerminologyGraph
  (g: types.ModelTerminologyGraph)
  (implicit ops: OWLAPIOMFOps)
  : NonEmptyList[java.lang.Throwable] \/ Unit = {
    val iri = catalogIRIMapper.resolveIRI(g.iri, catalogIRIMapper.saveResolutionStrategy)
    g.save(iri)
  }

  def saveTerminologyGraph
  (g: types.ModelTerminologyGraph, os: java.io.OutputStream)
  (implicit ops: OWLAPIOMFOps)
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    g.save(os)

  def makeTerminologyGraph
  (iri: IRI,
   relativeIRIPath: Option[String],
   kind: TerminologyKind)
  (implicit ops: OWLAPIOMFOps)
  : NonEmptyList[java.lang.Throwable] \/ types.MutableModelTerminologyGraph =
    mutableTBoxGraphs
    .get(iri)
    .fold[NonEmptyList[java.lang.Throwable] \/ types.MutableModelTerminologyGraph](
      if (ontManager.contains(iri))
        NonEmptyList(
          OMFError
          .omfOpsError(ops, s"makeTerminologyGraph(iri='$iri') --  already exists!")
        ).left
      else
      // not yet registered.
        createOMFModelTerminologyGraph(omfMetadata.get, iri, relativeIRIPath, ontManager.createOntology(iri), kind)
    ) { g =>
      g.right
    }

  def loadInstanceGraph
  (iri: IRI)
  : NonEmptyList[java.lang.Throwable] \/ instances.ImmutableModelInstanceGraph =
    ???

  def asImmutableInstanceGraph
  (g: instances.MutableModelInstanceGraph)
  : NonEmptyList[java.lang.Throwable] \/ instances.ImmutableModelInstanceGraph =
    ???

  def makeInstanceGraph
  (iri: IRI,
   instantiatedTGraphs: Iterable[types.ImmutableModelTerminologyGraph],
   extendedIGraphs: Iterable[instances.ImmutableModelInstanceGraph])
  : NonEmptyList[java.lang.Throwable] \/ instances.MutableModelInstanceGraph =
    ???

}
