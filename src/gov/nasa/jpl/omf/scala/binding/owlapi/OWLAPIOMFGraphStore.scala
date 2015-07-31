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

import java.io.OutputStream

import gov.nasa.jpl.omf.scala.binding.owlapi.types.ResolverHelper
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.{Imports, _}
import org.semanticweb.owlapi.util.PriorityCollection

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.reflect.internal.FatalError
import scala.util.{Failure, Success, Try}

case class OWLAPIOMFGraphStore(val omfModule: OWLAPIOMFModule, val ontManager: OWLOntologyManager) {

  require(null != omfModule)
  require(null != ontManager)

  implicit val ops = omfModule.ops

  val catalogIRIMapper: Option[CatalogIRIMapper] =
    for {
      catalogManager <- omfModule.catalogManager
    } yield {
      val mappers: PriorityCollection[OWLOntologyIRIMapper] = ontManager.getIRIMappers

      val mapper = new CatalogIRIMapper(catalogManager)
      mappers.add(Iterable[OWLOntologyIRIMapper](mapper).asJava)

      mapper
    }

  protected lazy val omfModelOntology = {
    val o = ontManager.loadOntology(omfModule.omfOntologyIRI)
    require(o != null, s"Could not find the OMF metadata ontology: ${omfModule.omfOntologyIRI}")
    //System.out.println(s"*** loaded omf metadata ontology: ${o.getOWLOntologyManager.getOntologyDocumentIRI(o)}")
    o
  }

  protected lazy val omfModelClasses =
    omfModelOntology.getClassesInSignature(Imports.EXCLUDED).
      map { c => c.getIRI.getRemainder.get -> c } toMap

  protected lazy val omfModelObjectPropertiesMap =
    omfModelOntology.getObjectPropertiesInSignature(Imports.EXCLUDED).
      map { op => op.getIRI.getRemainder.get -> op } toMap

  protected def omfModelObjectProperties(opIRI: String): OWLObjectProperty =
    omfModelObjectPropertiesMap.get(opIRI) match {
      case Some(op) =>
        op
      case None =>
        val keys = omfModelObjectPropertiesMap.keys.toList.sorted.mkString("\nkey: ","\nkey: ","\n")
        throw new IllegalArgumentException(s"No OMF Metadata ontology object property with iri $opIRI" + keys)
    }


  protected lazy val omfModelDataProperties =
    omfModelOntology.getDataPropertiesInSignature(Imports.EXCLUDED).
      map { dp => dp.getIRI.getRemainder.get -> dp } toMap

  protected lazy val omfNamedIndividuals =
    omfModelOntology.getIndividualsInSignature(Imports.EXCLUDED).
      map { dp => dp.getIRI.getRemainder.get -> dp } toMap

  protected lazy val allAnnotationProperties =
    omfModelOntology.getAnnotationPropertiesInSignature(Imports.INCLUDED)
      .map { ap => ap.getIRI.getShortForm -> ap } toMap

  lazy val RDFS_LABEL: OWLAnnotationProperty = ontManager.getOWLDataFactory.getRDFSLabel


  lazy val ANNOTATION_HAS_UUID: OWLAnnotationProperty =
    ontManager.
      getOWLDataFactory.
      getOWLAnnotationProperty(omfModule.ops.OMF_TBox_DataProperty_HasUUID)

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

  lazy val OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM =
    omfModelClasses("EntityDefinitionAspectSubClassAxiom")
  protected val OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityDefinitionAspectSubClassAxiom, OWLNamedIndividual]()

  lazy val OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM =
    omfModelClasses("EntityReifiedRelationshipSubClassAxiom")
  protected val OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.EntityReifiedRelationshipSubClassAxiom, OWLNamedIndividual]()

  lazy val OMF_SCALAR_DATA_TYPE_FACET_RESTRICTION =
    omfModelClasses("ScalarDataTypeFacetRestriction")
  protected val OMF_SCALAR_DATA_TYPE_FACET_RESTRICTION2Instance =
    scala.collection.mutable.HashMap[types.ScalarDataTypeFacetRestriction, OWLNamedIndividual]()

  // TerminologyGraphAxiom
  lazy val OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM =
    omfModelClasses("TerminologyGraphDirectExtensionAxiom")
  protected val OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.TerminologyGraphDirectExtensionAxiom, OWLNamedIndividual]()

  lazy val OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM =
    omfModelClasses("TerminologyGraphDirectNestingAxiom")
  protected val OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM2Instance =
    scala.collection.mutable.HashMap[types.TerminologyGraphDirectNestingAxiom, OWLNamedIndividual]()

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

  // ModelEntityDefinition
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

  // Named Individuals
  lazy val OMF_TOPLEVEL_DEFINITION_TBOX = omfNamedIndividuals("ToplevelDefinitionTBox")
  lazy val OMF_DEFINITION_TBOX = omfNamedIndividuals("DefinitionTBox")
  lazy val OMF_TOPLEVEL_DESIGNATION_TBOX = omfNamedIndividuals("ToplevelDesignationTBox")
  lazy val OMF_DESIGNATION_TBOX = omfNamedIndividuals("DesignationTBox")

  // Data Properties
  lazy val OMF_HAS_IRI = omfModelDataProperties("hasIRI")
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
  : Try[types.TerminologyGraphDirectNestingAxiom] = {
    val nestedChildren = nestingParent2NestedChildren.getOrElseUpdate(
      parentG,
      scala.collection.mutable.HashSet[types.ModelTerminologyGraph]())
    val axiom = directNestingAxioms.find { ax =>
      ax.nestedChild.iri == childG.iri &&
        ax.nestingParent.iri == parentG.iri
    }
    (axiom, nestedChild2NestingParent.get(childG)) match {
      case (None, Some(_)) =>
        Failure(new FatalError(s"createTerminologyGraphDirectNestingAxiom inconsistency"))
      case (Some(_), None) =>
        Failure(new FatalError(s"createTerminologyGraphDirectNestingAxiom inconsistency"))
      case (Some(ax), Some(nestingParent)) =>
        if (nestingParent.iri != parentG.iri)
          Failure(new FatalError(s"createTerminologyGraphDirectNestingAxiom inconsistency"))
        else
          Success(ax)
      case (None, None) =>
        val axiom = types.TerminologyGraphDirectNestingAxiom(
          nestedChild = childG,
          nestingParent = parentG)
        val parentI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(parentG)
        val nestedI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(childG)
        val directNestingI = owlDataFactory.getOWLNamedIndividual(
          makeMetadataInstanceIRI(omfMetadata.get, "DN", OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM2Instance))
        OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM2Instance += (axiom -> directNestingI)
        for {
          change <- Seq(
            ontManager.applyChange(new AddAxiom(omfMetadata.get,
              owlDataFactory.getOWLDeclarationAxiom(directNestingI))),
            ontManager.applyChange(new AddAxiom(omfMetadata.get,
              owlDataFactory.getOWLClassAssertionAxiom(
                OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM, directNestingI))),
            ontManager.applyChange(new AddAxiom(omfMetadata.get,
              owlDataFactory.getOWLObjectPropertyAssertionAxiom(
                OMF_HAS_DIRECT_NESTING, directNestingI, parentI))),
            ontManager.applyChange(new AddAxiom(omfMetadata.get,
              owlDataFactory.getOWLObjectPropertyAssertionAxiom(
                OMF_HAS_DIRECT_NESTED_CHILD, directNestingI, nestedI))))
        } require(change == ChangeApplied.SUCCESSFULLY)

        for {
          added <- Seq(
            directNestingAxioms.add(axiom),
            nestedChild2NestingParent.put(childG, parentG).isEmpty,
            nestedChildren.add(childG))
        } require(added)
        Success(axiom)
    }
  }

  def createTerminologyGraphDirectExtensionAxiom
  (extendingG: types.ModelTerminologyGraph,
   extendedG: types.ModelTerminologyGraph)
  : Try[types.TerminologyGraphDirectExtensionAxiom] = {
    val extendedParents = extendingChild2ExtendedParents.getOrElseUpdate(
      extendingG,
      scala.collection.mutable.HashSet[types.ModelTerminologyGraph]())
    if (extendedParents.contains(extendedG))
      directExtensionAxioms.find { ax =>
        ax.extendingChild.iri == extendingG.iri &&
          ax.extendedParent.iri == extendedG.iri
      } match {
        case None =>
          System.out.println(s"directExtensionAxioms: ${directExtensionAxioms.size}")
          directExtensionAxioms.foreach { ax =>
            System.out.println(s"=> extending: ${ax.extendingChild.iri} extended: ${ax.extendedParent.iri}")
          }
          System.out.println(s"extendingChild2ExtendedParents: ${extendingChild2ExtendedParents.size}")
          extendingChild2ExtendedParents.foreach { case (child, parents) =>
            System.out.println(s"=> child: ${child.iri} parents: ${parents.size}")
            parents.foreach { parent =>
              System.out.println(s"==> parent: ${parent.iri}")
            }
          }
          Failure(new FatalError("Duplicate TerminologyGraphDirectExtensionAxiom not in directExtensionAxioms"))
        case Some(ax) =>
          Success(ax)
      }
    else {

      if (!extendingG.ont.getDirectImportsDocuments.contains(extendedG.iri)) {
        val decl = ontManager.getOWLDataFactory.getOWLImportsDeclaration(extendedG.iri)
        val changeApplied = ontManager.applyChange(new AddImport(extendingG.ont, decl))
        require(ChangeApplied.SUCCESSFULLY == changeApplied)
      }

      val axiom = types.TerminologyGraphDirectExtensionAxiom(
        extendingChild = extendingG,
        extendedParent = extendedG)

      for {
        added <- Seq(
          directExtensionAxioms.add(axiom),
          extendedParents.add(extendedG))
      } require(added)

      val extendingI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(axiom.extendingChild)
      val extendedI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(axiom.extendedParent)
      val directImportingI = owlDataFactory.getOWLNamedIndividual(
        makeMetadataInstanceIRI(omfMetadata.get, "DI", OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM2Instance))
      OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM2Instance += (axiom -> directImportingI)
      //    System.out.println(
      //      s"""## createOMFModelTerminologyGraphExtension:
      //         |extending: ${axiom.extendingChild.iri}
      //         |extended: ${axiom.extendedParent.iri}""".stripMargin)
      for {
        change <- Seq(
          ontManager.applyChange(new AddAxiom(omfMetadata.get,
            owlDataFactory.getOWLDeclarationAxiom(directImportingI))),
          ontManager.applyChange(new AddAxiom(omfMetadata.get,
            owlDataFactory.getOWLClassAssertionAxiom(OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM, directImportingI))),
          ontManager.applyChange(new AddAxiom(omfMetadata.get,
            owlDataFactory.getOWLObjectPropertyAssertionAxiom(
              OMF_HAS_DIRECT_EXTENDED_PARENT, directImportingI, extendedI))),
          ontManager.applyChange(new AddAxiom(omfMetadata.get,
            owlDataFactory.getOWLObjectPropertyAssertionAxiom(
              OMF_HAS_DIRECT_EXTENSIONING_CHILD, directImportingI, extendingI))))
      } require(change == ChangeApplied.SUCCESSFULLY)

      Success(axiom)
    }
  }

  def getNestingGraph
  (g: types.ModelTerminologyGraph)
  : Option[types.ModelTerminologyGraph] =
    nestedChild2NestingParent.get(g)

  def getNestedGraphs
  (g: types.ModelTerminologyGraph)
  : Iterable[types.ModelTerminologyGraph] =
    nestingParent2NestedChildren(g)

  def fromTerminologyGraph
  (g: types.ModelTerminologyGraph)
  : OWLAPITerminologyGraphSignature =
    g.fromTerminologyGraph(
      nestedChild2NestingParent.get(g),
      nestingParent2NestedChildren(g),
      extendingChild2ExtendedParents(g))

  // OMF Ontology Instance Model Constructors  

  val owlDataFactory = ontManager.getOWLDataFactory

  def makeMetadataInstanceIRI
  (o: OWLOntology, instanceKind: String, map: scala.collection.Map[_, OWLNamedIndividual])
  : IRI =
    omfModule.ops.withFragment(o.getOntologyID.getOntologyIRI.get, instanceKind + map.size).get

  def createOMFModelTerminologyGraph
  (o: OWLOntology,
   iri: IRI,
   tboxOnt: OWLOntology,
   kind: TerminologyKind.TerminologyKind)
  : Try[types.MutableModelTerminologyGraph] = {
    val graphT =  new types.MutableModelTerminologyGraph(kind = kind, ont = tboxOnt)
    mutableTBoxGraphs.put(iri, graphT)
    val graphI = owlDataFactory.getOWLNamedIndividual(
      makeMetadataInstanceIRI(o, "G", OMF_MODEL_TERMINOLOGY_GRAPH2Instance))
    OMF_MODEL_TERMINOLOGY_GRAPH2Instance += (graphT -> graphI)
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
      change <- Seq(
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLDeclarationAxiom(graphI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH, graphI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_TERMINOLOGY_KIND, graphI, okind))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, graphI, graphT.iri.toString)))
      )
    } require(change == ChangeApplied.SUCCESSFULLY)
    Success(graphT)
  }

  def registerOMFModelEntityAspectInstance
  (o: OWLOntology,
   metadataGraph: types.ModelTerminologyGraph,
   aspectT: types.ModelEntityAspect)
  : Try[OWLNamedIndividual] =
    OMF_MODEL_ENTITY_ASPECT2Instance.get(aspectT) match {
      case Some(aspectI) =>
        //System.out.println( s"#! Aspect: ${aspectT.iri}" )
        Success(aspectI)
      case None =>
        val aspectI = owlDataFactory.getOWLNamedIndividual(
          makeMetadataInstanceIRI(o, "A", OMF_MODEL_ENTITY_ASPECT2Instance))
        OMF_MODEL_ENTITY_DEFINITION2Instance += (aspectT -> aspectI)
        OMF_MODEL_ENTITY_ASPECT2Instance += (aspectT -> aspectI)
        for {
          change <- Seq(
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLDeclarationAxiom(aspectI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_ENTITY_ASPECT, aspectI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLObjectPropertyAssertionAxiom(
                OMF_DIRECTLY_DEFINES_TYPE_TERM,
                OMF_MODEL_TERMINOLOGY_GRAPH2Instance(metadataGraph),
                aspectI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, aspectI, aspectT.iri.toString)))
          )
        } require(change == ChangeApplied.SUCCESSFULLY)
        //        System.out.println( s"## Aspect: ${aspectT.iri}" )
        Success(aspectI)
    }

  def createOMFModelEntityAspectInstance
  (o: OWLOntology,
   metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: String,
   aspectT: types.ModelEntityAspect)
  : Try[Unit] =
    for {
      _ <- registerOMFModelEntityAspectInstance(o, metadataGraph, aspectT)
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, aspectI, hasProvenanceFromRule ) ) )
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, aspectI, hasName ) ) )
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, aspectI, hasQualifiedName ) ) )
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, aspectI, hasUUID ) ) )
    } yield ()

  def registerOMFModelEntityConceptInstance
  (o: OWLOntology,
   metadataGraph: types.ModelTerminologyGraph,
   conceptT: types.ModelEntityConcept)
  : Try[OWLNamedIndividual] =
    OMF_MODEL_ENTITY_CONCEPT2Instance.get(conceptT) match {
      case Some(conceptI) =>
        //System.out.println( s"#! Concept: ${conceptT.iri}" )
        Success(conceptI)
      case None =>
        val conceptI = owlDataFactory.getOWLNamedIndividual(
          makeMetadataInstanceIRI(o, "C", OMF_MODEL_ENTITY_CONCEPT2Instance))
        OMF_MODEL_ENTITY_DEFINITION2Instance += (conceptT -> conceptI)
        OMF_MODEL_ENTITY_CONCEPT2Instance += (conceptT -> conceptI)
        for {
          change <- Seq(
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLDeclarationAxiom(conceptI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_ENTITY_CONCEPT, conceptI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLObjectPropertyAssertionAxiom(
                OMF_DIRECTLY_DEFINES_TYPE_TERM,
                OMF_MODEL_TERMINOLOGY_GRAPH2Instance(metadataGraph),
                conceptI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, conceptI, conceptT.iri.toString)))
          )
        } require(change == ChangeApplied.SUCCESSFULLY)
        //        System.out.println( s"## Concept: ${conceptT.iri}" )
        Success(conceptI)
    }

  def createOMFModelEntityConceptInstance
  (metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: Option[String],
   conceptT: types.ModelEntityConcept,
   isAbstract: Boolean)
  : Try[Unit] =
    for {
      conceptI <- registerOMFModelEntityConceptInstance(omfMetadata.get, metadataGraph, conceptT)
      //    ontManager.applyChange( new AddAxiom(
      // o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
      // OMF_HAS_PROVENANCE_FROM_RULE, conceptI, hasProvenanceFromRule ) ) )
      //    ontManager.applyChange( new AddAxiom(
      // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, conceptI, hasName ) ) )
      //    ontManager.applyChange( new AddAxiom(
      // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, conceptI, hasQualifiedName ) ) )
      //    ontManager.applyChange( new AddAxiom(
      // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, conceptI, hasUUID ) ) )
    } yield ()

  def registerOMFModelEntityReifiedRelationshipInstance
  (o: OWLOntology,
   metadataGraph: types.ModelTerminologyGraph,
   relationshipT: types.ModelEntityReifiedRelationship)
  : Try[OWLNamedIndividual] =
    OMF_MODEL_ENTITY_RELATIONSHIP2Instance.get(relationshipT) match {
      case Some(relationshipI) =>
        //System.out.println( s"#! Relationship: ${relationshipT.iri}" )
        Success(relationshipI)
      case None =>
        val relationshipI = owlDataFactory.getOWLNamedIndividual(
          makeMetadataInstanceIRI(o, "R", OMF_MODEL_ENTITY_RELATIONSHIP2Instance))
        OMF_MODEL_ENTITY_DEFINITION2Instance += (relationshipT -> relationshipI)
        OMF_MODEL_ENTITY_RELATIONSHIP2Instance += (relationshipT -> relationshipI)
        for {
          change <- Seq(
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLDeclarationAxiom(relationshipI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_ENTITY_RELATIONSHIP, relationshipI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLObjectPropertyAssertionAxiom(
                OMF_DIRECTLY_DEFINES_TYPE_TERM,
                OMF_MODEL_TERMINOLOGY_GRAPH2Instance(metadataGraph),
                relationshipI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLDataPropertyAssertionAxiom(
                OMF_HAS_IRI, relationshipI, relationshipT.iri.toString)))
          )
        } require(change == ChangeApplied.SUCCESSFULLY)
        //System.out.println( s"## Relationship: ${relationshipT.iri}" )
        Success(relationshipI)
    }

  def createOMFModelEntityReifiedRelationshipInstance
  (metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: Option[String],
   relationshipT: types.ModelEntityReifiedRelationship,
   isAbstract: Boolean)
  : Try[Unit] =
    for {
      relationshipI <- registerOMFModelEntityReifiedRelationshipInstance(omfMetadata.get, metadataGraph, relationshipT)
      sourceI = OMF_MODEL_ENTITY_DEFINITION2Instance(relationshipT.source)
      targetI = OMF_MODEL_ENTITY_DEFINITION2Instance(relationshipT.target)
      _ = for {
        change <- Seq(
          ontManager.applyChange(new AddAxiom(omfMetadata.get,
            owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_SOURCE, relationshipI, sourceI))),
          ontManager.applyChange(new AddAxiom(omfMetadata.get,
            owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_TARGET, relationshipI, targetI)))
          //    ontManager.applyChange( new AddAxiom( o,
          //     owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, relationshipI, hasProvenanceFromRule ) ) )
          //    ontManager.applyChange( new AddAxiom( o,
          //     owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, relationshipI, hasName ) ) )
          //    ontManager.applyChange( new AddAxiom( o,
          //     owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, relationshipI, hasQualifiedName ) ) )
          //    ontManager.applyChange( new AddAxiom( o,
          //     owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, relationshipI, hasUUID ) ) )
        )
      } require(change == ChangeApplied.SUCCESSFULLY)
    } yield ()


  def createOMFModelScalarDataTypeInstance
  (o: OWLOntology,
   metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: String,
   scalarDT: types.ModelScalarDataType,
   hasName: Option[String],
   hasUUID: Option[String])
  : Try[Unit] =
    for {
      _ <- registerOMFModelScalarDataTypeInstance(o, metadataGraph, scalarDT, hasName, hasUUID)
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, aspectI, hasProvenanceFromRule ) ) )
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, aspectI, hasName ) ) )
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, aspectI, hasQualifiedName ) ) )
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, aspectI, hasUUID ) ) )
    } yield ()

  def registerOMFModelScalarDataTypeInstance
  (o: OWLOntology,
   metadataGraph: types.ModelTerminologyGraph,
   scalarDT: types.ModelScalarDataType,
   hasName: Option[String] = None,
   hasUUID: Option[String] = None)
  : Try[OWLNamedIndividual] =
    OMF_MODEL_SCALAR_DATA_TYPE2Instance.get(scalarDT) match {
      case Some(scalarDI) =>
        Success(scalarDI)
      case None =>
        val scalarDI = owlDataFactory.getOWLNamedIndividual(
          makeMetadataInstanceIRI(o, "SC", OMF_MODEL_SCALAR_DATA_TYPE2Instance))
        OMF_MODEL_DATA_TYPE_DEFINITION2Instance += (scalarDT -> scalarDI)
        OMF_MODEL_SCALAR_DATA_TYPE2Instance += (scalarDT -> scalarDI)
        for {
          change <- Seq(
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLDeclarationAxiom(scalarDI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_SCALAR_DATA_TYPE, scalarDI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLObjectPropertyAssertionAxiom(
                OMF_DIRECTLY_DEFINES_TYPE_TERM,
                OMF_MODEL_TERMINOLOGY_GRAPH2Instance(metadataGraph),
                scalarDI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, scalarDI, scalarDT.iri.toString)))
          ) ++ (hasName match {
            case None =>
              Seq()
            case Some(shortName) =>
              Seq(ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_SHORT_NAME, scalarDI, shortName))))
          }) ++ (hasUUID match {
            case None =>
              Seq()
            case Some(id) =>
              Seq(ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, scalarDI, id))))
          })
        } require(change == ChangeApplied.SUCCESSFULLY)
        Success(scalarDI)
    }


  def createOMFModelStructuredDataTypeInstance
  (o: OWLOntology,
   metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: String,
   structuredDT: types.ModelStructuredDataType,
   hasName: Option[String],
   hasUUID: Option[String])
  : Try[Unit] =
    for {
      _ <- registerOMFModelStructuredDataTypeInstance(o, metadataGraph, structuredDT, hasName, hasUUID)
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, aspectI, hasProvenanceFromRule ) ) )
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, aspectI, hasName ) ) )
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, aspectI, hasQualifiedName ) ) )
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, aspectI, hasUUID ) ) )
    } yield ()

  def registerOMFModelStructuredDataTypeInstance
  (o: OWLOntology,
   metadataGraph: types.ModelTerminologyGraph,
   structuredDT: types.ModelStructuredDataType,
   hasName: Option[String] = None,
   hasUUID: Option[String] = None)
  : Try[OWLNamedIndividual] =
    OMF_MODEL_STRUCTURED_DATA_TYPE2Instance.get(structuredDT) match {
      case Some(scalarDI) =>
        Success(scalarDI)
      case None =>
        val scalarDI = owlDataFactory.getOWLNamedIndividual(
          makeMetadataInstanceIRI(o, "ST", OMF_MODEL_STRUCTURED_DATA_TYPE2Instance))
        OMF_MODEL_DATA_TYPE_DEFINITION2Instance += (structuredDT -> scalarDI)
        OMF_MODEL_STRUCTURED_DATA_TYPE2Instance += (structuredDT -> scalarDI)
        for {
          change <- Seq(
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLDeclarationAxiom(scalarDI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_STRUCTURED_DATA_TYPE, scalarDI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLObjectPropertyAssertionAxiom(
                OMF_DIRECTLY_DEFINES_TYPE_TERM,
                OMF_MODEL_TERMINOLOGY_GRAPH2Instance(metadataGraph),
                scalarDI))),
            ontManager.applyChange(new AddAxiom(o,
              owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, scalarDI, structuredDT.iri.toString)))
          ) ++ (hasName match {
            case None =>
              Seq()
            case Some(shortName) =>
              Seq(ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_SHORT_NAME, scalarDI, shortName))))
          }) ++ (hasUUID match {
            case None =>
              Seq()
            case Some(id) =>
              Seq(ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, scalarDI, id))))
          })
        } require(change == ChangeApplied.SUCCESSFULLY)
        Success(scalarDI)
    }

  def createOMFEntityDefinitionAspectSubClassAxiomInstance
  (o: OWLOntology,
   metadataGraph: types.ModelTerminologyGraph,
   axiomT: types.EntityDefinitionAspectSubClassAxiom,
   hasProvenanceFromRule: Option[String] = None)
  : Try[Unit] = {
    val subI = OMF_MODEL_ENTITY_DEFINITION2Instance(axiomT.sub)
    val supI = OMF_MODEL_ENTITY_ASPECT2Instance(axiomT.sup)
    val axiomI = owlDataFactory.getOWLNamedIndividual(makeMetadataInstanceIRI(o,
      "DefinitionAspectSubClass", OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM2Instance))
    OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
    for {
      change <- Seq(
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLDeclarationAxiom(axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(
            OMF_DIRECTLY_ASSERTS_AXIOM,
            OMF_MODEL_TERMINOLOGY_GRAPH2Instance(metadataGraph),
            axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLClassAssertionAxiom(OMF_ENTITY_DEFINITION_ASPECT_SUB_CLASS_AXIOM, axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_GENERAL_ASPECT, axiomI, supI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_SPECIFIC_ENTITY, axiomI, subI))))
    //    ontManager.applyChange( new AddAxiom(
    //   o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //     OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } require(change == ChangeApplied.SUCCESSFULLY)
//    System.out.println("# OMF/OWLAPI store: EntityDefinitionAspectSubClass:" +
//      " sup=" + supI.getIRI +
//      " sub=" + subI.getIRI)
    Success(Unit)
  }

  def createOMFEntityConceptSubClassAxiomInstance
  (metadataGraph: types.ModelTerminologyGraph,
   axiomT: types.EntityConceptSubClassAxiom,
   hasProvenanceFromRule: Option[String] = None)
  : Try[Unit] = {
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.sub)
    val supI = OMF_MODEL_ENTITY_CONCEPT2Instance(axiomT.sup)
    val axiomI = owlDataFactory.getOWLNamedIndividual(makeMetadataInstanceIRI(omfMetadata.get,
      "ConceptSubClass", OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance))
    OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
    for {
      change <- Seq(
        ontManager.applyChange(new AddAxiom(omfMetadata.get,
          owlDataFactory.getOWLDeclarationAxiom(axiomI))),
        ontManager.applyChange(new AddAxiom(omfMetadata.get,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(
            OMF_DIRECTLY_ASSERTS_AXIOM,
            OMF_MODEL_TERMINOLOGY_GRAPH2Instance(metadataGraph),
            axiomI))),
        ontManager.applyChange(new AddAxiom(omfMetadata.get,
          owlDataFactory.getOWLClassAssertionAxiom(OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM, axiomI))),
        ontManager.applyChange(new AddAxiom(omfMetadata.get,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_GENERAL_CONCEPT, axiomI, supI))),
        ontManager.applyChange(new AddAxiom(omfMetadata.get,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_SPECIFIC_CONCEPT, axiomI, subI)))
        //    ontManager.applyChange( new AddAxiom(
        //   o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
        //    OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
      )
    } require(change == ChangeApplied.SUCCESSFULLY)
//    System.out.println("# OMF/OWLAPI store: ConceptSubClass:" +
//      " sup=" + supI.getIRI +
//      " sub=" + subI.getIRI)
    Success(Unit)
  }

  def createOMFEntityConceptUniversalRestrictionAxiomInstance
  (o: OWLOntology,
   metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: String,
   axiomT: types.EntityConceptUniversalRestrictionAxiom,
   subT: types.ModelEntityConcept,
   relT: types.ModelEntityReifiedRelationship,
   rangeT: types.ModelEntityDefinition)
  : Try[Unit] = {
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance(subT)
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(relT)
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance(rangeT)
    val axiomI = owlDataFactory.getOWLNamedIndividual(makeMetadataInstanceIRI(o,
      "UniversalConceptRestriction", OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM2Instance))
    OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
    for {
      change <- Seq(
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLDeclarationAxiom(axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(
            OMF_DIRECTLY_ASSERTS_AXIOM,
            OMF_MODEL_TERMINOLOGY_GRAPH2Instance(metadataGraph),
            axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLClassAssertionAxiom(OMF_ENTITY_CONCEPT_UNIVERSAL_RESTRICTION_AXIOM, axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_RESTRICTS_CONCEPT, axiomI, subI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_RESTRICTS_RELATIONSHIP, axiomI, relI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_RANGE, axiomI, rangeI))))
    //    ontManager.applyChange( new AddAxiom(
    // o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //    OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } require(change == ChangeApplied.SUCCESSFULLY)
    Success(Unit)
  }

  def createOMFEntityConceptExistentialRestrictionAxiomInstance
  (o: OWLOntology,
   metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: String,
   axiomT: types.EntityConceptExistentialRestrictionAxiom,
   subT: types.ModelEntityConcept,
   relT: types.ModelEntityReifiedRelationship,
   rangeT: types.ModelEntityDefinition)
  : Try[Unit] = {
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance(subT)
    val relI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(relT)
    val rangeI = OMF_MODEL_ENTITY_DEFINITION2Instance(rangeT)
    val axiomI = owlDataFactory.getOWLNamedIndividual(
      makeMetadataInstanceIRI(o,
        "ExistentialConceptRestriction",
        OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM2Instance))
    OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM2Instance += (axiomT -> axiomI)
    for {
      change <- Seq(
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLDeclarationAxiom(axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(
            OMF_DIRECTLY_ASSERTS_AXIOM,
            OMF_MODEL_TERMINOLOGY_GRAPH2Instance(metadataGraph),
            axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLClassAssertionAxiom(OMF_ENTITY_CONCEPT_EXISTENTIAL_RESTRICTION_AXIOM, axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_RESTRICTS_CONCEPT, axiomI, subI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_RESTRICTS_RELATIONSHIP, axiomI, relI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_RESTRICTED_RANGE, axiomI, rangeI))))
    //    ontManager.applyChange( new AddAxiom(
    //    o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //      OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } require(change == ChangeApplied.SUCCESSFULLY)
    Success(Unit)
  }

  def createOMFEntityReifiedRelationshipSubClassAxiomInstance
  (o: OWLOntology,
   metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: String,
   axiomT: types.EntityReifiedRelationshipSubClassAxiom,
   subT: types.ModelEntityReifiedRelationship,
   supT: types.ModelEntityReifiedRelationship)
  : Try[Unit] = {
    val subI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(subT)
    val supI = OMF_MODEL_ENTITY_RELATIONSHIP2Instance(supT)
    val axiomI = owlDataFactory.getOWLNamedIndividual(makeMetadataInstanceIRI(o,
      "RelationshipSubClass", OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM2Instance))
    OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
    for {
      change <- Seq(
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLDeclarationAxiom(axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(
            OMF_DIRECTLY_ASSERTS_AXIOM,
            OMF_MODEL_TERMINOLOGY_GRAPH2Instance(metadataGraph),
            axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLClassAssertionAxiom(OMF_ENTITY_REIFIED_RELATIONSHIP_SUB_CLASS_AXIOM, axiomI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_GENERAL_REIFIED_RELATIONSHIP, axiomI, supI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_SPECIFIC_REIFIED_RELATIONSHIP, axiomI, subI))))
    //    ontManager.applyChange( new AddAxiom(
    //    o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //      OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
    } require(change == ChangeApplied.SUCCESSFULLY)
    Success(Unit)
  }

  // OMF API
  def asImmutableTerminologyGraph
  (g: types.MutableModelTerminologyGraph)
  : Try[types.ImmutableModelTerminologyGraph] = {
    val tgraph = fromTerminologyGraph(g)
    if (immutableTBoxGraphs.contains(g.iri))
      Failure(new IllegalArgumentException(
        s"There is already an immutable terminology graph with IRI='${g.iri}'"))
    else 
      tgraph.nesting match {
        case Some(nestingParent) =>
          Failure(new IllegalArgumentException(
            s"Immutability conversion is currently not supported " +
              s"for mutable graphs that have a nesting parent, $nestingParent")
          )
        case None =>

          val nestedG = tgraph.nested map {
            case i: types.ImmutableModelTerminologyGraph =>
              i
            case m: types.MutableModelTerminologyGraph =>
              this.asImmutableTerminologyGraph(m) match {
                case Failure(t) =>
                  return Failure(t)
                case Success(i) =>
                  i
              }
          }


          val extendedG = tgraph.imports map {
            case i: types.ImmutableModelTerminologyGraph =>
              i
            case m: types.MutableModelTerminologyGraph =>
              this.asImmutableTerminologyGraph(m) match {
                case Failure(t) =>
                  return Failure(t)
                case Success(i) =>
                  i
              }
          }

          val itgraph = tgraph.copy(nested = nestedG, imports=extendedG)

          val graphT = types.ImmutableModelTerminologyGraph(
            tgraph.kind,
            g.ont,
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
            tgraph.axioms.toList)(g.ops)

          register(graphT, itgraph)
      }
  }

  def register
  ( g: types.ImmutableModelTerminologyGraph,
    info: OWLAPITerminologyGraphSignature)
  : Try[types.ImmutableModelTerminologyGraph] = {

    immutableTBoxGraphs.put(g.iri, g)

    val graphI = owlDataFactory.getOWLNamedIndividual(
      makeMetadataInstanceIRI(omfMetadata.get, "G", OMF_MODEL_TERMINOLOGY_GRAPH2Instance))
    OMF_MODEL_TERMINOLOGY_GRAPH2Instance += (g -> graphI)
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
        ontManager.applyChange(new AddAxiom(omfMetadata.get,
          owlDataFactory.getOWLDeclarationAxiom(graphI))),
        ontManager.applyChange(new AddAxiom(omfMetadata.get,
          owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH, graphI))),
        ontManager.applyChange(new AddAxiom(omfMetadata.get,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_TERMINOLOGY_KIND, graphI, okind))),
        ontManager.applyChange(new AddAxiom(omfMetadata.get,
          owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, graphI, g.iri.toString)))
      )
    } require(change == ChangeApplied.SUCCESSFULLY)

    info.nested foreach { aNestedG: types.ModelTerminologyGraph =>
      ops.addNestedTerminologyGraph(
        parentG = g,
        nestedG = aNestedG)(this)
    }

    info.imports foreach { anExtendedG: types.ModelTerminologyGraph =>
      ops.addTerminologyGraphExtension(
        extendingG = g,
        extendedG = anExtendedG)(this)
    }

    // short name & uuid are represented in the ontology, g.ont
    for {
      a <- info.aspects
      ok = registerOMFModelEntityAspectInstance(omfMetadata.get, g, a)
    } require(ok.isSuccess)

    for {
      c <- info.concepts
      ok = registerOMFModelEntityConceptInstance(omfMetadata.get, g, c)
    } require(ok.isSuccess)

    for {
      rr <- info.reifiedRelationships
      ok = registerOMFModelEntityReifiedRelationshipInstance(omfMetadata.get, g, rr)
    } require(ok.isSuccess)

    for {
      sc <- info.scalarDataTypes
      ok = registerOMFModelScalarDataTypeInstance(omfMetadata.get, g, sc, None, None)
    } require(ok.isSuccess)

    for {
      st <- info.structuredDataTypes
      ok = registerOMFModelStructuredDataTypeInstance(omfMetadata.get, g, st, None, None)
    } require(ok.isSuccess)

    Success(g)
  }

  def loadTerminologyGraph
  (iri: IRI)
  (implicit ops: OWLAPIOMFOps)
  : Try[types.ImmutableModelTerminologyGraph] =
    immutableTBoxGraphs.get(iri) match {
      case Some(tbox) =>
        Success(tbox)
      case None =>
        try {
          val o =
            if (ontManager.contains(iri))
              ontManager.getOntology(iri)
            else
              ontManager.loadOntology(iri)
//          System.out.println(
//           s"# loadTerminologyGraph: iri=${iri}, o=${o.getOWLOntologyManager.getOntologyDocumentIRI( o )}" )
          registerImmutableOntologyAsTerminologyGraph(o)
        } catch {
          case t: OWLOntologyCreationException =>
            Failure(t.fillInStackTrace)
        }
    }

  protected def registerImmutableOntologyAsTerminologyGraph
  (o: OWLOntology,
   extendedTGraphs: Iterable[types.ImmutableModelTerminologyGraph] = Nil)
  (implicit ops: OWLAPIOMFOps)
  : Try[types.ImmutableModelTerminologyGraph] = {
    val iri = o.getOntologyID.getOntologyIRI
//    System.out.println(s"## >> registering ImmutableModelTerminologyGraph ${iri.get}")
    if (!iri.isPresent)
      Failure(new IllegalArgumentException("An ontology must have an OntologyID with an Ontology IRI"))
    else
      immutableTBoxGraphs.get(iri.get) match {
        case Some(g) =>
          // already registered.
//          System.out.println(s"## << already registered ImmutableModelTerminologyGraph ${iri.get}")
          Success(g)

        case None =>
          // not yet registered.

          val directlyExtendedTGraphs = scala.collection.mutable.HashSet[types.ImmutableModelTerminologyGraph]()

          o.getDirectImports foreach {
            importedO =>
//              System.out.println(s"## -- direct import: ${importedO.getOntologyID.getOntologyIRI.get}")
              registerImmutableOntologyAsTerminologyGraph(importedO) match {
                case Failure(t) =>
                  return Failure(t)

                case Success(eg) =>
                  directlyExtendedTGraphs += eg
              }
          }

          directlyExtendedTGraphs ++= extendedTGraphs

//          System.out.println(
//            s"## >> Creating ImmutableModelTerminologyGraph: ${iri.get} " +
//              s"(${directlyExtendedTGraphs.size} direct extensions)")
          for {
            g <- types.ImmutableModelTerminologyGraphResolver(
              ResolverHelper(omfMetadata.get, directlyExtendedTGraphs, o, this)).resolve
          } yield {
//            System.out.println(s"## << Created ImmutableModelTerminologyGraph: ${iri.get}")
            immutableTBoxGraphs.put(iri.get, g)
//            System.out.println(s"## << registered ImmutableModelTerminologyGraph ${iri.get}")
            g
          }
      }
  }

  def saveTerminologyGraph
  (g: types.ModelTerminologyGraph)
  (implicit ops: OWLAPIOMFOps)
  : Try[Unit] =
    catalogIRIMapper match {
      case None => Failure(new IllegalArgumentException(
        s"Cannot save a terminology graph without a catalog IRI mapper"))
      case Some(iriMapper) =>
        val iri = iriMapper.resolveIRI(g.iri, iriMapper.saveResolutionStrategy)
        g.save(iri)
    }

  def saveTerminologyGraph
  (g: types.ModelTerminologyGraph, os: OutputStream)
  (implicit ops: OWLAPIOMFOps)
  : Try[Unit] =
    g.save(os)


  def makeTerminologyGraph
  (iri: IRI,
   kind: TerminologyKind)
  (implicit ops: OWLAPIOMFOps)
  : Try[types.MutableModelTerminologyGraph] =
    mutableTBoxGraphs.get(iri) match {
      case Some(g) =>
        // already registered.
        Success(g)

      case None =>
        if (ontManager.contains(iri))
          Failure(new IllegalArgumentException(s"An ontology with iri='$iri' already exists"))
        else
          // not yet registered.
          createOMFModelTerminologyGraph(omfMetadata.get, iri, ontManager.createOntology(iri), kind)
    }

  def loadInstanceGraph
  (iri: IRI)
  : Try[instances.ImmutableModelInstanceGraph] =
    ???

  def asImmutableInstanceGraph
  (g: instances.MutableModelInstanceGraph)
  : Try[instances.ImmutableModelInstanceGraph] =
    ???

  def makeInstanceGraph
  (iri: IRI,
   instantiatedTGraphs: Iterable[types.ImmutableModelTerminologyGraph],
   extendedIGraphs: Iterable[instances.ImmutableModelInstanceGraph])
  : Try[instances.MutableModelInstanceGraph] =
    ???

}