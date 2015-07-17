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
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util.PriorityCollection

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

case class OWLAPIOMFGraphStore(val omfModule: OWLAPIOMFModule, val ontManager: OWLOntologyManager) {

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
        throw new IllegalArgumentException(s"No OMF Metadata ontology object property with iri $opIRI")
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

  lazy val OMF_HAS_GRAPH = omfModelObjectProperties("hasGraph")
  lazy val OMF_IS_GRAPH_OF_ENTITY = omfModelObjectProperties("isGraphOfEntity")

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

  val immutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, types.ImmutableModelTerminologyGraph]()
  val mutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, types.MutableModelTerminologyGraph]()

  val directExtensionAxioms = scala.collection.mutable.HashSet[types.TerminologyGraphDirectExtensionAxiom]()
  val directNestingAxioms = scala.collection.mutable.HashSet[types.TerminologyGraphDirectNestingAxiom]()

  val nestedChild2NestingParent =
    scala.collection.mutable.HashMap[types.ModelTerminologyGraph, types.ModelTerminologyGraph]()
  val nestingParent2NestedChildren =
    scala.collection.mutable.HashMap[
      types.ModelTerminologyGraph,
      scala.collection.mutable.HashSet[types.ModelTerminologyGraph]]().
      withDefaultValue(scala.collection.mutable.HashSet[types.ModelTerminologyGraph]())

  def createTerminologyGraphDirectNestingAxiom
  (parentG: types.MutableModelTerminologyGraph,
   childG: types.ModelTerminologyGraph)
  : Try[types.TerminologyGraphDirectNestingAxiom] =
  nestedChild2NestingParent.get(childG) match {
    case Some(_) =>
      Failure(new IllegalArgumentException(s"child graph is already nested"))
    case None =>
      val a = types.TerminologyGraphDirectNestingAxiom(
        nestedChild=childG,
        nestingParent=parentG)
      directNestingAxioms += a
      nestedChild2NestingParent += (childG -> parentG)
      nestingParent2NestedChildren(parentG) += childG
      Success(a)
  }

  def addNestedTerminologyGraph
  (o: OWLOntology,
   axiom: types.TerminologyGraphDirectNestingAxiom)
  : Try[Unit] = {
    val parentI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(axiom.nestingParent)
    val nestedI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(axiom.nestedChild)
    val directNestingI = owlDataFactory.getOWLNamedIndividual(
      makeMetadataInstanceIRI(o, "DN", OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM2Instance))
    OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM2Instance += (axiom -> directNestingI)
    for {
      change <- Seq(
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLDeclarationAxiom(directNestingI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLClassAssertionAxiom(
            OMF_TERMINOLOGY_GRAPH_DIRECT_NESTING_AXIOM, directNestingI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(
            OMF_HAS_DIRECT_NESTING, directNestingI, parentI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(
            OMF_HAS_DIRECT_NESTED_CHILD, directNestingI, nestedI))))
    } require(change == ChangeApplied.SUCCESSFULLY)
    Success(Unit)
  }

  def createTerminologyGraphDirectExtensionAxiom
  (extendingG: types.MutableModelTerminologyGraph,
   extendedG: types.ModelTerminologyGraph)
  : Try[types.TerminologyGraphDirectExtensionAxiom] =
    extendingG.addTerminologyGraphExtension(extendedG) match {
      case Failure(f) =>
        Failure(f)
      case Success(a) =>
        directExtensionAxioms += a
        Success(a)
    }

  def createOMFModelTerminologyGraphExtension
  (o: OWLOntology,
   axiom: types.TerminologyGraphDirectExtensionAxiom)
  : Try[Unit] = {
    val extendingI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(axiom.extendingChild)
    val extendedI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(axiom.extendedParent)
    val directImportingI = owlDataFactory.getOWLNamedIndividual(
      makeMetadataInstanceIRI(o, "DI", OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM2Instance))
    OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM2Instance += (axiom -> directImportingI)
    System.out.println(
      s"""createOMFModelTerminologyGraphExtension:
         |extending: ${axiom.extendingChild.iri}
         |extended: ${axiom.extendedParent.iri}""".stripMargin)
    for {
      change <- Seq(
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLDeclarationAxiom(directImportingI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLClassAssertionAxiom(OMF_TERMINOLOGY_GRAPH_DIRECT_EXTENSION_AXIOM, directImportingI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(
            OMF_HAS_DIRECT_EXTENDED_PARENT, directImportingI, extendedI))),
        ontManager.applyChange(new AddAxiom(o,
          owlDataFactory.getOWLObjectPropertyAssertionAxiom(
            OMF_HAS_DIRECT_EXTENSIONING_CHILD, directImportingI, extendingI))))
    } require(change == ChangeApplied.SUCCESSFULLY)
    Success(Unit)
  }

  def getNestingGraph
  ( g: types.ModelTerminologyGraph )
  : Option[types.ModelTerminologyGraph] =
    nestedChild2NestingParent.get(g)

  def getNestedGraphs
  ( g: types.ModelTerminologyGraph )
  : Iterable[types.ModelTerminologyGraph] =
    nestingParent2NestedChildren(g)

  def fromTerminologyGraph
  ( g: types.ModelTerminologyGraph )
  : OWLAPITerminologyGraphSignature =
  g.fromTerminologyGraph(
    nestedChild2NestingParent.get(g),
    nestingParent2NestedChildren(g))


  // OMF Ontology Instance Model Constructors  

  val owlDataFactory = ontManager.getOWLDataFactory

  def makeMetadataInstanceIRI
  (o: OWLOntology, instanceKind: String, map: scala.collection.Map[_, OWLNamedIndividual])
  : IRI =
    omfModule.ops.withFragment(o.getOntologyID.getOntologyIRI.get, instanceKind + map.size).get

  def registerOMFModelTerminologyGraphMapping
  (o: OWLOntology,
   graphT: types.ModelTerminologyGraph)
  : Try[Unit] = {

    def registerTBoxGraphDefinitions
    (tboxT: types.ModelTerminologyGraph)
    : Try[Unit] =
      if (OMF_MODEL_TERMINOLOGY_GRAPH2Instance.contains(tboxT))
        Success(Unit)
      else {
        val tboxI =
          owlDataFactory.getOWLNamedIndividual(
            makeMetadataInstanceIRI(o, "G", OMF_MODEL_TERMINOLOGY_GRAPH2Instance))
        OMF_MODEL_TERMINOLOGY_GRAPH2Instance += (tboxT -> tboxI)
        ontManager.applyChange(new AddAxiom(
          o, owlDataFactory.getOWLDeclarationAxiom(tboxI)))
        ontManager.applyChange(new AddAxiom(
          o, owlDataFactory.getOWLClassAssertionAxiom(OMF_MODEL_TERMINOLOGY_GRAPH, tboxI)))
        ontManager.applyChange(new AddAxiom(
          o, owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_IRI, tboxI, tboxT.iri.toString)))
        //System.out.println( s"*** Begin registering ${tboxT.iri} as ${tboxI}..." )
        //val ( _, _e, _, _, _a, _c, _r, _sc, _st, _esc, _est, _ssc, _sst, _ax ) = omfModule.ops.fromTerminologyGr taph(boxT )
        val gSignature = fromTerminologyGraph(tboxT)
        require(gSignature.entityGraphIRI.isEmpty)
        gSignature.aspects foreach (registerOMFModelEntityAspectInstance(o, tboxT, _))
        gSignature.concepts foreach (registerOMFModelEntityConceptInstance(o, tboxT, _))
        gSignature.reifiedRelationships foreach (registerOMFModelEntityReifiedRelationshipInstance(o, tboxT, _))
        //System.out.println( s"*** Finished registering ${tboxT.iri} as ${tboxI}" )
        Success(Unit)
      }

    def registerTBoxGraph
    (queue: List[types.ModelTerminologyGraph],
     ready: List[types.ModelTerminologyGraph],
     visited: List[types.ModelTerminologyGraph])
    : Try[Unit] = {

      //      System.out.println(
      // s"*** registerTBoxGraph(queue=${queue.size}, ready=${ready.size}, visited=${visited.size})" )
      //      queue.foreach( q => System.out.println( s"queue: ${q.iri}" ) )
      //      ready.foreach( r => System.out.println( s"ready: ${r.iri}" ) )
      //      visited.foreach( v => System.out.println( s"visited: ${v.iri}" ) )
      queue match {
        case Nil =>
          ready match {
            case Nil =>
              Success(Unit)
            case g :: gs =>
              if (visited.contains(g)) {
                //System.out.println( s"- visited: ${g.iri}" )
                registerTBoxGraph(Nil, gs, visited)
              } else
                for {
                  _ <- registerTBoxGraphDefinitions(g)
                  _ <- registerTBoxGraph(Nil, gs, g :: visited)
                } yield ()
          }
        case g :: gs =>
          if (ready.contains(g)) {
            //System.out.println( s"- skip: ${g.iri}" )
            registerTBoxGraph(gs, ready, visited)
          } else
            for {
              _ <- registerTBoxGraph(g.imports.toList ::: gs, g :: ready, visited)
            } yield {
              for {
                gImported <- g.imports
                ok = omfModule.ops.addTerminologyGraphExtension(o, g, gImported)
              } require(ok.isSuccess)
              ()
            }
      }
    }

    ontManager.applyChange(new AddImport(
      o, owlDataFactory.getOWLImportsDeclaration(omfModule.omfOntologyIRI)))
    registerTBoxGraph(List(graphT), List(), List())
  }

  def createOMFModelTerminologyGraph
  (o: OWLOntology,
   graphT: types.ModelTerminologyGraph,
   kind: TerminologyKind.TerminologyKind,
   hasName: Option[String],
   hasUUID: Option[String])
  : Try[Unit] = {
    val graphI = owlDataFactory.getOWLNamedIndividual(
      makeMetadataInstanceIRI(o, "G", OMF_MODEL_TERMINOLOGY_GRAPH2Instance))
    OMF_MODEL_TERMINOLOGY_GRAPH2Instance += (graphT -> graphI)
    val okind = kind match {
      case TerminologyKind.isToplevelDefinition=>
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
      ) ++ (hasName match {
        case None =>
          Seq()
        case Some(shortName) =>
          Seq(ontManager.applyChange(new AddAxiom(o,
            owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_SHORT_NAME, graphI, shortName))))
      }) ++ (hasUUID match {
        case None =>
          Seq()
        case Some(id) =>
          Seq(ontManager.applyChange(new AddAxiom(o,
            owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, graphI, id))))
      })
    } require(change == ChangeApplied.SUCCESSFULLY)
    Success(Unit)
  }

  def registerOMFModelEntityAspectInstance
  (o: OWLOntology,
   metadataGraph: types.ModelTerminologyGraph,
   aspectT: types.ModelEntityAspect,
   hasName: Option[String] = None,
   hasUUID: Option[String] = None)
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
          ) ++ (hasName match {
            case None =>
              Seq()
            case Some(shortName) =>
              Seq(ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_SHORT_NAME, aspectI, shortName))))
          }) ++ (hasUUID match {
            case None =>
              Seq()
            case Some(id) =>
              Seq(ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, aspectI, id))))
          })
        } require(change == ChangeApplied.SUCCESSFULLY)
        System.out.println( s"## Aspect: ${aspectT.iri}" )
        Success(aspectI)
    }

  def createOMFModelEntityAspectInstance
  (o: OWLOntology,
   metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: String,
   aspectT: types.ModelEntityAspect,
   hasName: Option[String],
   hasUUID: Option[String])
  : Try[Unit] =
  for {
    _ <- registerOMFModelEntityAspectInstance(o, metadataGraph, aspectT, hasName, hasUUID)
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
   conceptT: types.ModelEntityConcept,
   hasName: Option[String] = None,
   hasUUID: Option[String] = None)
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
          ) ++ (hasName match {
            case None =>
              Seq()
            case Some(shortName) =>
              Seq(ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_SHORT_NAME, conceptI, shortName))))
          }) ++ (hasUUID match {
            case None =>
              Seq()
            case Some(id) =>
              Seq(ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, conceptI, id))))
          })
        } require(change == ChangeApplied.SUCCESSFULLY)
        System.out.println( s"## Concept: ${conceptT.iri}" )
        Success(conceptI)
    }

  def createOMFModelEntityConceptInstance
  (o: OWLOntology,
   metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: String,
   conceptT: types.ModelEntityConcept,
   graphO: Option[types.MutableModelTerminologyGraph],
   hasName: Option[String],
   hasUUID: Option[String],
   isAbstract: Boolean)
  : Try[Unit] =
    for {
      conceptI <- registerOMFModelEntityConceptInstance(o, metadataGraph, conceptT, hasName, hasUUID)
      //    ontManager.applyChange( new AddAxiom(
      // o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
      // OMF_HAS_PROVENANCE_FROM_RULE, conceptI, hasProvenanceFromRule ) ) )
      //    ontManager.applyChange( new AddAxiom(
      // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, conceptI, hasName ) ) )
      //    ontManager.applyChange( new AddAxiom(
      // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, conceptI, hasQualifiedName ) ) )
      //    ontManager.applyChange( new AddAxiom(
      // o, owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, conceptI, hasUUID ) ) )
      _ = graphO match {
        case None => ()
        case Some(graphT) =>
          val graphI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(graphT)
          for {
            change <- Seq(
              ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_GRAPH, conceptI, graphI))),
              ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_IS_GRAPH_OF_ENTITY, graphI, conceptI))))
          } require(change == ChangeApplied.SUCCESSFULLY)
      }
    } yield ()

  def registerOMFModelEntityReifiedRelationshipInstance
  (o: OWLOntology,
   metadataGraph: types.ModelTerminologyGraph,
   relationshipT: types.ModelEntityReifiedRelationship,
   hasName: Option[String] = None,
   hasUUID: Option[String] = None)
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
          ) ++ (hasName match {
            case None =>
              Seq()
            case Some(shortName) =>
              Seq(ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_SHORT_NAME, relationshipI, shortName))))
          }) ++ (hasUUID match {
            case None =>
              Seq()
            case Some(id) =>
              Seq(ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLDataPropertyAssertionAxiom(OMF_HAS_UUID, relationshipI, id))))
          })
        } require(change == ChangeApplied.SUCCESSFULLY)
        //System.out.println( s"## Relationship: ${relationshipT.iri}" )
        Success(relationshipI)
    }

  def createOMFModelEntityReifiedRelationshipInstance
  (o: OWLOntology,
   metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: String,
   relationshipT: types.ModelEntityReifiedRelationship,
   graphO: Option[types.MutableModelTerminologyGraph],
   hasName: Option[String],
   hasUUID: Option[String],
   isAbstract: Boolean)
  : Try[Unit] =
    for {
      relationshipI <- registerOMFModelEntityReifiedRelationshipInstance(o,
        metadataGraph, relationshipT, hasName, hasUUID)
      sourceI = OMF_MODEL_ENTITY_DEFINITION2Instance(relationshipT.source)
      targetI = OMF_MODEL_ENTITY_DEFINITION2Instance(relationshipT.target)
      _ = for {
        change <- Seq(
          ontManager.applyChange(new AddAxiom(o,
            owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_SOURCE, relationshipI, sourceI))),
          ontManager.applyChange(new AddAxiom(o,
            owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_TARGET, relationshipI, targetI)))
      //    ontManager.applyChange( new AddAxiom( o,
      //     owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_PROVENANCE_FROM_RULE, relationshipI, hasProvenanceFromRule ) ) )
      //    ontManager.applyChange( new AddAxiom( o,
      //     owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_NAME, relationshipI, hasName ) ) )
      //    ontManager.applyChange( new AddAxiom( o,
      //     owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_QUALIFIED_NAME, relationshipI, hasQualifiedName ) ) )
      //    ontManager.applyChange( new AddAxiom( o,
      //     owlDataFactory.getOWLDataPropertyAssertionAxiom( OMF_HAS_UUID, relationshipI, hasUUID ) ) )
        ) ++ (graphO match {
          case None =>
            Seq()
          case Some(graphT) =>
            val graphI = OMF_MODEL_TERMINOLOGY_GRAPH2Instance(graphT)
            Seq(
              ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_GRAPH, relationshipI, graphI))),
              ontManager.applyChange(new AddAxiom(o,
                owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_IS_GRAPH_OF_ENTITY, graphI, relationshipI))))
        })
      } require(change == ChangeApplied.SUCCESSFULLY)
    } yield ()

  def createOMFEntityDefinitionAspectSubClassAxiomInstance
  (o: OWLOntology,
   metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: String,
   axiomT: types.EntityDefinitionAspectSubClassAxiom,
   subT: types.ModelEntityDefinition,
   supT: types.ModelEntityAspect)
  : Try[Unit] = {
    val subI = OMF_MODEL_ENTITY_DEFINITION2Instance(subT)
    val supI = OMF_MODEL_ENTITY_ASPECT2Instance(supT)
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
    Success(Unit)
  }

  def createOMFEntityConceptSubClassAxiomInstance
  (o: OWLOntology,
   metadataGraph: types.MutableModelTerminologyGraph,
   hasProvenanceFromRule: String,
   axiomT: types.EntityConceptSubClassAxiom,
   subT: types.ModelEntityConcept,
   supT: types.ModelEntityConcept)
  : Try[Unit] = {
    val subI = OMF_MODEL_ENTITY_CONCEPT2Instance(subT)
    val supI = OMF_MODEL_ENTITY_CONCEPT2Instance(supT)
    val axiomI = owlDataFactory.getOWLNamedIndividual(makeMetadataInstanceIRI(o,
      "ConceptSubClass", OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance))
    OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM2Instance += (axiomT -> axiomI)
    for {
      change <- Seq(
        ontManager.applyChange(new AddAxiom(
        o, owlDataFactory.getOWLDeclarationAxiom(axiomI))),
        ontManager.applyChange(new AddAxiom(o,
        owlDataFactory.getOWLObjectPropertyAssertionAxiom(
          OMF_DIRECTLY_ASSERTS_AXIOM,
          OMF_MODEL_TERMINOLOGY_GRAPH2Instance(metadataGraph),
          axiomI))),
        ontManager.applyChange(new AddAxiom(
        o, owlDataFactory.getOWLClassAssertionAxiom(OMF_ENTITY_CONCEPT_SUB_CLASS_AXIOM, axiomI))),
        ontManager.applyChange(new AddAxiom(
        o, owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_GENERAL_CONCEPT, axiomI, supI))),
        ontManager.applyChange(new AddAxiom(
        o, owlDataFactory.getOWLObjectPropertyAssertionAxiom(OMF_HAS_SPECIFIC_CONCEPT, axiomI, subI)))
    //    ontManager.applyChange( new AddAxiom(
    //   o, owlDataFactory.getOWLDataPropertyAssertionAxiom(
    //    OMF_HAS_PROVENANCE_FROM_RULE, axiomI, hasProvenanceFromRule ) ) )
      )
    } require(change == ChangeApplied.SUCCESSFULLY)
    System.out.println("# OMF/OWLAPI store: ConceptSubClass:"+
      " sup="+supI.getIRI+
      " sub="+subI.getIRI)
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
    //val ( iri, _e, _k, _i, _a, _c, _r, _sc, _st, _esc, _est, _ssc, _sst, _ax ) = fromTerminologyGraph( g )
    val tgraph = fromTerminologyGraph(g)
    if (immutableTBoxGraphs.contains(g.iri))
      Failure(new IllegalArgumentException(
        s"There is already an immutable terminology graph with IRI='${g.iri}'"))
    else {
      val nestedG = tgraph.nested map {
        case i: types.ImmutableModelTerminologyGraph => i
        case m: types.MutableModelTerminologyGraph =>
          this.asImmutableTerminologyGraph(m) match {
            case Failure(t) => return Failure(t)
            case Success(i) => i
          }
      }
      val ig = types.ImmutableModelTerminologyGraph(
        tgraph.kind,
        tgraph.imports,
        g.ont,
        tgraph.entityGraphIRI,
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
      immutableTBoxGraphs.put(g.iri, ig)
      Success(ig)
    }
  }

  protected def registerImmutableOntologyAsTerminologyGraph
  (o: OWLOntology,
   extendedTGraphs: Iterable[types.ImmutableModelTerminologyGraph] = Nil)
  (implicit ops: OWLAPIOMFOps)
  : Try[types.ImmutableModelTerminologyGraph] = {
    val iri = o.getOntologyID.getOntologyIRI
    if (!iri.isPresent)
      Failure(new IllegalArgumentException("An ontology must have an OntologyID with an Ontology IRI"))
    else
      immutableTBoxGraphs.get(iri.get) match {
        case Some(g) =>
          // already registered.
          Success(g)

        case None =>
          // not yet registered.

          val importedOrExtendedTGraphs = scala.collection.mutable.HashSet[types.ImmutableModelTerminologyGraph]()

          o.getDirectImports foreach (registerImmutableOntologyAsTerminologyGraph(_) match {
            case Failure(t) =>
              return Failure(t)

            case Success(ig) =>
              importedOrExtendedTGraphs.add(ig)
          })

          extendedTGraphs.foreach { tg =>
            immutableTBoxGraphs.get(tg.iri) match {
              case None => return Failure(new IllegalArgumentException(
                s"""Cannot create an ontology with iri='${iri.get}' extending a foreign terminology graph,
                    |'${tg.iri}' not managed by this ontology manager""".stripMargin))
              case Some(eg) =>
                importedOrExtendedTGraphs.add(eg)
                val decl = ontManager.getOWLDataFactory.getOWLImportsDeclaration(tg.iri)
                ontManager.applyChange(new AddImport(o, decl))
            }
          }

          for {
            g <- types.ImmutableModelTerminologyGraphResolver(
              ResolverHelper(importedOrExtendedTGraphs, o, ops)).resolve
          } yield {
            immutableTBoxGraphs.put(iri.get, g)
            g
          }
      }
  }

  protected def registerMutableOntologyAsTerminologyGraph
  (o: OWLOntology,
   kind: TerminologyKind,
   entity: Option[IRI])
  (implicit ops: OWLAPIOMFOps)
  : Try[types.MutableModelTerminologyGraph] = {
    val iri = o.getOntologyID.getOntologyIRI
    if (!iri.isPresent)
      Failure(new IllegalArgumentException("An ontology must have an OntologyID with an Ontology IRI"))
    else
      mutableTBoxGraphs.get(iri.get) match {
        case Some(g) =>
          // already registered.
          Success(g)

        case None =>
          // not yet registered.
          val g = new types.MutableModelTerminologyGraph(
            kind = kind,
            o,
            entity)
          mutableTBoxGraphs.put(iri.get, g)
          Success(g)
      }
  }

  def saveTerminologyGraph
  (g: types.MutableModelTerminologyGraph)
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
  (g: types.MutableModelTerminologyGraph, os: OutputStream)
  (implicit ops: OWLAPIOMFOps)
  : Try[Unit] =
    g.save(os)

  def loadTerminologyGraph
  (iri: IRI)
  (implicit ops: OWLAPIOMFOps)
  : Try[types.ImmutableModelTerminologyGraph] =
    immutableTBoxGraphs.get(iri) match {
      case Some(o) => Success(o)
      case None =>
        try {
          val o =
            if (ontManager.contains(iri)) ontManager.getOntology(iri)
            else ontManager.loadOntology(iri)
          //System.out.println(
          // s"loadTerminologyGraph: iri=${iri}, o=${o.getOWLOntologyManager.getOntologyDocumentIRI( o )}" )
          registerImmutableOntologyAsTerminologyGraph(o)
        } catch {
          case t: OWLOntologyCreationException =>
            Failure(t.fillInStackTrace)
        }
    }

  def makeTerminologyGraph
  (iri: IRI,
   kind: TerminologyKind,
   entityGraphIRI: Option[IRI])
  (implicit ops: OWLAPIOMFOps)
  : Try[types.MutableModelTerminologyGraph] =
    if (ontManager.contains(iri))
      Failure(new IllegalArgumentException(s"An ontology with iri='$iri' already exists"))
    else
      for {
        b <- Backbone.createBackbone(ontManager.createOntology(iri), kind, omfModule.ops)
        g <- registerMutableOntologyAsTerminologyGraph(b.ont, b.kind, entityGraphIRI)
      } yield g

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