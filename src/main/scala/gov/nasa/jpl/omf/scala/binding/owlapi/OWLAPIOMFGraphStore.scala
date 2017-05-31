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
import java.lang.System
import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.AnnotationProperty
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader._
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{ImmutableModule, Module, MutableModule}
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.{ImmutableDescriptionBox, MutableDescriptionBox, toImmutableDescriptionBoxSignature}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{terminologies, terminologyAxioms}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms._
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.OMLString.LocalName
import gov.nasa.jpl.omf.scala.core.builtin.BuiltInDatatypeMaps
import gov.nasa.jpl.omf.scala.core.TerminologyKind
import gov.nasa.jpl.omf.scala.core._
import org.apache.xml.resolver.tools.CatalogResolver
import org.apache.xml.resolver.Catalog
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters._
import org.semanticweb.owlapi.util.PriorityCollection

import scala.collection.immutable.{Set, _}
import scala.collection.JavaConverters._
import scala.util.control.Exception._
import scala.{Boolean, None, Option, Some, StringContext, Unit}
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._

case class OWLAPIOMFGraphStore
(omfModule: OWLAPIOMFModule,
 ontManager: OWLOntologyManager,
 catalogIRIMapper: CatalogIRIMapper)
extends OWLAPIOMFGraphStoreMetadata(omfModule, ontManager) {

  require(null != omfModule)
  require(null != ontManager)

  val LOG: Boolean =
    "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.GraphStore")

  implicit val ops = omfModule.ops

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
   relativeIRI: Option[String])
  : Seq[OWLOntologyChange]
  = relativeIRI.fold[Seq[OWLOntologyChange]](Seq.empty) { rIRI =>
    Seq(
      new AddOntologyAnnotation(
        o,
        owlDataFactory
          .getOWLAnnotation(ANNOTATION_HAS_RELATIVE_IRI, owlDataFactory.getOWLLiteral(rIRI)))
    )
  }

  lazy val ANNOTATION_HAS_IRI_HASH_PREFIX: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasIRIHashPrefix)

  def createAddOntologyHasIRIHashPrefixAnnotation
  (o: OWLOntology,
   iriHashPrefix: String)
  : AddOntologyAnnotation
  = new AddOntologyAnnotation(
    o,
    owlDataFactory
      .getOWLAnnotation(ANNOTATION_HAS_IRI_HASH_PREFIX, owlDataFactory.getOWLLiteral(iriHashPrefix)))

  lazy val ANNOTATION_HAS_IRI_HASH_SUFFIX: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasIRIHashSuffix)

  def createAddOntologyHasIRIHashSuffixAnnotation
  (o: OWLOntology,
   iriHashSuffix: String)
  : AddOntologyAnnotation
  = new AddOntologyAnnotation(
    o,
    owlDataFactory
      .getOWLAnnotation(ANNOTATION_HAS_IRI_HASH_SUFFIX, owlDataFactory.getOWLLiteral(iriHashSuffix)))

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


  override def setOMFMetadataOntology(o: OWLOntology): Unit = {
    super.setOMFMetadataOntology(o)
    loadBuiltinDatatypeMap().fold[Unit](
      l = (errors: Throwables) =>
        throw errors.toIterator.next(),
      r = (_) =>
        ()
    )
  }

  @scala.volatile
  private var builtInDatatypeMap
  : Option[BuiltInDatatypeMap]
  = None

  def isBuiltInDatatypeMapConstructed
  : Boolean
  = builtInDatatypeMap.isEmpty

  def getBuildInDatatypeMap
  : BuiltInDatatypeMap
  = builtInDatatypeMap match {
    case Some(drc) =>
      drc
    case None =>
      BuiltInDatatypeMaps.DataRangeCategories[OWLAPIOMF]()
  }

  def isAnyAtomicType
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = getBuildInDatatypeMap.anyAtomicType match {
    case Some(any) =>
      dr == any
    case None =>
      ops.getTermIRI(dr).toString == "http://www.w3.org/2001/XMLSchema#anyAtomicType"
  }

  def isNumericKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = getBuildInDatatypeMap.isNumericKind(dr)(this.ops, this)

  def isStringKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = getBuildInDatatypeMap.isStringKind(dr)(this.ops, this)

  def isPlainLiteralKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = getBuildInDatatypeMap.isPlainLiteralKind(dr)(this.ops, this)

  def isBinaryKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = getBuildInDatatypeMap.isBinaryKind(dr)(this.ops, this)

  def isIRIKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = getBuildInDatatypeMap.isIRIKind(dr)(this.ops, this)

  def isTimeKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = getBuildInDatatypeMap.isTimeKind(dr)(this.ops, this)

  def makeW3CTerminologyGraphDefinition
  (iri: IRI)
  : Throwables \/ MutableTerminologyBox
  = for {
    name <- ops.lastSegment(iri)
    uuid = generateUUID(ops.fromIRI(iri))
    g <- ops.makeTerminologyGraphWithPath(uuid, name,
      iri,
      relativeIRIPath = Option.empty[String],
      relativeIRIHashPrefix = Option.empty[String],
      TerminologyKind.isDefinition,
      extraProvenanceMetadata =
        Some(OTI2OMFModelTerminologyGraphProvenance(
          provenanceKind = OMFModelTerminologyGraphW3CProvenanceKind,
          provenanceURI = iri.toString)))(this)
  } yield g

  def loadBuiltinDatatypeMap
  ()
  : Throwables \/ BuiltInDatatypeMap
  = builtInDatatypeMap.fold {
      BuiltInDatatypeMaps
        .createBuiltInDatatypeMaps[OWLAPIOMF](makeW3CTerminologyGraphDefinition)(ops, this)
        .map { drc =>
          require(builtInDatatypeMap.isEmpty)
          builtInDatatypeMap = Some(drc)
          drc
        }
    } { drc =>
      \/-(drc)
    }

  def isBuiltinDatatypeMap
  (m: Module)
  : Option[Module]
  = getBuildInDatatypeMap.builtInDatatypeModules.find { _.uuid == m.uuid }

  def isBuiltinDatatypeMap
  (iri: IRI)
  : Option[Module]
  = getBuildInDatatypeMap.builtInDatatypeModules.find { _.iri == iri }

  protected val directBundlingAxioms = scala.collection.mutable.HashMap[
    Bundle,
    scala.collection.mutable.HashSet[BundledTerminologyAxiom]]()
    .withDefaultValue(scala.collection.mutable.HashSet[BundledTerminologyAxiom]())

  protected val directDesignationAxioms = scala.collection.mutable.HashMap[
    TerminologyBox,
    ConceptDesignationTerminologyAxiom]()

  protected val directExtensionAxioms = scala.collection.mutable.HashMap[
    TerminologyBox,
    scala.collection.mutable.HashSet[TerminologyExtensionAxiom]]()
    .withDefaultValue(scala.collection.mutable.HashSet[TerminologyExtensionAxiom]())

  /**
    * TerminologyGraphDirectNestingAxiom(nestingParent=G1, nestingContext=C, nestedChild=G2)
    *
    * key = nestedChild
    * value = (nestingParent, axiom)
    */
  protected val directNestingAxioms =
    scala.collection.mutable.HashMap[TerminologyBox, TerminologyNestingAxiom]()

  protected val directNestedAxioms = scala.collection.mutable.HashMap[
    TerminologyBox,
    scala.collection.mutable.HashSet[TerminologyNestingAxiom]]()
    .withDefaultValue(scala.collection.mutable.HashSet[TerminologyNestingAxiom]())

  protected val extendingChild2ExtendedParents =
    scala.collection.mutable.HashMap[
      TerminologyBox,
      scala.collection.mutable.HashSet[TerminologyBox]]()
      .withDefaultValue(scala.collection.mutable.HashSet[TerminologyBox]())

  def applyModelTermAxiomChanges[AX <: types.Axiom]
  (ax: AX,
   title: String,
   changes: Seq[OWLAxiomChange])
  : types.UnitNES
  = changes.foldLeft[types.UnitNES](types.rightUnitNES) { case (acc, change) =>
    acc.flatMap { _ =>
      val result = ontManager.applyChange(change)
      if (result == ChangeApplied.UNSUCCESSFULLY)
        -\/(Set(OMFError.omfError(s"$title\naxiom=$ax\nfailed change=$change")))
      else
        \/-(())
    }
  }

  def lookupBundledTerminologyAxioms
  (g: TerminologyBox)
  : Set[BundledTerminologyAxiom]
  = g match {
    case b: Bundle =>
      directBundlingAxioms.getOrElse(b, Set.empty[BundledTerminologyAxiom]).to[Set]
    case _ =>
      Set.empty[BundledTerminologyAxiom]
  }

  def lookupDesignationAxiom
  (g: TerminologyBox)
  : Option[ConceptDesignationTerminologyAxiom]
  = directDesignationAxioms.get(g)

  /**
    * Find the axiom TerminologyGraphDirectNestingAxiom(nestedChild==nestedG), if any.
    */
  def lookupNestingAxiomForNestedChildIfAny
  (nestedG: TerminologyBox)
  : Option[TerminologyNestingAxiom]
  = directNestingAxioms.get(nestedG)

  /**
    * Find the axioms TerminologyGraphDirectNestingAxiom(nestingContext=nestingC)
    */
  def lookupNestingAxiomsForNestingContext
  (nestingC: Concept)
  : Set[TerminologyNestingAxiom]
  = directNestingAxioms.values.filter(_.nestingContext == nestingC).to[Set]

  /**
    * Find the axioms TerminologyGraphDirectNestingAxiom(nestingParent=nestingG)
    */
  def lookupNestingAxiomsForNestingParent
  (nestingG: TerminologyBox)
  : Set[TerminologyNestingAxiom]
  = directNestedAxioms
    .getOrElseUpdate(nestingG, scala.collection.mutable.HashSet[TerminologyNestingAxiom]())
    .to[Set]

  def getNestingParentGraphOfAxiom
  (axiom: TerminologyNestingAxiom)
  : TerminologyBox
  = {
    val nestingParent = directNestingAxioms.find(_._2 == axiom).map(_._1)
    require(nestingParent.isDefined)
    nestingParent.get
  }

  def getNestingContextConceptOfAxiom
  (axiom: TerminologyNestingAxiom)
  : Concept
  = axiom.nestingContext

  // TerminologyNestingAxiom

  def createOMFTerminologyGraphDirectNestingAxiom
  (uuid: UUID,
   parentG: TerminologyBox,
   parentC: Concept,
   childG: TerminologyBox)
  : Throwables \/ TerminologyNestingAxiom
  = lookupNestingAxiomForNestedChildIfAny(childG)
    .fold[Throwables \/ TerminologyNestingAxiom] {

    for {
      axiom <-
      registerTerminologyGraphDirectNestingAxiom(
        childG,
        terminologyAxioms.TerminologyNestingAxiom(uuid, childG.uuid, parentG, parentC))
    } yield {
      directNestingAxioms += (childG -> axiom)
      directNestedAxioms
        .getOrElseUpdate(
          axiom.nestingTerminology,
          scala.collection.mutable.HashSet[TerminologyNestingAxiom]()) += axiom
      axiom
    }
  } { _ =>
    Set(
      OMFError
        .omfOpsError(ops, s"createTerminologyGraphDirectNestingAxiom inconsistency")
    ).left
  }

  // TerminologyExtensionAxiom

  def getExtensionAxioms
  (extendingChildG: TerminologyBox)
  : Set[TerminologyExtensionAxiom]
  = directExtensionAxioms
    .getOrElse(extendingChildG, Set.empty[TerminologyExtensionAxiom])
    .to[Set]

  def getDirectlyExtendingGraphsOfExtendedParentGraph
  (extendedParentG: TerminologyBox)
  : Map[TerminologyBox, TerminologyExtensionAxiom]
  = directExtensionAxioms
    .flatMap { case (g, axs) =>
      axs
        .find { ax => ax.extendedTerminology == extendedParentG }
        .map { ax => (g, ax) }
    }
    .toMap

  def createOMFTerminologyGraphDirectExtensionAxiom
  (uuid: UUID,
   extendingG: TerminologyBox,
   extendedG: TerminologyBox)
  : Throwables \/ TerminologyExtensionAxiom
  = {
    val extendedParents =
      extendingChild2ExtendedParents
        .getOrElseUpdate(extendingG, scala.collection.mutable.HashSet[TerminologyBox]())

    val result
    : Throwables \/ TerminologyExtensionAxiom
    = if (extendedParents.contains(extendedG)) {
      directExtensionAxioms
        .getOrElseUpdate(extendingG, scala.collection.mutable.HashSet[TerminologyExtensionAxiom]())
        .find { ax => ax.extendedTerminology.kindIRI == extendedG.kindIRI }
        .fold[Throwables \/ TerminologyExtensionAxiom] {
        System.out.println(s"directExtensionAxioms: ${directExtensionAxioms.size}")
        directExtensionAxioms.foreach { case (g, axs) =>
          System.out.println(s"=> extending: ${g.kindIRI} extended: ${axs.size}")
          System.out.println(axs.map(_.extendedTerminology.kindIRI.toString).mkString("\n  extended:", "\n  extended:", "\n"))
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
      } { ax =>
        \/-(ax)
      }
    } else {

      for {
        axiom <-
        registerTerminologyGraphDirectExtensionAxiom(extendingG,
          TerminologyExtensionAxiom(uuid, extendingG.uuid, extendedTerminology = extendedG))
      } yield {
        val extendedParents =
          extendingChild2ExtendedParents
            .getOrElseUpdate(extendingG, scala.collection.mutable.HashSet[TerminologyBox]())

        for {
          added <- Seq(
            directExtensionAxioms
              .getOrElseUpdate(extendingG, scala.collection.mutable.HashSet[TerminologyExtensionAxiom]())
              .add(axiom),
            extendedParents.add(axiom.extendedTerminology))
        } require(added)
        axiom
      }
    }

    result
  }


  // BundledTerminologyAxiom

  def getBundlingAxioms
  (terminologyBundle: Bundle)
  : Set[BundledTerminologyAxiom]
  = directBundlingAxioms
    .getOrElse(terminologyBundle, Set.empty[BundledTerminologyAxiom])
    .to[Set]

  def getDirectBundlesOfBundledTerminology
  (bundledTerminology: TerminologyBox)
  : Map[TerminologyBox, BundledTerminologyAxiom]
  = directBundlingAxioms
    .flatMap { case (g, axs) =>
      axs
        .find { ax => ax.bundledTerminology == bundledTerminology }
        .map { ax => (g, ax) }
    }
    .toMap

  def createOMFBundledTerminologyAxiom
  (uuid: UUID,
   terminologyBundle: MutableBundle,
   bundledTerminology: TerminologyBox)
  : Throwables \/ BundledTerminologyAxiom
  = {
    val bundledTerminologies = getBundlingAxioms(terminologyBundle).map(_.bundledTerminology)

    val result
    : Throwables \/ BundledTerminologyAxiom
    = if (bundledTerminologies.contains(bundledTerminology)) {
      Set(
        OMFError
          .omfOpsError(ops, "Duplicate BundledTerminologyAxiom")
      ).left
    } else {
      for {
        axiom <-
        registerBundledTerminologyAxiom(BundledTerminologyAxiom(uuid, terminologyBundle.uuid, bundledTerminology), terminologyBundle)
      } yield {
        directBundlingAxioms
          .getOrElseUpdate(terminologyBundle, scala.collection.mutable.HashSet[BundledTerminologyAxiom]())
          .add(axiom)
        axiom
      }
    }

    result
  }

  // OMF Ontology Instance Model Constructors

  private val owlDataFactory = ontManager.getOWLDataFactory

  val mDigest = java.security.MessageDigest.getInstance("SHA-256")

  def hashMessage(message: String): String = {
    val result
    : String
    = mDigest.digest(message.getBytes("UTF-8")).map("%02x".format(_)).mkString

    result
  }

  def createOMFTerminologyGraph
  (iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   tboxOnt: OWLOntology,
   kind: TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  : Throwables \/ MutableTerminologyGraph
  = for {
    name <- ops.lastSegment(iri)
    uuid = generateUUID(ops.fromIRI(iri))
    result <- Backbone
      .createBackbone(tboxOnt, kind, ops)
      .flatMap { backbone =>

        val aRelativeIRIPath: Option[String]
        = relativeIRIPath.orElse(iri.toString.stripPrefix("http://").some)

        System.out.println(s"\n*** createOMFTerminologyGraph\n=> iri=$iri\n=> rel=$aRelativeIRIPath")

        for {
          graphT <- terminologies.MutableTerminologyGraph.initialize(
            uuid, name, iri, kind = kind, ont = tboxOnt,
            extraProvenanceMetadata = extraProvenanceMetadata,
            backbone = backbone)(this, ops)
          _ <- createOMFTerminologyGraphMetadata(iri, aRelativeIRIPath, relativeIRIHashPrefix, kind, graphT)
          _ <- applyOntologyChangesOrNoOp(ontManager,
            createAddOntologyHasRelativeIRIAnnotation(tboxOnt, aRelativeIRIPath) ++
              calculateRelativeIRIUnhashedPrefixHashedSuffix(aRelativeIRIPath, relativeIRIHashPrefix)
                .fold[Seq[OWLOntologyChange]](Seq.empty) { case (unhashedPrefix, hashedSuffix) =>
                Seq(
                  createAddOntologyHasIRIHashPrefixAnnotation(tboxOnt, unhashedPrefix),
                  createAddOntologyHasIRIHashSuffixAnnotation(tboxOnt, hashedSuffix)
                )
              },
            "createOMFTerminologyGraph error")
        } yield {
          graphT
        }
      }

  } yield result

  def createOMFBundle
  (iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   tboxOnt: OWLOntology,
   kind: TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  : Throwables \/ MutableBundle
  = for {
    name <- ops.lastSegment(iri)
    uuid = generateUUID(ops.fromIRI(iri))
    result <- Backbone
      .createBackbone(tboxOnt, ops)
      .flatMap { backbone =>

        System.out.println(s"\n*** createOMFBundle\n=> iri=$iri\n=> rel=$relativeIRIPath")

        for {
          graphT <- terminologies.MutableBundle.initialize(
            uuid, name, iri, kind, ont = tboxOnt,
            extraProvenanceMetadata = extraProvenanceMetadata,
            backbone = backbone)(this, ops)

          _ <- createOMFBundleMetadata(iri, relativeIRIPath, relativeIRIHashPrefix, kind, graphT)
          _ <- applyOntologyChangesOrNoOp(ontManager,
            createAddOntologyHasRelativeIRIAnnotation(tboxOnt, relativeIRIPath) ++
              calculateRelativeIRIUnhashedPrefixHashedSuffix(relativeIRIPath, relativeIRIHashPrefix)
                .fold[Seq[OWLOntologyChange]](Seq.empty) { case (unhashedPrefix, hashedSuffix) =>
                Seq(
                  createAddOntologyHasIRIHashPrefixAnnotation(tboxOnt, unhashedPrefix),
                  createAddOntologyHasIRIHashSuffixAnnotation(tboxOnt, hashedSuffix)
                )
              },
            "createOMFBundle error")
        } yield {
          graphT
        }
      }

  } yield result

  def createOMFDescriptionBox
  (iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   dboxOnt: OWLOntology,
   kind: DescriptionKind)
  : Throwables \/ MutableDescriptionBox
  = for {
    name <- ops.lastSegment(iri)
    uuid = generateUUID(ops.fromIRI(iri))
    result <- Backbone
      .createBackbone(dboxOnt, kind, ops)
      .flatMap { backbone =>

        val aRelativeIRIPath: Option[String]
        = relativeIRIPath.orElse(iri.toString.stripPrefix("http://").some)

        System.out.println(s"\n*** createOMFDescriptionBox\n=> iri=$iri\n=> rel=$aRelativeIRIPath")

        for {
          dboxG <- MutableDescriptionBox.initialize(
            uuid, name, iri, kind = kind, ont = dboxOnt, backbone = backbone)(this, ops)
          //_ <- createOMFDescriptionBoxMetadata(iri, aRelativeIRIPath, relativeIRIHashPrefix, kind, dboxG)
          _ <- applyOntologyChangesOrNoOp(ontManager,
            createAddOntologyHasRelativeIRIAnnotation(dboxOnt, aRelativeIRIPath) ++
              calculateRelativeIRIUnhashedPrefixHashedSuffix(aRelativeIRIPath, relativeIRIHashPrefix)
                .fold[Seq[OWLOntologyChange]](Seq.empty) { case (unhashedPrefix, hashedSuffix) =>
                Seq(
                  createAddOntologyHasIRIHashPrefixAnnotation(dboxOnt, unhashedPrefix),
                  createAddOntologyHasIRIHashSuffixAnnotation(dboxOnt, hashedSuffix)
                )
              },
            "createOMFDescriptionBox error")
        } yield {
          dboxG
        }
      }

  } yield result

  object TBoxConversions {

    def convert1
    (acc: OntologyMapping,
     mm: MutableModule)
    (implicit store: OWLAPIOMFGraphStore)
    : Throwables \/ OntologyMapping
    = if (!acc.m2i.containsKey(mm))
      convert1New(acc, mm)
    else
      acc.right

    def convert1New
    (acc: OntologyMapping,
     mm: MutableModule)
    (implicit store: OWLAPIOMFGraphStore)
    : Throwables \/ OntologyMapping
    = {
      require(!acc.m2i.containsKey(mm), s"convert1: acc=${acc.m2i.size}, m=$mm")

      val i_mg_relativePath_value = getModuleRelativeIRIPath(mm)
      i_mg_relativePath_value.fold[Unit](()) { relPath =>
        require(!relPath.endsWith("_Gro"))
        require(!relPath.endsWith("_Grw"))
      }

      val i_mg_iriHashPrefix_value = getModuleIRIHashPrefix(mm)

      mm match {
        case mg: MutableTerminologyGraph =>
          for {
            ig <- convert1NewTerminologyGraph(acc, mg)
            next <- acc.addMappedModule(mg, ig)
            result <- registerTerminologyGraph(ig, next, i_mg_relativePath_value, i_mg_iriHashPrefix_value)
          } yield result
        case mb: MutableBundle =>
          for {
            ib <- convert1NewBundle(acc, mb)
            next <- acc.addMappedModule(mb, ib)
          } yield next
        case md: MutableDescriptionBox =>
          for {
            id <- convert1NewDescriptionBox(acc, md)
            next <- acc.addMappedModule(md, id)
          } yield next
      }
    }

    // need to map ModuleEdges to refer to the corresponding immutable modules

    def convert1NewTerminologyGraph
    (acc: OntologyMapping,
     mg: MutableTerminologyGraph)
    (implicit store: OWLAPIOMFGraphStore)
    : Throwables \/ ImmutableTerminologyGraph
    = ImmutableTerminologyGraph.initialize(
      toImmutableTerminologyBoxSignature(mg.sig),
      mg.ont, mg.extraProvenanceMetadata, mg.backbone)

    def convert1NewBundle
    (acc: OntologyMapping,
     mb: MutableBundle)
    (implicit store: OWLAPIOMFGraphStore)
    : Throwables \/ ImmutableBundle
    = ImmutableBundle.initialize(
      toImmutableTerminologyBoxSignature(mb.sig),
      mb.ont, mb.extraProvenanceMetadata, mb.backbone)

    def convert1NewDescriptionBox
    (acc: OntologyMapping,
     md: MutableDescriptionBox)
    (implicit store: OWLAPIOMFGraphStore)
    : Throwables \/ ImmutableDescriptionBox
    = ImmutableDescriptionBox.initialize(
      toImmutableDescriptionBoxSignature(md.sig),
      md.ont, md.backbone)

    @scala.annotation.tailrec
    def convert
    (acc: OntologyMapping,
     queue: Seq[MutableModule],
     visited: Seq[MutableModule])
    (implicit store: OWLAPIOMFGraphStore)
    : Throwables \/ OntologyMapping
    = {
      if (queue.isEmpty) {
        if (visited.isEmpty)
          \/-(acc)
        else {
          val mg = visited.head

          if (acc.m2i.containsKey(mg))
            convert(acc, queue, visited.tail)
          else
            convert1(acc, mg) match {
              case \/-(acc1) =>
                convert(acc1, queue, visited.tail)
              case -\/(nels) =>
                -\/(nels)
            }
        }
      } else {
        val mg = queue.head

        val extendedTerminologyQueue =
          mg.sig.importedTerminologies
            .flatMap {
              case _: ImmutableTerminologyBox =>
                None
              case me: MutableTerminologyBox =>
                if (queue.contains(me))
                  None
                else if (acc.m2i.containsKey(me))
                  None
                else
                  Some(me)
            }
            .to[Seq]

        val extendedDescriptionQueue =
          mg.sig.importedDescriptions
            .flatMap {
              case _: ImmutableDescriptionBox =>
                None
              case me: MutableDescriptionBox =>
                if (queue.contains(me))
                  None
                else if (acc.m2i.containsKey(me))
                  None
                else
                  Some(me)
            }
            .to[Seq]

        convert(
          acc,
          extendedTerminologyQueue ++ extendedDescriptionQueue ++ queue.tail,
          queue.head +: visited)
      }
    }

  }

  // OMF API
  def asImmutableModule
  (m: MutableModule,
   m2i: Mutable2ImmutableModuleMap)
  : Throwables \/ (ImmutableModule, Mutable2ImmutableModuleMap)
  = for {
    dcr <- loadBuiltinDatatypeMap()
    om = OntologyMapping.initialize(m2i, dcr)
    next <- TBoxConversions.convert(om, Seq(m), Seq())(this)
    i <- next.m2i.get(m) match {
      case Some(im) =>
        im.right[Throwables]
      case None =>
        Set[java.lang.Throwable](OMFError.omfError(
          s"asImmutableModule: Failed to find converted immutable module for ${m.iri}"
        )).left[ImmutableModule]
    }
  } yield i -> next.m2i

  /**
    * Registers an immutable TBox graph in the store's OMF Metadata graph.
    *
    * @note postcondition: `lookupTerminologyGraph(g.iri)` should be `\/-(g)`
    * @param ig                    The immutable TBox graph to register in the store's current OMF Metadata graph
    * @param om                    The current OntologyMapping
    * @param relativeIRIPath       The relativeIRIPath of `g`
    * @param relativeIRIHashPrefix The relativeIRIHashPrefix of `g`
    * @return `om`
    */
  def registerTerminologyGraph
  (ig: ImmutableTerminologyGraph,
   om: OntologyMapping,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String])
  : Throwables \/ OntologyMapping
  = for {
    _ <- registerMetadata(ig, relativeIRIPath, relativeIRIHashPrefix)

    _ <- ig.sig.aspects.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, a: Aspect) =>
        acc +++ registerOMFModelEntityAspectInstance(ig, a).map(_ => ())
    }

    _ <- ig.sig.concepts.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, c: Concept) =>
        acc +++ registerOMFModelEntityConceptInstance(ig, c).map(_ => ())
    }

    _ <- ig.sig.reifiedRelationships.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, r: ReifiedRelationship) =>
        acc +++ registerOMFModelEntityReifiedRelationshipInstance(ig, r).map(_ => ())
    }

    _ <- ig.sig.scalarDataTypes.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, sc: Scalar) =>
        acc +++ registerOMFModelScalarDataTypeInstance(ig, sc).map(_ => ())
    }

    _ <- ig.sig.structuredDataTypes.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, st: Structure) =>
        acc +++ registerOMFModelStructuredDataTypeInstance(ig, st).map(_ => ())
    }

    _ <- ig.sig.axioms.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, axiom: types.Axiom) =>
        acc +++ (axiom match {
          case ax: ConceptSpecializationAxiom =>
            registerOMFEntityConceptSubClassAxiomInstance(ig, ax).map(_ => ())
          case ax: ReifiedRelationshipSpecializationAxiom =>
            registerOMFEntityReifiedRelationshipSubClassAxiomInstance(ig, ax).map(_ => ())
          case ax: AspectSpecializationAxiom =>
            registerOMFEntityDefinitionAspectSubClassAxiomInstance(ig, ax).map(_ => ())
          case ax: EntityUniversalRestrictionAxiom =>
            registerOMFEntityDefinitionUniversalRestrictionAxiomInstance(ig, ax).map(_ => ())
          case ax: EntityExistentialRestrictionAxiom =>
            registerOMFEntityDefinitionExistentialRestrictionAxiomInstance(ig, ax).map(_ => ())
          case ax: EntityScalarDataPropertyExistentialRestrictionAxiom =>
            registerOMFEntityScalarDataPropertyExistentialRestrictionAxiomInstance(ig, ax).map(_ => ())
          case ax: EntityScalarDataPropertyParticularRestrictionAxiom =>
            registerOMFEntityScalarDataPropertyParticularRestrictionAxiomInstance(ig, ax).map(_ => ())
          case ax: EntityScalarDataPropertyUniversalRestrictionAxiom =>
            registerOMFEntityScalarDataPropertyUniversalRestrictionAxiomInstance(ig, ax).map(_ => ())
          case ax =>
            Set(
              OMFError
                .omfError(s"Unrecognized axiom: $ax")).left
        })
    }

    _ <- ig.sig.extensions.foldLeft(types.rightUnitNES) {
      case (acc: types.UnitNES, axiom: TerminologyExtensionAxiom) =>
        axiom.extendedTerminology match {
          case _: ImmutableTerminologyBox =>
            acc +++
              registerTerminologyGraphDirectExtensionAxiom(ig, axiom)
                .map(_ => ())
          case extMT: MutableTerminologyBox =>
            om.m2i.getImmutableTerminologyBox(extMT)(this, ops).fold {
              Set(OMFError.omfError(
                s"""No Immutable graph available for an imported mutable graph:
                   |mutable graph to convert:
                   |$extMT
                   |immutable graph registration:
                   |$ig
                   |""".stripMargin
              )).left[Unit]
            } { extIT =>
              acc +++ registerTerminologyGraphDirectExtensionAxiom(ig,
                axiom.copy(
                  extendedTerminology = extIT))
                .map(_ => ())
            }
        }
    }

    _ <- ig.sig.nesting.foldLeft(types.rightUnitNES) {
      case (acc: types.UnitNES, axiom: TerminologyNestingAxiom) =>
        axiom.nestingTerminology match {
          case _: ImmutableTerminologyGraph =>
            directNestingAxioms += (ig -> axiom)
            directNestedAxioms
              .getOrElseUpdate(
                axiom.nestingTerminology,
                scala.collection.mutable.HashSet[TerminologyNestingAxiom]()) += axiom
            acc +++
              registerTerminologyGraphDirectNestingAxiom(ig, axiom)
                .map(_ => ())
          case npMG: MutableTerminologyGraph =>
            om.m2i.getImmutableTerminologyGraph(npMG)(this, ops).fold {
              Set(OMFError.
                omfError(
                  s"""No Immutable graph available for a nesting parent mutable graph:
                     |mutable graph to convert:
                     |$npMG
                     |immutable graph registration:
                     |$ig
                     |""".stripMargin
                )).left[Unit]
            } { npIG =>
              val iaxiom =
                axiom.copy(
                  nestingTerminology = npIG)
              directNestingAxioms += (ig -> iaxiom)
              directNestedAxioms
                .getOrElseUpdate(
                  axiom.nestingTerminology,
                  scala.collection.mutable.HashSet[TerminologyNestingAxiom]()) += axiom
              acc +++
                registerTerminologyGraphDirectNestingAxiom(ig, iaxiom).map(_ => ())
            }
        }
    }

  } yield om


  def loadModule
  (m2i: Mutable2ImmutableModuleMap,
   iri: IRI)
  : Throwables \/ ImmutableModuleConversionMap
  = loadBuiltinDatatypeMap().flatMap { drc =>
    builtInDatatypeMap = Some(drc)
    OWLAPIOMFLoader.loadModule(iri, m2i, drc)(ops, this)
  }

  type ResolverTupleState =
    (Set[ImmutableModule], Set[MutableModule], Set[MutableModule], Mutable2ImmutableModuleMap)

  def convertModuleFromOntologyDocument
  (s: OntologyLoadedState,
   om: OntologyMapping,
   ontIRI: IRI,
   ont: OWLOntology)
  (implicit ops: OWLAPIOMFOps)
  : Throwables \/ OntologyMapping
  = {

    val builtInImports
    : Set[Module]
    = om.drc.builtInDatatypeModules

    val extendedGraphs
    : Throwables \/ Set[ImmutableModule]
    = s
      .extensions
      .filter(_.extending == ontIRI)
      .foldLeft(Set.empty[ImmutableModule].right[Throwables]) {
        (acc,
         ext: ExtendingOntologyToExtendedModuleIRI) =>
          acc +++
            om.m2i.getImmutableModule(ext.extended)(this, ops).map(i => Set(i))
      }

    val nestingContextAndGraphIfAny
    : types.NestingConceptAndGraphOptionNES
    = s
      .nested2context
      .find(_.nestedG == ontIRI)
      .fold[types.NestingConceptAndGraphOptionNES](types.emptyNestingConceptAndGraphNES) { n2n =>
      om
        .m2i
        .terminologyBoxValues(this, ops)
        .flatMap { nestingParent =>
          ops
            .lookupConcept(nestingParent, n2n.nestingC, recursively = false)(this)
            .map { nestingC => (nestingC, nestingParent) }
        }
        .headOption
        .fold[types.NestingConceptAndGraphOptionNES] {
        -\/(Set(OMFError.omfError(s"Failued to resolve immutable OMF Terminology Graph for $n2n")))
      } { pair =>
        \/-(Some(pair))
      }
    }

    // Add only the annotation properties used that are not imported.

    val annotationProperties
    : Throwables \/ Set[AnnotationProperty]
    = getRelevantOntologyAnnotations(ont)
      .foldLeft(Set.empty[AnnotationProperty].right[Throwables]) {
      case (acc, a) =>
        for {
          as <- acc
          ap = getAnnotationPropertyFromOWLAnnotation(a)
        } yield as + ap
    }

    val result
    : Throwables \/ OntologyMapping
    = for {
      as <- annotationProperties
      extensions <- extendedGraphs
      nesting <- nestingContextAndGraphIfAny
      resolver <- types.immutableModuleResolver(omfMetadata, s, ont, as, extensions, nesting, om, this)
      resolved <- resolver.resolve()
    } yield {
      resolved._2
    }

    result
  }

  def saveOMFMetadataOntology(saveIRI: IRI): Throwables \/ Unit
  = nonFatalCatch[Unit]
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
        .fold[Throwables \/ Unit](
        Set(
          OMFError.omfError(
            s"cannot save OMF Metadata Ontology because it's not yet created.")
        ).left
      ) { ontM =>
        ontManager.saveOntology(ontM, saveIRI).right
      })

  def saveDescription
  (g: descriptions.DescriptionBox)
  (implicit ops: OWLAPIOMFOps)
  : Throwables \/ Unit
  = {
    val iri = catalogIRIMapper.resolveIRI(g.iri, catalogIRIMapper.saveResolutionStrategy)
    g.save(iri)
  }

  def saveDescription
  (g: descriptions.DescriptionBox, os: java.io.OutputStream)
  (implicit ops: OWLAPIOMFOps)
  : Throwables \/ Unit
  = g.save(os)

  def saveTerminology
  (g: TerminologyBox)
  (implicit ops: OWLAPIOMFOps)
  : Throwables \/ Unit
  = {
    val iri = catalogIRIMapper.resolveIRI(g.iri, catalogIRIMapper.saveResolutionStrategy)
    g.save(iri)
  }

  def saveTerminology
  (g: TerminologyBox, os: java.io.OutputStream)
  (implicit ops: OWLAPIOMFOps)
  : Throwables \/ Unit
  = g.save(os)

  def isBuiltInIRI
  (iri: IRI)
  : Boolean
  = "http://www.w3.org/2001/XMLSchema" == iri.toString ||
    "http://www.w3.org/1999/02/22-rdf-syntax-ns" == iri.toString ||
    "http://www.w3.org/2002/07/owl" == iri.toString

  def makeTerminologyGraph
  (uuid: UUID,
   name: LocalName,
   iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  (implicit ops: OWLAPIOMFOps)
  : Throwables \/ MutableTerminologyGraph
  = if (ontManager.contains(iri)) {
    if (isBuiltInIRI(iri))
      createOMFTerminologyGraph(
        iri, relativeIRIPath, relativeIRIHashPrefix,
        ontManager.getOntology(iri), kind, extraProvenanceMetadata)
    else
      Set(
        OMFError
          .omfOpsError(ops, s"makeTerminologyGraph(iri='$iri') --  already exists!")
      ).left
  } else
    createOMFTerminologyGraph(
      iri, relativeIRIPath, relativeIRIHashPrefix,
      ontManager.createOntology(iri), kind, extraProvenanceMetadata)

  def makeBundle
  (uuid: UUID,
   name: LocalName,
   iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  (implicit ops: OWLAPIOMFOps)
  : Throwables \/ MutableBundle
  = if (ontManager.contains(iri)) {
    if (isBuiltInIRI(iri))
      createOMFBundle(
        iri, relativeIRIPath, relativeIRIHashPrefix,
        ontManager.getOntology(iri), kind, extraProvenanceMetadata)
    else
      Set(
        OMFError
          .omfOpsError(ops, s"makeBundle(iri='$iri') --  already exists!")
      ).left
  } else
    createOMFBundle(
      iri, relativeIRIPath, relativeIRIHashPrefix,
      ontManager.createOntology(iri), kind, extraProvenanceMetadata)

  def loadDescriptionBox
  (iri: IRI)
  : Throwables \/ descriptions.ImmutableDescriptionBox
  = ???

  def makeDescriptionBox
  (uuid: UUID,
   name: LocalName,
   iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   k: DescriptionKind)
  : Throwables \/ descriptions.MutableDescriptionBox
  = if (ontManager.contains(iri)) {
    Set(
      OMFError
        .omfOpsError(ops, s"makeDescriptionBox(iri='$iri') --  already exists!")
    ).left[descriptions.MutableDescriptionBox]
  } else
    createOMFDescriptionBox(
      iri, relativeIRIPath, relativeIRIHashPrefix,
      ontManager.createOntology(iri), k)

  def resolveIRIAsLocalFile
  (iri: IRI)
  : Throwables \/ File
  = catalogIRIMapper.resolveIRIAsLocalFile(iri)

}

object OWLAPIOMFGraphStore {

  def initGraphStore
  (omfModule: OWLAPIOMFModule,
   ontManager: OWLOntologyManager,
   catalogResolver: CatalogResolver,
   catalog: Catalog)
  : OWLAPIOMFGraphStore
  = {
    val catalogIRIMapper: CatalogIRIMapper = {
      val mappers: PriorityCollection[OWLOntologyIRIMapper] = ontManager.getIRIMappers
      val mapper = CatalogIRIMapper(omfModule.catalogManager, catalogResolver, catalog)
      mappers.add(Iterable[OWLOntologyIRIMapper](mapper).asJava)
      mapper
    }
    OWLAPIOMFGraphStore(omfModule, ontManager, catalogIRIMapper)
  }

}