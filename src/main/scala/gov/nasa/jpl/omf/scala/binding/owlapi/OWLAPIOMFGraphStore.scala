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

import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, LocalName}
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{ImmutableTerminologyConversionMap, Mutable2ImmutableTerminologyMap, terminologies, terminologyAxioms}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms._
import gov.nasa.jpl.omf.scala.core.builtin.BuiltInDatatypeMaps
import gov.nasa.jpl.omf.scala.core.TerminologyKind
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters._
import org.semanticweb.owlapi.util.PriorityCollection

import scala.collection.immutable._
import scala.collection.JavaConverters._
import scala.util.control.Exception._
import scala.{Boolean, None, Option, Some, StringContext, Tuple3, Unit}
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._

case class OWLAPIOMFGraphStore(omfModule: OWLAPIOMFModule, ontManager: OWLOntologyManager)
extends OWLAPIOMFGraphStoreMetadata(omfModule, ontManager) {

  require(null != omfModule)
  require(null != ontManager)

  protected val allAnnotationProperties = new scala.collection.mutable.TreeSet[AnnotationProperty]()

  def annotationProperties()
  : Seq[AnnotationProperty]
  = allAnnotationProperties.iterator.to[Seq]

  def addAnnotationProperty
  (ap: AnnotationProperty)
  : Set[java.lang.Throwable] \/ AnnotationProperty
  = {
    allAnnotationProperties.add(ap)
    ap.right
  }

  def lookupAnnotationProperty
  (a: OWLAnnotation)
  : Set[java.lang.Throwable] \/ AnnotationProperty
  = {
    val ap = getAnnotationPropertyFromOWLAnnotation(a)
    if (allAnnotationProperties.contains(ap))
      ap.right
    else
      Set[java.lang.Throwable](OMFError.omfError(s"Unregistered annotation property: $ap")).left
  }

  val LOG: Boolean =
    "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.GraphStore")

  implicit val ops = omfModule.ops

  val catalogIRIMapper: CatalogIRIMapper = {
      val mappers: PriorityCollection[OWLOntologyIRIMapper] = ontManager.getIRIMappers
      val mapper = new CatalogIRIMapper(omfModule.catalogManager)
      mappers.add(Iterable[OWLOntologyIRIMapper](mapper).asJava)
      mapper
  }

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
        .getOWLAnnotation( ANNOTATION_HAS_IRI_HASH_PREFIX, owlDataFactory.getOWLLiteral( iriHashPrefix ) ) )

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


  override def setOMFMetadataOntology(o: OWLOntology): Unit = {
    super.setOMFMetadataOntology(o)
    loadBuiltinDatatypeMap().fold[Unit](
      l = (errors: Set[java.lang.Throwable]) =>
        throw errors.toIterator.next(),
      r = (_) =>
        ()
    )
  }

  protected val immutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, ImmutableTerminologyBox]()
  protected val mutableTBoxGraphs = scala.collection.mutable.HashMap[IRI, MutableTerminologyBox]()

  type BuiltInDatatypeMap
  = (ImmutableTerminologyBox, Mutable2ImmutableTerminologyMap, BuiltInDatatypeMaps.DataRangeCategories[OWLAPIOMF])

  @scala.volatile
  private var builtInDatatypeMap
  : Option[BuiltInDatatypeMap]
  = None

  def isNumericKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = builtInDatatypeMap.fold[Boolean](true) { case (_, _, dcr) =>
      dcr.isNumericKind(dr)(this.ops, this)
  }

  def isStringKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = builtInDatatypeMap.fold[Boolean](true) { case (_, _, dcr) =>
    dcr.isStringKind(dr)(this.ops, this)
  }

  def isPlainLiteralKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = builtInDatatypeMap.fold[Boolean](true) { case (_, _, dcr) =>
    dcr.isPlainLiteralKind(dr)(this.ops, this)
  }

  def isBinaryKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = builtInDatatypeMap.fold[Boolean](true) { case (_, _, dcr) =>
    dcr.isBinaryKind(dr)(this.ops, this)
  }

  def isIRIKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = builtInDatatypeMap.fold[Boolean](true) { case (_, _, dcr) =>
    dcr.isIRIKind(dr)(this.ops, this)
  }

  def isTimeKind
  (dr: OWLAPIOMF#DataRange)
  : Boolean
  = builtInDatatypeMap.fold[Boolean](true) { case (_, _, dcr) =>
    dcr.isTimeKind(dr)(this.ops, this)
  }

  def makeW3CTerminologyGraphDefinition
  (iri: IRI)
  : Set[java.lang.Throwable] \/ MutableTerminologyBox
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
  : Set[java.lang.Throwable] \/ ImmutableTerminologyConversionMap
  = {
    builtInDatatypeMap
      .fold[Set[java.lang.Throwable] \/ ImmutableTerminologyConversionMap] {
      BuiltInDatatypeMaps
        .createBuiltInDatatypeMaps[OWLAPIOMF](makeW3CTerminologyGraphDefinition)(ops, this)
        .map { case (i, m2i, dtmap) =>
          require(builtInDatatypeMap.isEmpty)
          builtInDatatypeMap = Some(Tuple3(i, m2i, dtmap))
          require(builtInDatatypeMap.isDefined)
          m2i.values foreach { builtInG =>
            immutableTBoxGraphs += (builtInG.iri -> builtInG)
          }
          i -> m2i
        }
    } { case (i, m2i, dtmap) =>
      \/-(i -> m2i)
    }
  }

  def isBuiltinDatatypeMap
  (tbox: TerminologyBox)
  : Boolean
  = builtInDatatypeMap.fold[Boolean](false) { case (_, m2i, _) =>
    tbox match {
      case mbox: MutableTerminologyBox =>
        m2i.contains(mbox)
      case ibox: ImmutableTerminologyBox =>
        m2i.values.exists(_ == ibox)
    }
  }

  def isBuiltinDatatypeMap
  (iri: IRI)
  : Boolean
  = lookupTerminology(iri).fold[Boolean](false)(isBuiltinDatatypeMap)

  def getBuiltinDatatypeMapTerminologyGraph
  : ImmutableTerminologyBox
  = {
    val result = loadBuiltinDatatypeMap()
    require(result.isRight)
    result.toOption.get._1
  }

  def lookupTerminology
  (iri: IRI)
  : Option[TerminologyBox]
  = {
    val result
    : Option[TerminologyBox]
    = immutableTBoxGraphs.get(iri)
      .orElse(mutableTBoxGraphs.get(iri))

    result
  }

  def lookupImmutableTerminology
  (uuid: UUID)
  : Option[ImmutableTerminologyBox]
  = immutableTBoxGraphs
    .find { case (_, ig) => uuid == ig.uuid }
    .map(_._2)

  def lookupMutableTerminology
  (uuid: UUID)
  : Option[MutableTerminologyBox]
  = mutableTBoxGraphs
    .find { case (_, ig) => uuid == ig.uuid }
    .map(_._2)

  def lookupTerminology
  (uuid: UUID)
  : Option[TerminologyBox]
  = lookupImmutableTerminology(uuid).orElse(lookupMutableTerminology(uuid))

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
  : Set[java.lang.Throwable] \/ TerminologyNestingAxiom
  = lookupNestingAxiomForNestedChildIfAny(childG)
    .fold[Set[java.lang.Throwable] \/ TerminologyNestingAxiom] {

    for {
      axiom <-
      registerTerminologyGraphDirectNestingAxiom(
        childG,
        terminologyAxioms.TerminologyNestingAxiom(uuid, parentG, parentC))
    } yield {
      directNestingAxioms += (childG -> axiom)
      directNestedAxioms
        .getOrElseUpdate(
          axiom.nestingTerminology,
          scala.collection.mutable.HashSet[TerminologyNestingAxiom]()) += axiom
      axiom
    }
  }{ _ =>
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
  : Set[java.lang.Throwable] \/ TerminologyExtensionAxiom
  = {
    val extendedParents =
      extendingChild2ExtendedParents
      .getOrElseUpdate(extendingG, scala.collection.mutable.HashSet[TerminologyBox]())

    val result
    : Set[java.lang.Throwable] \/ TerminologyExtensionAxiom
    = if (extendedParents.contains(extendedG)) {
      directExtensionAxioms
        .getOrElseUpdate(extendingG, scala.collection.mutable.HashSet[TerminologyExtensionAxiom]())
        .find { ax => ax.extendedTerminology.kindIRI == extendedG.kindIRI }
        .fold[Set[java.lang.Throwable] \/ TerminologyExtensionAxiom]{
        System.out.println(s"directExtensionAxioms: ${directExtensionAxioms.size}")
        directExtensionAxioms.foreach { case (g, axs) =>
          System.out.println(s"=> extending: ${g.kindIRI} extended: ${axs.size}")
          System.out.println(axs.map(_.extendedTerminology.kindIRI.toString).mkString("\n  extended:","\n  extended:","\n"))
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
          TerminologyExtensionAxiom(uuid, extendedTerminology = extendedG))
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
  : Set[java.lang.Throwable] \/ BundledTerminologyAxiom
  = {
    val bundledTerminologies = getBundlingAxioms(terminologyBundle).map(_.bundledTerminology)

    val result
    : Set[java.lang.Throwable] \/ BundledTerminologyAxiom
    = if (bundledTerminologies.contains(bundledTerminology)) {
        Set(
          OMFError
            .omfOpsError(ops, "Duplicate BundledTerminologyAxiom")
        ).left
    } else {
      for {
        axiom <-
        registerBundledTerminologyAxiom(BundledTerminologyAxiom(uuid, terminologyBundle, bundledTerminology))
      } yield {
        directBundlingAxioms
          .getOrElseUpdate(axiom.terminologyBundle, scala.collection.mutable.HashSet[BundledTerminologyAxiom]())
          .add(axiom)
        axiom
      }
    }

    result
  }

  //

  def fromTerminology
  (g: TerminologyBox)
  : OWLAPITerminologySignature
  = g.fromTerminology

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
  : Set[java.lang.Throwable] \/ MutableTerminologyGraph
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
            iri, uuid, name, kind = kind, ont = tboxOnt,
            extraProvenanceMetadata = extraProvenanceMetadata,
            backbone = backbone)(this)
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
          mutableTBoxGraphs.put(iri, graphT)
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
  : Set[java.lang.Throwable] \/ MutableBundle
  = for {
    name <- ops.lastSegment(iri)
    uuid = generateUUID(ops.fromIRI(iri))
    result <- Backbone
      .createBackbone(tboxOnt, ops)
      .flatMap { backbone =>

        System.out.println(s"\n*** createOMFBundle\n=> iri=$iri\n=> rel=$relativeIRIPath")

        for {
          graphT <- terminologies.MutableBundle.initialize(
            iri, uuid, name, kind, ont = tboxOnt,
            extraProvenanceMetadata = extraProvenanceMetadata,
            backbone = backbone)(this)

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
          mutableTBoxGraphs.put(iri, graphT)
          graphT
        }
      }

  } yield result

  object Conversions {

    def convert1
    (acc: types.Mutable2ImmutableTerminologyMap,
     mg: MutableTerminologyBox)
    (implicit store: OWLAPIOMFGraphStore)
    : Set[java.lang.Throwable] \/ types.Mutable2ImmutableTerminologyMap
    = {
      immutableTBoxGraphs
        .get(mg.iri)
        .fold[Set[java.lang.Throwable] \/ types.Mutable2ImmutableTerminologyMap](
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
    (acc: types.Mutable2ImmutableTerminologyMap,
     mg: MutableTerminologyBox)
    (implicit store: OWLAPIOMFGraphStore)
    : Set[java.lang.Throwable] \/ types.Mutable2ImmutableTerminologyMap
    = {
      require(!acc.contains(mg), s"convert1: acc=${acc.size}, m=${mg.kindIRI}")
      val tgraph = fromTerminology(mg)

      val tiN
      : Set[java.lang.Throwable] \/ Vector[TerminologyBoxAxiom]
      = tgraph
        .gaxioms
        .foldLeft[Set[java.lang.Throwable] \/ Vector[TerminologyBoxAxiom]] {
        Vector.empty[TerminologyBoxAxiom].right
      } {
        case (-\/(errors), _) =>
          -\/(errors)
        case (\/-(es), gax: TerminologyExtensionAxiom) =>
          gax.extendedTerminology match {
            case _: ImmutableTerminologyBox =>
              (es :+ gax).right
            case mg: MutableTerminologyBox =>
              acc
                .get(mg)
                .fold[Set[java.lang.Throwable] \/ Vector[TerminologyBoxAxiom]] {
                Set[java.lang.Throwable](OMFError.omfError(
                  s"""No Immutable graph available for an imported mutable graph:
                       |mutable graph to convert:
                       |$mg
                       |""".
                    stripMargin)).left
                }{ ig =>
                  (es :+ gax.copy(extendedTerminology=ig)).right
                }
            }
        case (\/-(es), gax: TerminologyNestingAxiom) =>
          gax.nestingTerminology match {
            case _: ImmutableTerminologyGraph =>
              (es :+ gax).right
            case mg: MutableTerminologyGraph =>
              acc
                .get(mg)
                .fold[Set[java.lang.Throwable] \/ Vector[TerminologyBoxAxiom]] {
                Set[java.lang.Throwable](OMFError.omfError(
                  s"""No Immutable graph available for an imported mutable graph:
                      |mutable graph to convert:
                      |$mg
                      |""".
                    stripMargin)).left
              }{ ig =>
                (es :+ gax.copy(nestingTerminology=ig)).right
              }
          }
        case (\/-(es), _) =>
          \/-(es)
        }

      val ibAxioms: Set[java.lang.Throwable] \/ Vector[OWLAPIOMF#BundledTerminologyAxiom]
      = tgraph.bAxioms.foldLeft[Set[java.lang.Throwable] \/ Vector[OWLAPIOMF#BundledTerminologyAxiom]](
        Vector.empty[OWLAPIOMF#BundledTerminologyAxiom].right
      ) { case (acc, bAxiom) =>
        acc
      }
      tiN.flatMap { is =>

        val itgraph = tgraph.copy(imports = is.map {
          case gax: TerminologyExtensionAxiom =>
            gax.extendedTerminology
          case gax: TerminologyNestingAxiom =>
            gax.nestingTerminology
        })

        val ig =
          ImmutableTerminologyGraph(
            mg.uuid, mg.name,
            kind = mg.kind,
            ont = mg.ont,
            extraProvenanceMetadata = mg.extraProvenanceMetadata,
            mg.backbone,
            tgraph.aspects.toVector,
            tgraph.concepts.toVector,
            tgraph.reifiedRelationships.toVector,
            tgraph.unreifiedRelationships.toVector,
            tgraph.scalarDataTypes.toVector,
            tgraph.structuredDataTypes.toVector,

            tgraph.scalarOneOfRestrictions.toVector,
            tgraph.binaryScalarRestrictions.toVector,
            tgraph.iriScalarRestrictions.toVector,
            tgraph.numericScalarRestrictions.toVector,
            tgraph.plainLiteralScalarRestrictions.toVector,
            tgraph.stringScalarRestrictions.toVector,
            tgraph.synonymScalarRestrictions.toVector,
            tgraph.timeScalarRestrictions.toVector,

            tgraph.entityScalarDataProperties.toVector,
            tgraph.entityStructuredDataProperties.toVector,
            tgraph.scalarDataProperties.toVector,
            tgraph.structuredDataProperties.toVector,

            tgraph.axioms.toVector,
            is,

            is.flatMap { case cx: ConceptDesignationTerminologyAxiom => Some(cx); case _ => None }.headOption,
            is.flatMap { case ex: TerminologyExtensionAxiom => Some(ex); case _ => None }.to[Set],
            is.flatMap { case nx: TerminologyNestingAxiom => Some(nx); case _ => None }.headOption,

            annotations=tgraph.annotations
          )(mg.ops)

        val i_mg_relativePath_value = getModelTerminologyGraphRelativeIRIPath(mg)
        i_mg_relativePath_value.fold[Unit](()) { relPath =>
          require(!relPath.endsWith("_Gro"))
          require(!relPath.endsWith("_Grw"))
        }

        val i_mg_iriHashPrefix_value = getModelTerminologyGraphIRIHashPrefix(mg)

        val m2i: types.Mutable2ImmutableTerminologyMap = Map(mg -> ig) ++ acc

        val result =
          register(
            ig, itgraph, m2i,
            mg.name,
            mg.uuid,
            i_mg_relativePath_value, i_mg_iriHashPrefix_value)

        result
      }
    }

    @scala.annotation.tailrec
    def convert
    (acc: types.Mutable2ImmutableTerminologyMap,
     queue: Seq[MutableTerminologyBox],
     visited: Seq[MutableTerminologyBox])
    (implicit store: OWLAPIOMFGraphStore)
    : Set[java.lang.Throwable] \/ types.Mutable2ImmutableTerminologyMap
    = {
      if (queue.isEmpty) {
        if (visited.isEmpty)
          \/-(acc)
        else {
          val mg = visited.head

          if (acc.contains(mg))
            convert(acc, queue, visited.tail)
          else
            convert1(acc, mg) match {
              case -\/(nels) =>
                -\/(nels)
              case \/-(acc1) =>
                convert(acc1, queue, visited.tail)
            }
        }
      } else {
        val mg = queue.head
        val mgInfo = fromTerminology(mg)

        val extendedQueue =
          mgInfo
          .imports
          .flatMap {
            case _: ImmutableTerminologyBox =>
              None
            case me: MutableTerminologyBox =>
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
  def asImmutableTerminology
  (m2i: types.Mutable2ImmutableTerminologyMap,
   g: MutableTerminologyBox)
  : Set[java.lang.Throwable] \/ (ImmutableTerminologyBox, types.Mutable2ImmutableTerminologyMap)
  = for {
      next <- Conversions.convert(m2i, Seq(g), Seq())(this)
    } yield {
      require(next.contains(g))
      (next(g), next)
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
  (g: ImmutableTerminologyBox,
   info: OWLAPITerminologySignature,
   m2i: types.Mutable2ImmutableTerminologyMap,
   name: LocalName,
   uuid: UUID,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String])
  : Set[java.lang.Throwable] \/ types.Mutable2ImmutableTerminologyMap
  = for {
    _ <- registerMetadata(g, relativeIRIPath, relativeIRIHashPrefix)

    _ <- immutableTBoxGraphs.put(g.iri, g).fold[types.UnitNES](types.rightUnitNES) { conflict =>
      Set[java.lang.Throwable](OMFError.omfError(
        s"Register conflict for ${g.iri} between new graph:\n$g\nconflict:\n$conflict")).left
    }

    _ <- info.aspects.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, a: Aspect) =>
        acc +++ registerOMFModelEntityAspectInstance(g, a).map(_ => ())
    }

    _ <- info.concepts.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, c: Concept) =>
        acc +++ registerOMFModelEntityConceptInstance(g, c).map(_ => ())
    }

    _ <- info.reifiedRelationships.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, r: ReifiedRelationship) =>
        acc +++ registerOMFModelEntityReifiedRelationshipInstance(g, r).map(_ => ())
    }

    _ <- info.scalarDataTypes.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, sc: Scalar) =>
        acc +++ registerOMFModelScalarDataTypeInstance(g, sc).map(_ => ())
    }

    _ <- info.structuredDataTypes.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, st: Structure) =>
        acc +++ registerOMFModelStructuredDataTypeInstance(g, st).map(_ => ())
    }

    _ <- info.axioms.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, axiom: types.Axiom) =>
        acc +++ (axiom match {
          case ax: ConceptSpecializationAxiom =>
            registerOMFEntityConceptSubClassAxiomInstance(g, ax).map(_ => ())
          case ax: ReifiedRelationshipSpecializationAxiom =>
            registerOMFEntityReifiedRelationshipSubClassAxiomInstance(g, ax).map(_ => ())
          case ax: AspectSpecializationAxiom =>
            registerOMFEntityDefinitionAspectSubClassAxiomInstance(g, ax).map(_ => ())
          case ax: EntityUniversalRestrictionAxiom =>
            registerOMFEntityDefinitionUniversalRestrictionAxiomInstance(g, ax).map(_ => ())
          case ax: EntityExistentialRestrictionAxiom =>
            registerOMFEntityDefinitionExistentialRestrictionAxiomInstance(g, ax).map(_ => ())
          case ax: EntityScalarDataPropertyExistentialRestrictionAxiom =>
            registerOMFEntityScalarDataPropertyExistentialRestrictionAxiomInstance(g, ax).map(_ => ())
          case ax: EntityScalarDataPropertyParticularRestrictionAxiom =>
            registerOMFEntityScalarDataPropertyParticularRestrictionAxiomInstance(g, ax).map(_ => ())
          case ax: EntityScalarDataPropertyUniversalRestrictionAxiom =>
            registerOMFEntityScalarDataPropertyUniversalRestrictionAxiomInstance(g, ax).map(_ => ())
          case ax =>
            Set(
              OMFError
                .omfError(s"Unrecognized axiom: $ax")).left
        })
    }

    _ <- info.gaxioms.foldLeft[types.UnitNES](types.rightUnitNES) {
      (acc: types.UnitNES, ax: TerminologyAxiom) =>
        ax match {
          case axiom: TerminologyExtensionAxiom =>
            axiom.extendedTerminology match {
              case _: ImmutableTerminologyBox =>
                acc +++
                  registerTerminologyGraphDirectExtensionAxiom(g, axiom)
                    .map(_ => ())
              case extMG: MutableTerminologyBox =>
                m2i.get(extMG).fold[types.UnitNES] {
                  Set(OMFError.omfError(
                    s"""No Immutable graph available for an imported mutable graph:
                              |mutable graph to convert:
                              |$extMG
                              |immutable graph registration:
                              |$g
                              |""".stripMargin
                    ) ).left
                  } { extIG =>
                    acc +++
                      registerTerminologyGraphDirectExtensionAxiom(g, axiom.copy(
                        extendedTerminology = extIG))
                        .map(_ => ())
                  }
              }
          case axiom: TerminologyNestingAxiom =>
            axiom.nestingTerminology match {
                case _: ImmutableTerminologyGraph =>
                  directNestingAxioms += (g -> axiom)
                  directNestedAxioms
                    .getOrElseUpdate(
                      axiom.nestingTerminology,
                      scala.collection.mutable.HashSet[TerminologyNestingAxiom]()) += axiom
                  acc +++
                    registerTerminologyGraphDirectNestingAxiom(g, axiom)
                      .map(_ => ())
                case npMG: MutableTerminologyGraph =>
                  m2i.get(npMG).fold[types.UnitNES] {
                    Set(OMFError.
                      omfError(
                        s"""No Immutable graph available for a nesting parent mutable graph:
                              |mutable graph to convert:
                              |$npMG
                              |immutable graph registration:
                              |$g
                              |""".stripMargin
                    ) ).left
                  } { npIG =>
                    val iaxiom = axiom.copy(nestingTerminology = npIG)
                    directNestingAxioms += (g -> iaxiom)
                    directNestedAxioms
                      .getOrElseUpdate(
                        axiom.nestingTerminology,
                        scala.collection.mutable.HashSet[TerminologyNestingAxiom]()) += axiom
                    acc +++
                      registerTerminologyGraphDirectNestingAxiom(g, iaxiom)
                      .map(_ => ())
                  }
              }
          }
      }

    } yield m2i

  
  def loadTerminologyGraph
  (iri: IRI)
  : Set[java.lang.Throwable] \/ ImmutableTerminologyConversionMap
  = {
    loadBuiltinDatatypeMap()
      .flatMap { case (_, m2i) =>
        immutableTBoxGraphs
          .get(iri)
          .fold[Set[java.lang.Throwable] \/ ImmutableTerminologyConversionMap](
          OWLAPIOMFLoader.loadTerminologyGraph(iri, m2i)(ops, this)
        ){ ig =>
          \/-(ig -> m2i)
        }
      }
  }

  def createTerminologyGraphFromOntologyDocument
  (s: OntologyLoadedState,
   mgraphs: Set[MutableTerminologyGraph],
   ontIRI: IRI,
   ont: OWLOntology)
  (implicit ops: OWLAPIOMFOps)
  : Set[java.lang.Throwable] \/ Set[MutableTerminologyGraph]
  = {
    for {
      created <- types.mutableModelTerminologyGraphResolver(omfMetadata, s, ont, mgraphs, this)
    } yield {
      mutableTBoxGraphs.put(ontIRI, created._1)
      created._2
    }
  }

  type ResolverTupleState = ( Set[ImmutableTerminologyBox],
    Set[MutableTerminologyBox],
    Set[MutableTerminologyBox],
    types.Mutable2ImmutableTerminologyMap )

  def loadTerminologyGraphFromOntologyDocument
  (s: OntologyLoadedState,
   mGraph: MutableTerminologyBox,
   mGraphQueue: Set[MutableTerminologyBox],
   mGraphAcc: Set[MutableTerminologyBox],
   m2i: types.Mutable2ImmutableTerminologyMap)
  (implicit ops: OWLAPIOMFOps)
  : Set[java.lang.Throwable] \/ types.ModelTerminologyGraphsLoadState
  = {
    for {
      immutableGraphExtensionsState <- s
        .extensions
        .filter(_.extendingG == mGraph.iri)
        .foldLeft[Set[java.lang.Throwable] \/ ResolverTupleState] {
          (Set.empty[ImmutableTerminologyBox], mGraphQueue, mGraphAcc, m2i).right[Set[java.lang.Throwable]]
        }{
          resolveExtensionForLoadingTerminologyGraphFromOntologyDocument(s)
        }

      (extIGraphs, otherMGraphs, resultMGraphs, updatedM2I) = immutableGraphExtensionsState

      loaded <- types
        .loadMutableModelTerminologyGraphResolver(omfMetadata.get, s, mGraph, extIGraphs, otherMGraphs, resultMGraphs, updatedM2I, this)

    } yield loaded
  }

  def resolveExtensionForLoadingTerminologyGraphFromOntologyDocument
  (s: OntologyLoadedState)
  ( acc: Set[java.lang.Throwable] \/ ResolverTupleState, ext: ExtendingOntologyToExtendedGraphIRI )
  : Set[java.lang.Throwable] \/ ResolverTupleState
  = for {
    resolverTupleState <- acc
    (extIGraphs, otherMGraphs, resultMGraphs, m2i) = resolverTupleState

    extMGraph = otherMGraphs.find(_.iri == ext.extendedG)
    extIGraph = m2i.values.find(_.iri == ext.extendedG)

    result <- (extMGraph, extIGraph) match {
        case (None, Some(extIG)) =>
          (extIGraphs + extIG, otherMGraphs, resultMGraphs, m2i).right

        case (Some(extMG), None) =>
          for {
            converted <- asImmutableTerminology(m2i, extMG)
            (extIG, updatedM2I) = converted
            result = (extIGraphs + extIG, otherMGraphs - extMG, resultMGraphs, updatedM2I)
          } yield result

        case (None, None) =>
          -\/(Set(OMFError.omfError(
            s"Failed to resolve immutable OMF Terminology Graph for $ext"
          )))

        case (Some(extMG), Some(extIG)) =>
          -\/(Set(OMFError.omfError(
            s"Internal error when loading extension $ext\nextMG=$extMG\nextIG=$extIG"
          )))

      }
  } yield result

  def convertTerminologyGraphFromOntologyDocument
  (s: OntologyLoadedState,
   m2i: types.Mutable2ImmutableTerminologyMap,
   ontIRI: IRI,
   ont: OWLOntology,
   builtInImport: Option[ImmutableTerminologyBox])
  (implicit ops: OWLAPIOMFOps)
  : Set[java.lang.Throwable] \/ types.Mutable2ImmutableTerminologyMap
  = {

    val extendedGraphs
    : types.ImmutableTerminologyBoxesNES
    = s
      .extensions
      .filter(_.extendingG == ontIRI)
      .foldLeft[types.ImmutableTerminologyBoxesNES](builtInImport.toSet.right) {
        (acc: types.ImmutableTerminologyBoxesNES,
         ext: ExtendingOntologyToExtendedGraphIRI) =>
          acc +++
            immutableTBoxGraphs
              .get(ext.extendedG)
              .fold[types.ImmutableTerminologyBoxesNES](
              -\/(Set(OMFError.omfError(
                s"Failed to resolve immutable OMF Terminology Graph for $ext"
              )))
            ) { extG =>
              \/-(Set(extG))
            }
      }

    val nestingContextAndGraphIfAny
    : types.NestingConceptAndGraphOptionNES
    = s
      .nested2context
      .find(_.nestedG == ontIRI)
      .fold[types.NestingConceptAndGraphOptionNES](types.emptyNestingConceptAndGraphNES) { n2n =>
      immutableTBoxGraphs
        .flatMap { case (_, nestingParent) =>
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

    val annotationPropertiesAdded
    : Set[java.lang.Throwable] \/ Seq[OWLAnnotation]
    = getRelevantOntologyAnnotations(ont)
      .foldLeft[Set[java.lang.Throwable] \/ Seq[OWLAnnotation]](Seq.empty.right) {
      case (acc, a) =>
        for {
          as <- acc
          oap = a.getProperty
          tap <- addAnnotationProperty(getAnnotationPropertyFromOWLAnnotation(a))
        } yield as :+ a
    }

    val result
    : Set[java.lang.Throwable] \/ types.Mutable2ImmutableTerminologyMap
    = for {
      as <- annotationPropertiesAdded
      extensions <- extendedGraphs
      nesting <- nestingContextAndGraphIfAny
      resolver <- types.immutableModelTerminologyGraphResolver(omfMetadata, s, ont, as, extensions, nesting, m2i, this)
      resolved <- resolver.resolve()
    } yield {
      immutableTBoxGraphs.put(ontIRI, resolved._1)
      resolved._2
    }

    result
  }

  def saveOMFMetadataOntology( saveIRI: IRI ): Set[java.lang.Throwable] \/ Unit
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
            .fold[Set[java.lang.Throwable] \/ Unit](
          Set(
            OMFError.omfError(
              s"cannot save OMF Metadata Ontology because it's not yet created.")
          ).left
        ) { ontM =>
          ontManager.saveOntology(ontM, saveIRI).right
        })

  def saveTerminology
  (g: TerminologyBox)
  (implicit ops: OWLAPIOMFOps)
  : Set[java.lang.Throwable] \/ Unit
  = {
    val iri = catalogIRIMapper.resolveIRI(g.iri, catalogIRIMapper.saveResolutionStrategy)
    g.save(iri)
  }

  def saveTerminology
  (g: TerminologyBox, os: java.io.OutputStream)
  (implicit ops: OWLAPIOMFOps)
  : Set[java.lang.Throwable] \/ Unit
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
  : Set[java.lang.Throwable] \/ MutableTerminologyGraph
  = mutableTBoxGraphs
    .get(iri)
    .fold[Set[java.lang.Throwable] \/ MutableTerminologyGraph](
      if (ontManager.contains(iri)) {
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
      // not yet registered.
        createOMFTerminologyGraph(
          iri, relativeIRIPath, relativeIRIHashPrefix,
          ontManager.createOntology(iri), kind, extraProvenanceMetadata)
    ) {
    case g: MutableTerminologyGraph =>
      g.right
    case _: MutableBundle =>
      Set(
        OMFError
          .omfOpsError(ops, s"makeTerminologyGraph(iri='$iri') --  already exists as a Bundle!!")
      ).left
  }

  def makeBundle
  (uuid: UUID,
   name: LocalName,
   iri: IRI,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  (implicit ops: OWLAPIOMFOps)
  : Set[java.lang.Throwable] \/ MutableBundle
  = mutableTBoxGraphs
    .get(iri)
    .fold[Set[java.lang.Throwable] \/ MutableBundle](
    if (ontManager.contains(iri)) {
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
    // not yet registered.
      createOMFBundle(
        iri, relativeIRIPath, relativeIRIHashPrefix,
        ontManager.createOntology(iri), kind, extraProvenanceMetadata)
  ) {
    case g: MutableBundle =>
      g.right
    case _: MutableTerminologyGraph =>
      Set(
        OMFError
          .omfOpsError(ops, s"makeBundle(iri='$iri') --  already exists as a TerminologyGraph!")
      ).left
  }

  def loadInstanceGraph
  (iri: IRI)
  : Set[java.lang.Throwable] \/ instances.ImmutableModelInstanceGraph
  = ???

  def asImmutableInstanceGraph
  (g: instances.MutableModelInstanceGraph)
  : Set[java.lang.Throwable] \/ instances.ImmutableModelInstanceGraph
  = ???

  def makeInstanceGraph
  (iri: IRI,
   instantiatedTGraphs: Iterable[ImmutableTerminologyBox],
   extendedIGraphs: Iterable[instances.ImmutableModelInstanceGraph])
  : Set[java.lang.Throwable] \/ instances.MutableModelInstanceGraph
  = ???

  def resolveIRIAsLocalFile
  (iri: IRI)
  : Set[java.lang.Throwable] \/ File
  = catalogIRIMapper.resolveIRIAsLocalFile(iri)
}