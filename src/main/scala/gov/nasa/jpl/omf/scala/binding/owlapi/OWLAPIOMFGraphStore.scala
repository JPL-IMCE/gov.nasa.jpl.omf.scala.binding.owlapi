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
import java.lang.{IllegalArgumentException, System}
import java.util.function.Predicate

import gov.nasa.jpl.imce.oml.resolver.Filterable.filterable
import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.AnnotationProperty
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader._
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{ImmutableModule, Module, MutableModule}
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.{ImmutableDescriptionBox, MutableDescriptionBox, toImmutableDescriptionBoxSignature}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{terminologies, terminologyAxioms}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms._
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.builtin.BuiltInDatatypeMaps
import gov.nasa.jpl.omf.scala.core.TerminologyKind
import gov.nasa.jpl.omf.scala.core._
import org.apache.xml.resolver.tools.CatalogResolver
import org.apache.xml.resolver.Catalog
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters._
import org.semanticweb.owlapi.util.PriorityCollection
import com.github.benmanes.caffeine.cache.{CacheLoader, Caffeine, LoadingCache}

import scala.collection.immutable.{Set, _}
import scala.collection.JavaConverters._
import scala.compat.java8.OptionConverters.RichOptionalGeneric
import scala.compat.java8.StreamConverters._
import scala.util.Properties
import scala.util.control.Exception._
import scala.{Boolean, Long, None, Option, Some, StringContext, Unit}
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms.ReifiedRelationshipSpecializationAxiom


case class OWLAPIOMFGraphStore
(omfModule: OWLAPIOMFModule,
 ontManager: OWLOntologyManager,
 catalogIRIMapper: CatalogIRIMapper) {

  require(null != omfModule)
  require(null != ontManager)

  implicit val ops = omfModule.ops

  val MAXIMUM_IMPORTS: Long = Properties.propOrElse("omf.core.maxImports", "100000").toLong

  val terminologyCacheLoader
  : CacheLoader[_ >: OWLAPIOMF#Module, Set[OWLAPIOMF#TerminologyBox]]
  = new CacheLoader[OWLAPIOMF#Module, Set[OWLAPIOMF#TerminologyBox]] {

    override def load(key: OWLAPIOMF#Module)
    : Set[OWLAPIOMF#TerminologyBox]
    = computeTerminologyBoxImportClosure[OWLAPIOMF](key)(ops, OWLAPIOMFGraphStore.this)

  }.asInstanceOf[CacheLoader[_ >: OWLAPIOMF#Module, Set[OWLAPIOMF#TerminologyBox]]]

  val terminologyClosureCache
  : LoadingCache[OWLAPIOMF#Module, Set[OWLAPIOMF#TerminologyBox]]
  = Caffeine
    .newBuilder()
    .maximumSize(MAXIMUM_IMPORTS)
    .build[OWLAPIOMF#Module, Set[OWLAPIOMF#TerminologyBox]](terminologyCacheLoader)

  val descriptionCacheLoader
  : CacheLoader[_ >: OWLAPIOMF#Module, Set[OWLAPIOMF#DescriptionBox]]
  = new CacheLoader[OWLAPIOMF#Module, Set[OWLAPIOMF#DescriptionBox]] {

    override def load(key: OWLAPIOMF#Module)
    : Set[OWLAPIOMF#DescriptionBox]
    = computeDescriptionBoxImportClosure[OWLAPIOMF](key)(ops, OWLAPIOMFGraphStore.this)

  }.asInstanceOf[CacheLoader[_ >: OWLAPIOMF#Module, Set[OWLAPIOMF#DescriptionBox]]]

  val descriptionClosureCache
  : LoadingCache[OWLAPIOMF#Module, Set[OWLAPIOMF#DescriptionBox]]
  = Caffeine
    .newBuilder()
    .maximumSize(MAXIMUM_IMPORTS)
    .build[OWLAPIOMF#Module, Set[OWLAPIOMF#DescriptionBox]](descriptionCacheLoader)

  val LOG: Boolean =
    "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.GraphStore")

  lazy val RDFS_LABEL: OWLAnnotationProperty = ontManager.getOWLDataFactory.getRDFSLabel

  lazy val ANNOTATION_HAS_UUID: OWLAnnotationProperty =
    ontManager
      .getOWLDataFactory
      .getOWLAnnotationProperty(omfModule.ops.AnnotationHasUUID)

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

  private var modules = new scala.collection.mutable.HashMap[OWLAPIOMF#IRI, OWLAPIOMF#Module]()

  def lookupModule
  (iri: OWLAPIOMF#IRI)
  : OMFError.Throwables \/ OWLAPIOMF#Module
  = modules.get(iri) match {
    case Some(m) =>
      \/-(m)
    case None =>
      -\/(Set[java.lang.Throwable](OMFError.omfError(
        s"lookupModule($iri): no such Module!"
      )))
  }

  def lookupTerminologyBox
  (iri: OWLAPIOMF#IRI)
  : OMFError.Throwables \/ OWLAPIOMF#TerminologyBox
  = modules.get(iri) match {
    case Some(tbox: OWLAPIOMF#TerminologyBox) =>
      \/-(tbox)
    case _ =>
      -\/(Set[java.lang.Throwable](OMFError.omfError(
        s"lookupTerminologyBox($iri): no such TerminologyBox!"
      )))
  }

  def lookupDescriptionBox
  (iri: OWLAPIOMF#IRI)
  : OMFError.Throwables \/ OWLAPIOMF#DescriptionBox
  = modules.get(iri) match {
    case Some(dbox: OWLAPIOMF#DescriptionBox) =>
      \/-(dbox)
    case _ =>
      -\/(Set[java.lang.Throwable](OMFError.omfError(
        s"lookupDescriptionBox($iri): no such DescriptionBox!"
      )))
  }

  @scala.volatile
  private var builtInDatatypeMap
  : Option[BuiltInDatatypeMaps.DataRangeCategories[OWLAPIOMF]]
  = None

  def isBuiltInDatatypeMapConstructed
  : Boolean
  = builtInDatatypeMap.isEmpty

  def getBuildInDatatypeMap
  : BuiltInDatatypeMaps.DataRangeCategories[OWLAPIOMF]
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
  = ops.makeTerminologyGraph(iri, TerminologyKind.isOpenWorld)(this)

  def loadBuiltinDatatypeMap
  ()
  : Throwables \/ BuiltInDatatypeMaps.DataRangeCategories[OWLAPIOMF]
  = builtInDatatypeMap.fold {
      BuiltInDatatypeMaps
        .createBuiltInDatatypeMaps[OWLAPIOMF](makeW3CTerminologyGraphDefinition)(ops, this)
        .map { drc =>
          require(builtInDatatypeMap.isEmpty)
          builtInDatatypeMap = Some(drc)
          drc.builtInDatatypeModules.foreach { m =>
            require(
              modules.contains(m.iri),
              s"built-in datatype module should have been registered: ${m.iri}")
            require(
              modules.get(m.iri).contains(m),
              s"built-in datatype module ${m.iri} registration mismatch:\nexpected: $m\nactual: ${modules.get(m.iri)}")
          }
          drc.builtInImport.foreach { tbox =>
            require(
              modules.contains(tbox.iri),
              s"built-in import module should have been registered: ${tbox.iri}")
            require(
              modules.get(tbox.iri).contains(tbox),
              s"built-in import module ${tbox.iri} registration mismatch:\nexpected: $tbox\nactual: ${modules.get(tbox.iri)}")
          }
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
    IRI,
    scala.collection.mutable.HashSet[TerminologyNestingAxiom]]()
    .withDefaultValue(scala.collection.mutable.HashSet[TerminologyNestingAxiom]())

  protected val extendingChild2ExtendedParents =
    scala.collection.mutable.HashMap[
      TerminologyBox,
      scala.collection.mutable.HashSet[IRI]]()
      .withDefaultValue(scala.collection.mutable.HashSet[IRI]())

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
  (nestingC: ConceptKind)
  : Set[TerminologyNestingAxiom]
  = directNestingAxioms.values.filter(_.nestingContext == nestingC).to[Set]

  /**
    * Find the axioms TerminologyGraphDirectNestingAxiom(nestingParent=nestingG)
    */
  def lookupNestingAxiomsForNestingParent
  (nestingG: TerminologyBox)
  : Set[TerminologyNestingAxiom]
  = directNestedAxioms
    .getOrElseUpdate(nestingG.iri, scala.collection.mutable.HashSet[TerminologyNestingAxiom]())
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
  : ConceptKind
  = axiom.nestingContext

  // TerminologyNestingAxiom

  def createOMFTerminologyGraphDirectNestingAxiom
  (uuid: api.taggedTypes.TerminologyNestingAxiomUUID,
   parentG: TerminologyBox,
   parentC: ConceptKind,
   childG: TerminologyBox)
  : Throwables \/ TerminologyNestingAxiom
  = lookupNestingAxiomForNestedChildIfAny(childG)
    .fold[Throwables \/ TerminologyNestingAxiom] {

    val axiom = terminologyAxioms.TerminologyNestingAxiom(uuid, childG.uuid, parentG.iri, parentC)
    directNestingAxioms += (childG -> axiom)
    directNestedAxioms
      .getOrElseUpdate(
        axiom.targetModuleIRI,
        scala.collection.mutable.HashSet[TerminologyNestingAxiom]()) += axiom
    axiom.right
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
        .find { ax => ax.targetModuleIRI == extendedParentG.iri }
        .map { ax => (g, ax) }
    }
    .toMap

  def createOMFTerminologyGraphDirectExtensionAxiom
  (uuid: api.taggedTypes.TerminologyExtensionAxiomUUID,
   extendingG: TerminologyBox,
   extendedG: TerminologyBox)
  : Throwables \/ TerminologyExtensionAxiom
  = {
    val extendedParents =
      extendingChild2ExtendedParents
        .getOrElseUpdate(extendingG, scala.collection.mutable.HashSet[IRI]())

    val result
    : Throwables \/ TerminologyExtensionAxiom
    = if (extendedParents.contains(extendedG.iri)) {
      directExtensionAxioms
        .getOrElseUpdate(extendingG, scala.collection.mutable.HashSet[TerminologyExtensionAxiom]())
        .find { ax => ax.targetModuleIRI == extendedG.iri }
        .fold[Throwables \/ TerminologyExtensionAxiom] {
        System.out.println(s"directExtensionAxioms: ${directExtensionAxioms.size}")
        directExtensionAxioms.foreach { case (g, axs) =>
          System.out.println(s"=> extending: ${g.kindIRI} extended: ${axs.size}")
          System.out.println(axs.map(_.targetModuleIRI.toString).mkString("\n  extended:", "\n  extended:", "\n"))
        }
        System.out.println(s"extendingChild2ExtendedParents: ${extendingChild2ExtendedParents.size}")
        extendingChild2ExtendedParents.foreach { case (child, parents) =>
          System.out.println(s"=> child: ${child.kindIRI} parents: ${parents.size}")
          parents.foreach { parent =>
            System.out.println(s"==> parent: $parent")
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

      val axiom = TerminologyExtensionAxiom(uuid, extendingG.uuid, extendedG.iri)
      val extendedParents =
        extendingChild2ExtendedParents
          .getOrElseUpdate(extendingG, scala.collection.mutable.HashSet[IRI]())

      for {
        added <- Seq(
          directExtensionAxioms
            .getOrElseUpdate(extendingG, scala.collection.mutable.HashSet[TerminologyExtensionAxiom]())
            .add(axiom),
          extendedParents.add(axiom.targetModuleIRI))
      } require(added)

      terminologyClosureCache.invalidateAll()

      axiom.right
    }

    result
  }

  def reifiedRelationshipOf(p: ForwardProperty)
  : Option[ReifiedRelationship]
  = modules
    .values
    .flatMap {
      case tbox: TerminologyBox =>
        tbox.lookupReifiedRelationship(p)
      case _ =>
        None
    }
    .headOption

  def reifiedRelationshipOf(p: InverseProperty)
  : Option[ReifiedRelationship]
  = modules
    .values
    .flatMap {
      case tbox: TerminologyBox =>
        tbox.lookupReifiedRelationship(p)
      case _ =>
        None
    }
    .headOption

  def relation(r: RestrictableRelationship)
  : EntityRelationship
  = r match {
    case ur: UnreifiedRelationship =>
      ur
    case p: ForwardProperty =>
      val r = modules
        .values
        .flatMap {
          case tbox: TerminologyBox =>
            tbox.lookupReifiedRelationship(p)
          case _ =>
            None
        }
        .headOption

      r match {
        case Some(rr) =>
          rr
        case None =>
          throw new IllegalArgumentException(s"relation($r : ForwardProperty) should have a ReifiedRelationship!")
      }
    case p: InverseProperty =>
      val r = modules
        .values
        .flatMap {
          case tbox: TerminologyBox =>
            tbox.lookupReifiedRelationship(p)
          case _ =>
            None
        }
        .headOption

      r match {
        case Some(rr) =>
          rr
        case None =>
          throw new IllegalArgumentException(s"relation($r : InverseProperty) should have a ReifiedRelationship!")
      }
  }

  @scala.annotation.tailrec
  final def rootCharacterizedEntityRelationships
  (crr: CardinalityRestrictedReifiedRelationship)
  : Set[CharacterizedEntityRelationship]
  = relation(crr.restrictedRelationship) match {
    case x: CharacterizedEntityRelationship =>
      Set(x)
    case x: ReifiedRelationshipRestriction =>
      rootCharacterizedEntityRelationships(x)
    case x: CardinalityRestrictedReifiedRelationship =>
      rootCharacterizedEntityRelationships(x)
  }

  def rootCharacterizedEntityRelationships
  (rrr: ReifiedRelationshipRestriction)
  : Set[CharacterizedEntityRelationship]
  = {
    val r = modules
      .values
      .flatMap {
        case tbox: TerminologyBox if tbox.isTypeTermDefined(rrr) =>
          Some(tbox)
        case _ =>
          None
      }
      .headOption

    r match {
      case Some(tbox) =>
        val allSpecializationAxioms
        : Set[ReifiedRelationshipSpecializationAxiom]
        = tbox.importClosure()(this).flatMap { t =>
          t.getTermAxioms._2.selectByKindOf { case ax: ReifiedRelationshipSpecializationAxiom => ax }
        }
        rootCharacterizedEntityRelationships(Set(rrr), allSpecializationAxioms, Set.empty)

      case None =>
        throw new IllegalArgumentException(s"rootCharacterizedEntityRelationships($rrr) -- There should be a defining tbox!")
    }
    Set.empty
  }

  @scala.annotation.tailrec
  final def rootCharacterizedEntityRelationships
  (horizon: Set[ConceptualRelationship],
   candidates: Set[ReifiedRelationshipSpecializationAxiom],
   results: Set[CharacterizedEntityRelationship])
  : Set[CharacterizedEntityRelationship]
  = {
    val axioms = candidates.filter { ax => horizon.contains(ax.sub) }
    if (axioms.isEmpty)
      results
    else {
      val parents = axioms.map(_.sup)
      val moreResults = parents.selectByKindOf { case rr: ReifiedRelationship => rr }
      val nextHorizon = parents.selectByKindOf { case cr: ConceptualRelationship => cr }
      rootCharacterizedEntityRelationships(nextHorizon, candidates, results ++ moreResults)
    }
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
        .find { ax => ax.targetModuleIRI == bundledTerminology.iri }
        .map { ax => (g, ax) }
    }
    .toMap

  def createOMFBundledTerminologyAxiom
  (uuid: api.taggedTypes.BundledTerminologyAxiomUUID,
   terminologyBundle: MutableBundle,
   bundledTerminology: TerminologyBox)
  : Throwables \/ BundledTerminologyAxiom
  = {
    val bundledTerminologies = getBundlingAxioms(terminologyBundle).map(_.targetModuleIRI)

    val result
    : Throwables \/ BundledTerminologyAxiom
    = if (bundledTerminologies.contains(bundledTerminology.iri)) {
      Set(
        OMFError
          .omfOpsError(ops, "Duplicate BundledTerminologyAxiom")
      ).left
    } else {
      val axiom = BundledTerminologyAxiom(uuid, terminologyBundle.uuid, bundledTerminology.iri)
      directBundlingAxioms
          .getOrElseUpdate(terminologyBundle, scala.collection.mutable.HashSet[BundledTerminologyAxiom]())
          .add(axiom)
      axiom.right
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
   tboxOnt: OWLOntology,
   kind: TerminologyKind)
  : Throwables \/ MutableTerminologyGraph
  = for {
    name <- ops.lastSegment(iri)
    uuid = api.taggedTypes.terminologyGraphUUID(generateUUIDFromString(ops.fromIRI(iri).toString))
    result <- Backbone
      .createBackbone(
        tboxOnt, kind,
        tboxOnt.getOWLOntologyManager.getOWLDataFactory.getOWLAnnotationProperty(ops.AnnotationIsTerminologyGraph),
        ops)
      .flatMap { backbone =>
        terminologies.MutableTerminologyGraph.initialize(
          uuid, name, iri, kind = kind, ont = tboxOnt,
          backbone = backbone)(this, ops).map { tbox =>
          modules += iri -> tbox
          tbox
        }
      }
  } yield result

  def createOMFBundle
  (iri: IRI,
   tboxOnt: OWLOntology,
   kind: TerminologyKind)
  : Throwables \/ MutableBundle
  = for {
    name <- ops.lastSegment(iri)
    uuid = api.taggedTypes.bundleUUID(generateUUIDFromString(ops.fromIRI(iri)))
    result <- Backbone
      .createBackbone(
        tboxOnt, kind,
        tboxOnt.getOWLOntologyManager.getOWLDataFactory.getOWLAnnotationProperty(ops.AnnotationIsBundle),
        ops)
      .flatMap { backbone =>
        terminologies.MutableBundle.initialize(
          uuid, name, iri, kind, ont = tboxOnt,
          backbone = backbone)(this, ops).map { bbox =>
          modules += iri -> bbox
          bbox
        }
      }
  } yield result

  def createOMFDescriptionBox
  (iri: IRI,
   dboxOnt: OWLOntology,
   kind: DescriptionKind)
  : Throwables \/ MutableDescriptionBox
  = for {
    name <- ops.lastSegment(iri)
    uuid = api.taggedTypes.descriptionBoxUUID(generateUUIDFromString(ops.fromIRI(iri)))
    result <- Backbone
      .createBackbone(
        dboxOnt, kind,
        dboxOnt.getOWLOntologyManager.getOWLDataFactory.getOWLAnnotationProperty(ops.AnnotationIsDescriptionBox),
        ops)
      .flatMap { backbone =>
        MutableDescriptionBox.initialize(
          uuid, name, iri, kind = kind, ont = dboxOnt, backbone = backbone)(this, ops).map { dbox =>
          modules += iri -> dbox
          dbox
        }
      }
  } yield result

  object TBoxConversions {

    def convert1New
    (acc: OntologyMapping,
     mm: MutableModule)
    (implicit store: OWLAPIOMFGraphStore)
    : Throwables \/ OntologyMapping
    = {
      require(
        acc.lookupImmutableModule(mm.iri).isEmpty,
        s"convert1: acc=${acc.size}, m=$mm")

      mm match {
        case mg: MutableTerminologyGraph =>
          for {
            pair <- convert1NewTerminologyGraph(acc, mg)
            (_, next) = pair
          } yield next
        case mb: MutableBundle =>
          for {
            pair <- convert1NewBundle(acc, mb)
            (_, next) = pair
          } yield next
        case md: MutableDescriptionBox =>
          for {
            pair <- convert1NewDescriptionBox(acc, md)
            (_, next) = pair
          } yield next
      }
    }

    // need to map ModuleEdges to refer to the corresponding immutable modules

    def convert1NewTerminologyGraph
    (acc: OntologyMapping,
     mg: MutableTerminologyGraph)
    (implicit store: OWLAPIOMFGraphStore)
    : Throwables \/ (ImmutableTerminologyGraph, OntologyMapping)
    = for {
      ig <- ImmutableTerminologyGraph.initialize(toImmutableTerminologyBoxSignature(mg.sig), mg.ont, mg.backbone)
      _ = ig.sig.nesting.foreach { ax =>
        directNestingAxioms += (ig -> ax)
        modules += ig.iri -> ig
      }
      next <- acc :+ (mg -> ig)
    } yield ig -> next

    def convert1NewBundle
    (acc: OntologyMapping,
     mb: MutableBundle)
    (implicit store: OWLAPIOMFGraphStore)
    : Throwables \/ (ImmutableBundle, OntologyMapping)
    = ImmutableBundle.initialize(toImmutableTerminologyBoxSignature(mb.sig), mb.ont, mb.backbone).flatMap { ib =>
      for {
        next <- acc :+ (mb -> ib)
        _ = modules += ib.iri -> ib
      } yield ib -> next
    }

    def convert1NewDescriptionBox
    (acc: OntologyMapping,
     md: MutableDescriptionBox)
    (implicit store: OWLAPIOMFGraphStore)
    : Throwables \/ (ImmutableDescriptionBox, OntologyMapping)
    = ImmutableDescriptionBox.initialize(toImmutableDescriptionBoxSignature(md.sig), md.ont, md.backbone).flatMap { id =>
      for {
        next <- acc :+ (md -> id)
        _ = modules += id.iri -> id
      } yield id -> next
    }

    @scala.annotation.tailrec
    final def convert
    (acc: OntologyMapping,
     queue: Seq[MutableModule],
     visited: Set[MutableModule])
    (implicit store: OWLAPIOMFGraphStore)
    : Throwables \/ OntologyMapping
    = {
      if (queue.isEmpty) {
        \/-(acc)
      } else {
        val mg = queue.head
        if (acc.lookupImmutableModule(mg.iri).isDefined)
          convert(acc, queue.tail, visited + mg)
        else
          convert1New(acc, mg) match {
            case \/-(acc1) =>
              val q1
              : Throwables \/ Seq[MutableModule]
              = mg.sig.importedTerminologies.foldLeft[Throwables \/ Seq[MutableModule]] {
                \/-(queue.tail)
              } { case (qi, iri) =>
                for {
                  prev <- qi
                  next <- acc1.lookupModule(iri) match {
                    case Some(_: ImmutableModule) =>
                      \/-(prev)
                    case Some(m: MutableModule) =>
                      \/-(prev :+ m)
                    case x =>
                      -\/(Set[java.lang.Throwable](OMFError.omfError(
                        s"convert: Failed to find imported terminology for $iri from ${mg.iri}: $x"
                      )))
                  }
                } yield next
              }

              val q2
              : Throwables \/ Seq[MutableModule]
              = mg.sig.importedDescriptions.foldLeft[Throwables \/ Seq[MutableModule]] {
                q1
              } { case (qj, iri) =>
                for {
                  prev <- qj
                  next <- acc1.lookupModule(iri) match {
                    case Some(_: ImmutableDescriptionBox) =>
                      \/-(prev)
                    case Some(m: MutableDescriptionBox) =>
                      \/-(prev :+ m)
                    case _ =>
                      -\/(Set[java.lang.Throwable](OMFError.omfError(
                        s"convert: Failed to find imported descripiton for $iri from ${mg.iri}"
                      )))
                  }
                } yield next
              }

              q2 match {
                case \/-(nextq) =>

                  convert(
                    acc1,
                    nextq,
                    visited + mg)

                case -\/(errors) =>
                  -\/(errors)
              }
            case -\/(errors) =>
              -\/(errors)
          }
      }
    }

  }

  // OMF API
  def asImmutableModule
  (m: MutableModule,
   om: OntologyMapping)
  : Throwables \/ (ImmutableModule, OntologyMapping)
  = for {
    next <- TBoxConversions.convert(om, Seq(m), Set())(this)
    i <- next.get(m) match {
      case Some(im) =>
        im.right[Throwables]
      case None =>
        Set[java.lang.Throwable](OMFError.omfError(
          s"asImmutableModule: Failed to find converted immutable module for ${m.iri}"
        )).left[ImmutableModule]
    }
  } yield i -> next

  def loadModule
  (m2i: OntologyMapping,
   inputFile: File)
  : Throwables \/ (Module, OntologyMapping)
  = for {
    iri <-
    nonFatalCatch[OMFError.Throwables \/ IRI]
      .withApply {
        case t: OWLOntologyAlreadyExistsException =>
          t.getOntologyID.getOntologyIRI.asScala.fold[OMFError.Throwables \/ IRI] {
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(
              s"No OntologyIRI for ontology file: $inputFile"
            )))
          } { iri =>
            \/-(iri)
          }
        case t: java.lang.Throwable =>
          -\/(Set[java.lang.Throwable](new IllegalArgumentException(
            s"Failed to load ontology file: $inputFile", t
          )))
      }
      .apply {
        val inputIRI = inputFile.toURI.toString
        val ont = ontManager
          .ontologies()
          .filter( new Predicate[OWLOntology]() {
            override def test(ont: OWLOntology): Boolean = {
              val docIRI = ontManager.getOntologyDocumentIRI(ont).toString
              val found = docIRI == inputIRI
              found
            }
          })
          .findFirst()
          .asScala match {
          case Some(o) =>
            System.out.println(s"# Already loaded      : $inputFile")
            o
          case None =>
            ontManager.loadOntologyFromOntologyDocument(inputFile)
        }

        ont.getOntologyID.getOntologyIRI.asScala.fold[OMFError.Throwables \/ IRI] {
          -\/(Set[java.lang.Throwable](new IllegalArgumentException(
            s"No OntologyIRI for ontology file: $inputFile"
          )))
        } { iri =>
          System.out.println(s"# Ontology document   : $inputFile")
          System.out.println(s"# Ontology raw IRI   => $iri")
          val cleanIRI = IRI.create(iri.toString.stripSuffix("#"))
          System.out.println(s"# Ontology clean IRI => $cleanIRI")
          \/-(cleanIRI)
        }
      }
    loadResult <- OWLAPIOMFLoader.loadModule(iri, m2i)(ops, this)
  } yield loadResult

  def initializeOntologyMapping(mdrc: builtin.BuiltInDatatypeMaps.DataRangeCategories[OWLAPIOMF])
  : Throwables \/ OntologyMapping
  = for {
    builtin_m2i <-
      mdrc.builtInDatatypeModules.foldLeft {
        Map.empty[MutableTerminologyGraph, ImmutableTerminologyGraph].right[Throwables]
      } {
        case (acc, mg: MutableTerminologyGraph) =>
          for {
            prev <- acc
            ig <- ImmutableTerminologyGraph.initialize(toImmutableTerminologyBoxSignature(mg.sig), mg.ont, mg.backbone)(this)
            next = prev + (mg -> ig)
          } yield next
        case (acc, _: ImmutableTerminologyGraph) =>
          acc
        case (_, other) =>
          -\/(Set(OMFError.omfError(
            s"initializeOntologyMapping() failed to convert builtin datatype map graph: $other")))
      }
    idrc = BuiltInDatatypeMaps.DataRangeCategories[OWLAPIOMF](
      builtInImport = mdrc.builtInImport.flatMap {
        case mi: MutableTerminologyGraph =>
          builtin_m2i.get(mi).map(_.asInstanceOf[OWLAPIOMF#TerminologyBox])
        case ig: ImmutableTerminologyGraph =>
          Some(ig.asInstanceOf[OWLAPIOMF#TerminologyBox])
      },
      builtInDatatypeModules =
        builtin_m2i.values.map(_.asInstanceOf[OWLAPIOMF#Module]).to[Set] ++
          mdrc.builtInDatatypeModules.filter {
            case _: ImmutableTerminologyGraph =>
              true
            case _ =>
              false
          },
      anyAtomicType = mdrc.anyAtomicType,
      boolean = mdrc.boolean,
      numeric = mdrc.numeric,
      string = mdrc.string,
      plainLiteral = mdrc.plainLiteral,
      xmlLiteral = mdrc.xmlLiteral,
      binary = mdrc.binary,
      iri = mdrc.iri,
      time = mdrc.time,
      nonNormative = mdrc.nonNormative)
    om <- builtin_m2i.foldLeft(OntologyMapping.initialize(idrc).right[Throwables]) { case (acc, (m, i)) =>
      for {
        prev <- acc
        next <- prev :+ (m -> i)
      } yield next
    }
  } yield om

  type ResolverTupleState =
    (Set[ImmutableModule], Set[MutableModule], Set[MutableModule], OntologyMapping)

  def convertModuleFromOntologyDocument
  (s: OntologyLoadedState,
   om: OntologyMapping,
   mm: MutableModule)
  : Throwables \/ (ImmutableModule, OntologyMapping)
  = {
    val ontIRI: IRI = mm.iri
    val ont: OWLOntology = mm.ont
    val ontOps = new OWLOntologyOps(ont)

    val builtInImports
    : Set[ImmutableModule]
    = if (om.drc.isBuiltInModule(ontIRI))
        Set.empty
    else om.drc.lookupBuiltInModule(omlIRI) match {
      case ioml: ImmutableModule =>
        Set(ioml)
      case _ =>
        Set.empty
    }

    // Add only the annotation properties used that are not imported.
    val ontPrefix = ontIRI.getIRIString
    val aps =
      ont
        .annotationPropertiesInSignature(Imports.EXCLUDED).toScala[Set]
        .filter(_.getIRI.getIRIString.startsWith(ontPrefix))

    val annotationProperties
    : Throwables \/ Set[AnnotationProperty]
    = aps
      .foldLeft(Set.empty[AnnotationProperty].right[Throwables]) {
        case (acc, ont_ap) =>
          for {
            as <- acc
            ap <- getAnnotationPropertyFromOWLAnnotationProperty(ont_ap)
          } yield as + ap
      }

    val addedAnnotations
    : Throwables \/ Unit
    = for {
      aps <- annotationProperties
      _ <- aps.foldLeft(types.rightUnitNES) { case (acc, ap) =>
        for {
          _ <- acc
          _ <- mm.addAnnotationProperty(ap)(this)
        } yield ()
      }

      _ <- getRelevantOntologyAnnotations(ont).foldLeft(types.rightUnitNES) { case (acc, a) =>
        for {
          _ <- acc
          av <- getAnnotationValueFromOWLAnnotation(a.getValue)
          ap <- getAnnotationPropertyFromOWLAnnotation(a)
          _ <- mm.addAnnotation(mm, ap, av)(this)
        } yield ()
      }
    } yield ()

    val extensionsFromOnt =
      s
        .extensions
        .filter(_.extending == ontIRI)

    val nestingsFromOnt =
      s
        .nested2context
        .find(_.nestedG == ontIRI)

    val resolver
    : Throwables \/ ImmutableResolver
    = mm match {
      case mb: MutableBundle =>
        for {
          _ <- extensionsFromOnt
            .foldLeft[Throwables \/ Unit](().right[Throwables]) { case (acc, ext) =>
            for {
              _ <- acc
              extTbox <- om.lookupModule(ext.extended) match {
                case Some(b: Bundle) =>
                  mb.createBundledTerminologyAxiom(b)(this)

                case Some(t: TerminologyBox) =>
                  mb.createTerminologyExtensionAxiom(t)(this)

                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"convertModuleFromOntologyDocument($ontIRI) Bundle cannot extend a non-terminology module: ${ext.extended}"
                  )).left
              }
            } yield ()
          }
          h = new BundleResolverHelper(mb, ont, this, om, ontOps)
          r = new ImmutableBundleResolver(h)
        } yield r

      case mg: MutableTerminologyGraph =>
        for {
          _ <- extensionsFromOnt
            .foldLeft[Throwables \/ Unit](().right[Throwables]) { case (acc, ext) =>
            for {
              _ <- acc
              _ <- om.lookupModule(ext.extended) match {
                case Some(t: TerminologyBox) =>
                  mg.createTerminologyExtensionAxiom(t)(this)

                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"convertModuleFromOntologyDocument($ontIRI) TerminologyGraph cannot extend a non-terminology module: ${ext.extended}"
                  )).left
              }
            } yield ()
          }
          _ <- nestingsFromOnt.foldLeft[Throwables \/ Unit](().right[Throwables]) { case (acc, n2c) =>
            for {
              _ <- acc
              _ <- ops.lookupConcept(mg, n2c.nestingC, recursively = true)(this) match {
                case Some(c) =>
                  om.terminologyBoxValues.find { tbox =>
                    ops.lookupConcept(tbox, n2c.nestingC, recursively = false)(this).isDefined
                  } match {
                    case Some(tbox) =>
                      mg.createTerminologyNestingAxiom(parentG = tbox, parentC = c)(this)

                    case None =>
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"convertModuleFromOntologyDocument($ontIRI) Failed to resolve nesting concept: ${n2c.nestingC}"
                      )).left
                  }
                case None =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"convertModuleFromOntologyDocument($ontIRI) Failed to resolve nesting concept: ${n2c.nestingC}"
                  )).left
              }
            } yield ()
          }

          h = new TerminologyBoxResolverHelper(mg, ont, this, om, ontOps)
          r = new ImmutableTerminologyBoxResolver(h)
        } yield r
      case md: MutableDescriptionBox =>
        for {
          _ <- extensionsFromOnt
            .foldLeft[Throwables \/ Unit](().right[Throwables]) { case (acc, ext) =>
            for {
              _ <- acc
              _ <- om.lookupModule(ext.extended) match {
                case Some(t: TerminologyBox) =>
                  // TODO add API...
                  \/-(())

                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"convertModuleFromOntologyDocument($ontIRI) TerminologyGraph cannot extend a non-terminology module: ${ext.extended}"
                  )).left
              }
            } yield ()
          }

          h = DescriptionBoxResolverHelper(md, ont, this, om, ontOps)
          r = ImmutableDescriptionBoxResolver(h)
        } yield r
    }

    resolver.flatMap { r =>
      r.resolve()
    }
  }

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

  def hasBuiltAnnotations
  (iri: IRI)
  : Boolean
  = {
    val siri = iri.toString
    "http://www.w3.org/2002/07/owl" == siri
  }

  def isBuiltInIRI
  (iri: IRI)
  : Boolean
  = {
    val siri = iri.toString
    "http://purl.org/dc/elements/1.1/" == siri ||
      "http://www.w3.org/2002/07/owl" == siri ||
      "http://www.w3.org/2001/XMLSchema" == siri ||
      "http://www.w3.org/2000/01/rdf-schema" == siri ||
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#" == siri ||
      "http://www.w3.org/2003/11/swrl" == siri
  }

  def makeTerminologyGraph
  (name: tables.taggedTypes.LocalName,
   iri: IRI,
   kind: TerminologyKind)
  (implicit ops: OWLAPIOMFOps)
  : Throwables \/ MutableTerminologyGraph
  = if (ontManager.contains(iri)) {
    if (isBuiltInIRI(iri))
      createOMFTerminologyGraph(iri, ontManager.getOntology(iri), kind)
    else
      Set(
        OMFError
          .omfOpsError(ops, s"makeTerminologyGraph(iri='$iri') --  already exists!")
      ).left
  } else
    createOMFTerminologyGraph(iri, ontManager.createOntology(iri), kind)

  def makeBundle
  (name: tables.taggedTypes.LocalName,
   iri: IRI,
   kind: TerminologyKind)
  (implicit ops: OWLAPIOMFOps)
  : Throwables \/ MutableBundle
  = if (ontManager.contains(iri)) {
    if (isBuiltInIRI(iri))
      createOMFBundle(iri, ontManager.getOntology(iri), kind)
    else
      Set(
        OMFError
          .omfOpsError(ops, s"makeBundle(iri='$iri') --  already exists!")
      ).left
  } else
    createOMFBundle(iri, ontManager.createOntology(iri), kind)

  def loadDescriptionBox
  (iri: IRI)
  : Throwables \/ descriptions.ImmutableDescriptionBox
  = ???

  def makeDescriptionBox
  (name: tables.taggedTypes.LocalName,
   iri: IRI,
   k: DescriptionKind)
  : Throwables \/ descriptions.MutableDescriptionBox
  = if (ontManager.contains(iri)) {
    Set(
      OMFError
        .omfOpsError(ops, s"makeDescriptionBox(iri='$iri') --  already exists!")
    ).left[descriptions.MutableDescriptionBox]
  } else
    createOMFDescriptionBox(iri, ontManager.createOntology(iri), k)

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