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
package gov.nasa.jpl.omf.scala.binding.owlapi.types

import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import org.semanticweb.owlapi.reasoner.{NodeSet, OWLReasoner}

import scala.collection.JavaConversions._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

object ImmutableOperation extends Enumeration {
  type ImmutableOperation = Value
  val Save,
  AddEntityAspect,
  AddEntityConcept,
  AddEntityReifiedRelationship,
  AddScalarDataType,
  AddDataRelationshipFromEntityToScalar,
  AddDataRelationshipFromEntityToStructure,
  AddDataRelationshipFromStructureToScalar,
  AddDataRelationshipFromStructureToStructure,
  AddEntityConceptSubClassAxiom,
  AddEntityDefinitionAspectSubClassAxiom = Value
}

import gov.nasa.jpl.omf.scala.binding.owlapi.types.ImmutableOperation._

sealed abstract class ImmutableModelTerminologyGraphException(val message: String)
  extends IllegalArgumentException(message) {
  require(null != message)
}

case class ReadOnlyImmutableTerminologyGraphException(val operation: ImmutableOperation)
  extends ImmutableModelTerminologyGraphException(
    s"Operation '$operation' is illegal on an read-only ImmutableTerminologyGraph") {
  require(null != operation)
}

case class ImmutableModelTerminologyGraph
(override val kind: TerminologyKind,
 override val ont: OWLOntology,
 override protected val aspects: List[ModelEntityAspect],
 override protected val concepts: List[ModelEntityConcept],
 override protected val reifiedRelationships: List[ModelEntityReifiedRelationship],
 override protected val unreifiedRelationships: List[ModelEntityUnreifiedRelationship],
 override protected val sc: List[ModelScalarDataType],
 override protected val st: List[ModelStructuredDataType],
 override protected val e2sc: List[ModelDataRelationshipFromEntityToScalar],
 override protected val e2st: List[ModelDataRelationshipFromEntityToStructure],
 override protected val s2sc: List[ModelDataRelationshipFromStructureToScalar],
 override protected val s2st: List[ModelDataRelationshipFromStructureToStructure],
 override protected val ax: List[ModelTermAxiom])
(override implicit val ops: OWLAPIOMFOps)
  extends ModelTerminologyGraph(kind, ont)(ops) {

  require(null != kind)
  require(null != ont)
  require(null != aspects)
  require(null != concepts)
  require(null != reifiedRelationships)
  require(null != unreifiedRelationships)
  require(null != sc)
  require(null != st)
  require(null != e2sc)
  require(null != e2st)
  require(null != s2sc)
  require(null != s2st)
  require(null != ax)

  override val isImmutableModelTerminologyGraph = true
  override val isMutableModelTerminologyGraph = false

  val getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition] =
    ((aspects map (a => a.e -> a)) ++
      (concepts map (c => c.e -> c)) ++
      (reifiedRelationships map (r => r.e -> r))) toMap

  val getScalarDatatypeDefinitionMap: Map[OWLDatatype, ModelScalarDataType] =
    sc map ( t => t.sc -> t ) toMap

  override protected val iri2typeTerm = {
    def term2pair[T <: ModelTypeTerm](t: T) = t.iri -> t

    (aspects map term2pair) ++
      (concepts map term2pair) ++
      (reifiedRelationships map term2pair) ++
      (sc map term2pair) ++
      (st map term2pair) ++
      (e2sc map term2pair) ++
      (e2st map term2pair) ++
      (s2sc map term2pair) ++
      (s2st map term2pair) toMap
  }

}

case class ResolverHelper
(val omfMetadata: OWLOntology,
 val imports: Iterable[ImmutableModelTerminologyGraph],
 val ont: OWLOntology,
 val omfStore: OWLAPIOMFGraphStore) {

  require(null != omfMetadata)
  require(null != imports)
  require(null != ont)
  require(null != omfStore)

  import omfStore.ops._
  implicit val store = omfStore

  def getOntologyIRI: IRI =
    ont.getOntologyID.getOntologyIRI.get

  val kind =
    if (isOntologyTBoxDefinition)
      if (isOntologyTBoxToplevel)
        TerminologyKind.isToplevelDefinition
      else
        TerminologyKind.isDefinition
    else
    if (isOntologyTBoxToplevel)
      TerminologyKind.isToplevelDesignation
    else
      TerminologyKind.isDesignation


  lazy val tboxG: MutableModelTerminologyGraph = resolveTerminologyGraph(
    o = omfMetadata,
    ont = ont,
    kind = kind )(omfStore) match {

    case Failure(f) =>
      throw f

    case Success(g) =>
      ( omfStore.ops.setTerminologyGraphShortName(g, getOntologyShortName)(omfStore),
        omfStore.ops.setTerminologyGraphUUID(g, getOntologyUUID)(omfStore) ) match {
        case ( Failure(f), _ ) =>
          throw f
        case ( _, Failure(f) ) =>
          throw f
        case _ =>
          for {
            importG <- imports
            ok = addTerminologyGraphExtension(extendingG = g, extendedG = importG)(omfStore)
          } ok match {
            case Failure(f) =>
              throw f
            case Success(_) =>
              ()
          }
          g
      }
  }

  val provenance = s"load($getOntologyIRI)"

  def isOntologyTBoxToplevel: Boolean = {
    for {
      aaa <- ont.getAnnotations
      if aaa.getProperty.getIRI == AnnotationIsToplevel
    } {
      aaa.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          return l.parseBoolean
        case _ =>
          ()
      }
    }

    false
  }

  def isOntologyTBoxDefinition: Boolean = {
    for {
      aaa <- ont.getAnnotations
      if aaa.getProperty.getIRI == AnnotationIsDefinition
    } {
      aaa.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          return l.parseBoolean
        case _ =>
          ()
      }
    }

    true
  }

  def isOntologyTBoxDesignation: Boolean = {
    for {
      aaa <- ont.getAnnotations
      if aaa.getProperty.getIRI == AnnotationIsDesignation
    } {
      aaa.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          return l.parseBoolean
        case _ =>
          ()
      }
    }

    false
  }

  val getOntologyShortName: Option[String] =
    ont.getAnnotations.
      find(_.getProperty.getIRI == rdfs_label).
      flatMap(_.getValue match {
      case l: OWLLiteral =>
        Some(l.getLiteral)
      case _ =>
        None
    })

  val getOntologyUUID: Option[String] =
    ont.getAnnotations.
      find(_.getProperty.getIRI == AnnotationHasUUID).
      flatMap(_.getValue match {
      case l: OWLLiteral =>
        Some(l.getLiteral)
      case _ =>
        None
    })

  def isAnnotatedAbstract(iri: IRI): Boolean = {
    for {
      aaa <- ont.getAnnotationAssertionAxioms(iri)
      if aaa.getProperty.getIRI == AnnotationIsAbstract
    } {
      aaa.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          return l.parseBoolean
        case _ =>
          ()
      }
    }

    false
  }

  def isAnnotatedDerived(iri: IRI): Boolean = {
    for {
      aaa <- ont.getAnnotationAssertionAxioms(iri)
      if aaa.getProperty.getIRI == AnnotationIsDerived
    } {
      aaa.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          return l.parseBoolean
        case _ =>
          ()
      }
    }

    false
  }

  def getOWLTermShortName(entityIRI: IRI): Option[String] = {
    for {
      aaa <- ont.getAnnotationAssertionAxioms(entityIRI)
      if aaa.getProperty.getIRI == rdfs_label
    } {
      aaa.getValue match {
        case l: OWLLiteral =>
          Some(l.getLiteral)
        case _ =>
          ()
      }
    }

    None
  }

  def getOWLTermUUID(entityIRI: IRI): Option[String] = {
    for {
      aaa <- ont.getAnnotationAssertionAxioms(entityIRI)
      if aaa.getProperty.getIRI == AnnotationHasUUID
    } {
      aaa.getValue match {
        case l: OWLLiteral =>
          Some(l.getLiteral)
        case _ =>
          ()
      }
    }

    None
  }


  // Lookup of entity aspects

  def findDirectEntityAspect(iri: IRI, aspects: Map[OWLClass, ModelEntityAspect]): Option[ModelEntityAspect] =
    (for {
      (aspectC, aspectM) <- aspects
      if iri == aspectC.getIRI
    } yield aspectM) headOption

  def findImportedEntityAspect(iri: IRI): Option[ModelEntityAspect] =
    (for {
      g <- imports
      aspectM <- omfStore.ops.lookupEntityAspect(g, iri, recursively = true)
    } yield aspectM) headOption

  def findEntityAspect(iri: IRI, aspects: Map[OWLClass, ModelEntityAspect]): Option[ModelEntityAspect] =
    findDirectEntityAspect(iri, aspects) orElse findImportedEntityAspect(iri)

  // Lookup of entity concepts

  def findDirectEntityConcept(iri: IRI, concepts: Map[OWLClass, ModelEntityConcept]): Option[ModelEntityConcept] =
    (for {
      (conceptC, conceptM) <- concepts
      if iri == conceptC.getIRI
    } yield conceptM) headOption

  def findImportedEntityConcept(iri: IRI): Option[ModelEntityConcept] =
    (for {
      g <- imports
      conceptM <- omfStore.ops.lookupEntityConcept(g, iri, recursively = true)
    } yield conceptM) headOption

  def findEntityConcept(iri: IRI, concepts: Map[OWLClass, ModelEntityConcept]): Option[ModelEntityConcept] =
    findDirectEntityConcept(iri, concepts) orElse findImportedEntityConcept(iri)

  // Lookup of entity relationships

  def findDirectEntityReifiedRelationship
  (iri: IRI, relationships: Map[OWLClass, ModelEntityReifiedRelationship])
  : Option[ModelEntityReifiedRelationship] =
    (for {
      (conceptC, conceptM) <- relationships
      if iri == conceptC.getIRI
    } yield conceptM) headOption

  def findImportedEntityReifiedRelationship
  (iri: IRI)
  : Option[ModelEntityReifiedRelationship] =
    (for {
      g <- imports
      conceptM <- omfStore.ops.lookupEntityReifiedRelationship(g, iri, recursively = true)
    } yield conceptM) headOption

  def findEntityReifiedRelationship
  (iri: IRI, relationships: Map[OWLClass, ModelEntityReifiedRelationship])
  : Option[ModelEntityReifiedRelationship] =
    findDirectEntityReifiedRelationship(iri, relationships) orElse findImportedEntityReifiedRelationship(iri)

  // ------

  type DOPInfo = (OWLDataProperty, OWLClass, OWLDatatype)

  def resolveDataPropertyDPIRIs
  (subDPs: NodeSet[OWLDataProperty], tDPs: Set[OWLDataProperty])
  (implicit reasoner: OWLReasoner)
  : Iterable[DOPInfo] =
    for {
      dataPropertyN <- subDPs
      dataPropertyDP <- dataPropertyN flatMap { case dp: OWLDataProperty => Some(dp) }
      if tDPs.contains(dataPropertyDP)
      dataPropertyDomain <- reasoner.getDataPropertyDomains(dataPropertyDP, true).getFlattened
      dataPropertyRange <- ont.getDataPropertyRangeAxioms(dataPropertyDP)
      dataPropertyType = dataPropertyRange.getRange.asOWLDatatype
    } yield (dataPropertyDP, dataPropertyDomain, dataPropertyType)

  def resolveDataRelationshipsFromEntity2Scalars
  (entityDefinitions: Map[OWLClass, ModelEntityDefinition],
   dataPropertyDPIRIs: Iterable[DOPInfo],
   DTs: Map[OWLDatatype, ModelScalarDataType])
  : Try[List[ModelDataRelationshipFromEntityToScalar]] = {
    val remainingDataPropertyDPIRIs = scala.collection.mutable.ListBuffer[DOPInfo]()
    remainingDataPropertyDPIRIs ++= dataPropertyDPIRIs

    val e2sc = for {
      dataPropertyDPIRI <- dataPropertyDPIRIs
      (e2sc_dp, e2sc_source, e2sc_target) = dataPropertyDPIRI
      e2sc_sourceDef <- entityDefinitions.get(e2sc_source)
      e2sc_targetDef <- DTs.get(e2sc_target)
    } yield {
        remainingDataPropertyDPIRIs -= dataPropertyDPIRI
        tboxG.addDataRelationshipFromEntityToScalar(e2sc_dp.getIRI, e2sc_sourceDef, e2sc_targetDef)(omfStore) match {
          case Failure(f) =>
            return Failure(f)
          case Success(r) =>
            r
        }
      }

    if (remainingDataPropertyDPIRIs.isEmpty)
      Success(e2sc.toList)
    else
      Failure(new IllegalArgumentException("... esc ..."))
  }

  // ----------

  type ROPInfo = (IRI, OWLObjectProperty, OWLClass, OWLClass, Option[OWLObjectProperty])

  def ropInfoToString(ropInfo: ROPInfo): String =
    s"""|ROPInfo 
        |  iri=${ropInfo._1}
        |obj. prop=${ropInfo._2}
        |domain=${ropInfo._3}
        |range=${ropInfo._4}
        |inv o. p.=${ropInfo._5}
        |""".stripMargin('|')

  type Chains = Set[(OWLObjectProperty, OWLObjectProperty, OWLObjectProperty)]

  def chainToString(chain: (OWLObjectProperty, OWLObjectProperty, OWLObjectProperty)): String =
    s"""|Chain
        |obj. prop=${chain._1}
        |hasSource=${chain._2}
        |hasTarget=${chain._3}
        |""".stripMargin('|')

  def resolveEntityDefinitionsForRelationships
  (entityDefinitions: Map[OWLClass, ModelEntityDefinition],
   RCs: Map[IRI, OWLClass],
   ROPs: Iterable[ROPInfo],
   sourceROPs: Iterable[ROPInfo],
   targetROPs: Iterable[ROPInfo],
   chains: Chains,
   entityReifiedRelationships: Map[OWLClass, ModelEntityReifiedRelationship])
  : Try[Map[OWLClass, ModelEntityReifiedRelationship]] = {

    val rcs = RCs.values.toSet
    val (resolvableROPs, unresolvedROPs) = ROPs.partition {
      case (_, op, source, target, _) =>
        entityDefinitions.contains(source) && entityDefinitions.contains(target)
    }
    val (resolvableSourceROPs, unresolvableSourceROPs) = sourceROPs.partition {
      case (_, op, source, target, _) =>
        rcs.contains(source) && entityDefinitions.contains(target)
    }
    val (resolvableTargetROPs, unresolvableTargetROPs) = targetROPs.partition {
      case (_, op, source, target, _) =>
        rcs.contains(source) && entityDefinitions.contains(target)
    }

    val remainingROPs = scala.collection.mutable.HashSet[ROPInfo]()
    remainingROPs ++= resolvableROPs

    val remainingSourceROPs = scala.collection.mutable.HashSet[ROPInfo]()
    remainingSourceROPs ++= resolvableSourceROPs

    val remainingTargetROPs = scala.collection.mutable.HashSet[ROPInfo]()
    remainingTargetROPs ++= resolvableTargetROPs

    val m = for {
      (chainOP, chainSource, chainTarget) <- chains

      (r_iri, r_op, r_source, r_target, r_inv_op) <- resolvableROPs
      if chainOP.getIRI == r_iri
      //_ = System.out.println(s"r_op: matches!")
      //_ = System.out.println(s"r_op: chainSource: ${chainSource.getIRI} chainTarget: ${chainTarget.getIRI}")
      //_ = System.out.println(s"r_op: r_source: ${r_source.getIRI} r_target: ${r_target.getIRI}")

      (s_iri, s_op, s_source, s_target, s_inv_op) <- resolvableSourceROPs filter {
        case (x, _, _, y, _) =>
          x == chainSource.getIRI && y.getIRI == r_source.getIRI
      }
      //_ = System.out.println(s"s_op: ${s_op.getIRI}")

      (t_iri, t_op, t_source, t_target, t_inv_op) <- resolvableTargetROPs filter {
        case (x, _, _, y, _) =>
          x == chainTarget.getIRI && y.getIRI == r_target.getIRI
      }
      //_ = System.out.println(s"t_op: ${t_op.getIRI}")

      if s_source == t_source

      r_sourceDef <- entityDefinitions.get(r_source)
      r_targetDef <- entityDefinitions.get(r_target)

      rc = s_source

      resolvedROP = (r_iri, r_op, r_source, r_target, r_inv_op)
      resolvedSourceROP = (s_iri, s_op, s_source, s_target, s_inv_op)
      resolvedTargetROP = (t_iri, t_op, t_source, t_target, t_inv_op)

      rop = addEntityReifiedRelationship(
        o = omfMetadata,
        hasProvenanceFromRule = provenance,
        graph = tboxG,
        source = r_sourceDef,
        target = r_targetDef,
        characteristics = Nil,
        reifiedRelationshipName = getIRIFragment(rc.getIRI),
        unreifiedRelationshipName = getIRIFragment(r_op.getIRI),
        unreifiedInverseRelationshipName = r_inv_op match {
          case None =>
            None
          case Some(inv) =>
            Some(getIRIFragment(inv.getIRI))
        },
        isAbstract = isAnnotatedAbstract(rc.getIRI)
      )(omfStore) match {
        case Failure(f) =>
          return Failure(f)
        case Success(rr) =>
          rr
      }
    //      rop = ModelEntityReifiedRelationship(
    //        e = rc,
    //        unreified = r_op,
    //        inverse = None,
    //        source = r_sourceDef, rSource = s_op,
    //        target = r_targetDef, rTarget = t_op,
    //        characteristics = Nil,
    //        isAbstract = isAnnotatedAbstract(rc.getIRI))
    //_ = System.out.println(s"rop=$r_iri $s_iri $t_iri")
    } yield {
        remainingROPs -= resolvedROP
        remainingSourceROPs -= resolvedSourceROP
        remainingTargetROPs -= resolvedTargetROP
        rc -> rop
      }

    if ((remainingROPs.isEmpty && (remainingSourceROPs.nonEmpty || remainingTargetROPs.nonEmpty)) ||
      (remainingSourceROPs.isEmpty && (remainingROPs.nonEmpty || remainingTargetROPs.nonEmpty)) ||
      (remainingTargetROPs.isEmpty && (remainingROPs.nonEmpty || remainingSourceROPs.nonEmpty)) ||
      remainingROPs.size != remainingSourceROPs.size ||
      remainingROPs.size != remainingTargetROPs.size) {

      val rops = remainingROPs.map(ropInfoToString).mkString("\n")
      val srops = remainingSourceROPs.map(ropInfoToString).mkString("\n")
      val trops = remainingTargetROPs.map(ropInfoToString).mkString("\n")

      Failure(new IllegalArgumentException(
        s"""|Unresolved Reified Object Properties, ROPs: 
            |
            |*** ${remainingROPs.size} remaining ROPs ***
            |$rops
            |
            |*** ${remainingSourceROPs.size} remaining source ROPs ***
            |$srops
            |
            |*** ${remainingTargetROPs.size} remaining target ROPs ***
            |$trops
            |""".stripMargin('|')))
    } else if (remainingROPs.isEmpty && remainingSourceROPs.isEmpty && remainingTargetROPs.isEmpty &&
      unresolvedROPs.isEmpty && unresolvableSourceROPs.isEmpty && unresolvableTargetROPs.isEmpty)
      Success(entityReifiedRelationships ++ m.toMap)

    else if (m.isEmpty) {
      //      System.out.println(s"\n#resolveEntityDefinitionsForRelationships no more matches")
      //
      //      val chainDescriptions = chains.map( chainToString ).mkString( "\n" )
      //      System.out.println(
      //        s"""|Chains: ${chains.size}
      //            |
      //            |$chainDescriptions
      //            |
      //            |""".stripMargin( '|' ) )
      //
      //      val rops = remainingROPs.map( ropInfoToString ).mkString( "\n" )
      //      val srops = remainingSourceROPs.map( ropInfoToString ).mkString( "\n" )
      //      val trops = remainingTargetROPs.map( ropInfoToString ).mkString( "\n" )
      //
      //      System.out.println(
      //        s"""|Remaining Reified Object Properties, ROPs:
      //            |
      //            |*** ${remainingROPs.size} remaining ROPs ***
      //            |$rops
      //            |
      //            |*** ${remainingSourceROPs.size} remaining source ROPs ***
      //            |$srops
      //            |
      //            |*** ${remainingTargetROPs.size} remaining target ROPs ***
      //            |$trops
      //            |""".stripMargin( '|' ) )
      //
      //      val urops = unresolvedROPs.map( ropInfoToString ).mkString( "\n" )
      //      val usrops = unresolvableSourceROPs.map( ropInfoToString ).mkString( "\n" )
      //      val utrops = unresolvableTargetROPs.map( ropInfoToString ).mkString( "\n" )
      //
      //      System.out.println(
      //          s"""|Unresolved Reified Object Properties, ROPs:
      //              |
      //              |*** ${unresolvedROPs.size} unresolved ROPs ***
      //              |$urops
      //              |
      //              |*** ${unresolvableSourceROPs.size} unresolved source ROPs ***
      //              |$usrops
      //              |
      //              |*** ${unresolvableTargetROPs.size} unresolved target ROPs ***
      //              |$utrops
      //              |""".stripMargin( '|' ) )
      Success(entityReifiedRelationships)
    } else {
      //      System.out.println(s"\n#resolveEntityDefinitionsForRelationships with ${m.size}")
      resolveEntityDefinitionsForRelationships(
        entityDefinitions ++ m.toMap,
        RCs ++ (m map { case (rc, rcDef) => rc.getIRI -> rc }).toMap,
        unresolvedROPs ++ remainingROPs,
        unresolvableSourceROPs ++ remainingSourceROPs,
        unresolvableTargetROPs ++ remainingTargetROPs,
        chains,
        entityReifiedRelationships ++ m.toMap)
    }
  }

  // ---------

  def resolveThingCIRIs
  (thingSubClasses: NodeSet[OWLClass], tCs: Set[OWLClass])
  : Map[IRI, OWLClass] =
    (for {
      thingN <- thingSubClasses
      if !thingN.isBottomNode
      thingC = thingN.getRepresentativeElement
      if tCs.contains(thingC)
      thingIRI = thingC.getIRI
    } yield thingIRI -> thingC).toMap

  def resolveAspectCIRIs
  (entitySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass])
  : Map[IRI, OWLClass] =
    (for {
      aspectN <- entitySubClasses
      if !aspectN.isBottomNode
      aspectC = aspectN.getRepresentativeElement
      if tCs.contains(aspectC)
      aspectIRI = aspectC.getIRI
    } yield aspectIRI -> aspectC).toMap

  def resolveConceptCIRIs
  (entitySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass])
  : Map[IRI, OWLClass] =
    (for {
      conceptN <- entitySubClasses
      if !conceptN.isBottomNode
      conceptC = conceptN.getRepresentativeElement
      if tCs.contains(conceptC)
      conceptIRI = conceptC.getIRI
    } yield conceptIRI -> conceptC).toMap

  def resolveReifiedObjectPropertyCIRIs
  (reifiedObjectPropertySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass])
  : Map[IRI, OWLClass] =
    (for {
      reifiedObjectPropertyN <- reifiedObjectPropertySubClasses
      if !reifiedObjectPropertyN.isBottomNode
      reifiedObjectPropertyC = reifiedObjectPropertyN.getRepresentativeElement
      if tCs.contains(reifiedObjectPropertyC)
      reifiedObjectPropertyCIRI = reifiedObjectPropertyC.getIRI
    } yield reifiedObjectPropertyCIRI -> reifiedObjectPropertyC).toMap

  def resolveReifiedStructuredDataPropertyCIRIs
  (reifiedStructuredDataPropertySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass])
  : Map[IRI, OWLClass] =
    (for {
      reifiedStructuredDataPropertyN <- reifiedStructuredDataPropertySubClasses
      if !reifiedStructuredDataPropertyN.isBottomNode
      reifiedStructuredDataPropertyC = reifiedStructuredDataPropertyN.getRepresentativeElement
      if tCs.contains(reifiedStructuredDataPropertyC)
      reifiedStructuredDataPropertyCIRI = reifiedStructuredDataPropertyC.getIRI
    } yield reifiedStructuredDataPropertyCIRI -> reifiedStructuredDataPropertyC).toMap

  def resolveStructuredDatatypeCIRIs
  (structuredDatatypeSubClasses: NodeSet[OWLClass], tCs: Set[OWLClass])
  : Map[IRI, OWLClass] =
    (for {
      structuredDatatypeN <- structuredDatatypeSubClasses
      if !structuredDatatypeN.isBottomNode
      structuredDatatypeC = structuredDatatypeN.getRepresentativeElement
      if tCs.contains(structuredDatatypeC)
      structuredDatatypeCIRI = structuredDatatypeC.getIRI
    } yield structuredDatatypeCIRI -> structuredDatatypeC).toMap

  def resolveDomainRangeForObjectProperties
  (subOPs: NodeSet[OWLObjectPropertyExpression], tOPs: Set[OWLObjectProperty])
  (implicit reasoner: OWLReasoner)
  : Iterable[ROPInfo] =
    (for {
      _n_ <- subOPs
      _op_ <- _n_ flatMap {
        case op: OWLObjectProperty =>
          if (tOPs.contains(op) && !isAnnotatedDerived(op.getIRI))
            Some(op)
          else
            None
        case inv: OWLObjectInverseOf =>
          inv.getInverse match {
            case op: OWLObjectProperty =>
              if (tOPs.contains(op) && !isAnnotatedDerived(op.getIRI))
                Some(op)
              else
                None
            case _ =>
              None
          }
      }
      _d_ <- reasoner.getObjectPropertyDomains(_op_, true).getFlattened
      _r_ <- reasoner.getObjectPropertyRanges(_op_, true).getFlattened
    } yield {
        val INV = (_n_ flatMap {
          case op: OWLObjectProperty =>
            if (tOPs.contains(op) && isAnnotatedDerived(op.getIRI))
              Some(op)
            else
              None
          case inv: OWLObjectInverseOf =>
            inv.getInverse match {
              case op: OWLObjectProperty =>
                if (tOPs.contains(op) && isAnnotatedDerived(op.getIRI))
                  Some(op)
                else
                  None
              case _ =>
                None
            }
        }).headOption
        (_op_.getIRI, _op_, _d_, _r_, INV)
      }).toSet

  def resolveConceptSubClassAxioms
  (conceptCMs: Map[OWLClass, ModelEntityConcept],
   allConceptsIncludingImported: Map[OWLClass, ModelEntityConcept])
  (implicit reasoner: OWLReasoner, backbone: OMFBackbone)
  : Try[Unit] =
    Success(for {
      (subC, subM) <- conceptCMs
      supC <- reasoner.getSuperClasses(subC, true).getFlattened
      supM <- findEntityConcept(supC.getIRI, allConceptsIncludingImported)
    } addEntityConceptSubClassAxiom(
      o = omfMetadata,
      hasProvenanceFromRule = provenance,
      graph = tboxG,
      sub = subM,
      sup = supM
      )(omfStore) match {
        case Failure(f) =>
          return Failure(f)
        case Success(_) =>
          ()
      })

  def resolveReifiedRelationshipSubClassAxioms
  (reifiedRelationshipCMs: Map[OWLClass, ModelEntityReifiedRelationship],
   allReifiedRelationshipsIncludingImported: Map[OWLClass, ModelEntityReifiedRelationship])
  (implicit reasoner: OWLReasoner, backbone: OMFBackbone)
  : Try[Unit] =
    Success(for {
      (subC, subM) <- reifiedRelationshipCMs
      supC <- reasoner.getSuperClasses(subC, true).getFlattened
      supM <- findEntityReifiedRelationship(supC.getIRI, allReifiedRelationshipsIncludingImported)
    } addEntityReifiedRelationshipSubClassAxiom(
        o = omfMetadata,
        hasProvenanceFromRule = provenance,
        graph = tboxG,
        sub = subM,
        sup = supM
      )(omfStore) match {
        case Failure(f) =>
          return Failure(f)
        case Success(_) =>
          ()
      })

  def resolveDefinitionAspectSubClassAxioms
  (allEntityDefinitions: Map[OWLClass, ModelEntityDefinition],
   allAspectsIncludingImported: Map[OWLClass, ModelEntityAspect])
  (implicit reasoner: OWLReasoner, backbone: OMFBackbone)
  : Try[Unit] =
    Success(for {
      (subC, subM) <- allEntityDefinitions
      supC <- reasoner.getSuperClasses(subC, true).getFlattened
      supM <- findEntityAspect(supC.getIRI, allAspectsIncludingImported)
    } addEntityDefinitionAspectSubClassAxiom(
        o = omfMetadata,
        hasProvenanceFromRule = provenance,
        graph = tboxG,
        sub=subM,
      sup=supM)(omfStore) match {
        case Failure(f) =>
          return Failure(f)
        case Success(_) =>
          ()
      })
}

case class ImmutableModelTerminologyGraphResolver(resolver: ResolverHelper) {

  require(null != resolver)

  import resolver._
  import resolver.omfStore.ops._

  def resolve: Try[ImmutableModelTerminologyGraph] = {
    val dTs = ont.getDatatypesInSignature(Imports.EXCLUDED).filter(ont.isDeclared)

    val scalarDatatypeSCs = for {
      scalarDatatypeDT <- dTs
      scalarDatatypeIRI = scalarDatatypeDT.getIRI
      scalarDatatypeSC = addScalarDataType(
        o = resolver.omfMetadata,
        hasProvenanceFromRule = provenance,
        graph = tboxG,
        scalarName = getIRIFragment(scalarDatatypeIRI),
        hasName = getOWLTermShortName(scalarDatatypeIRI),
        hasUUID = getOWLTermUUID(scalarDatatypeIRI)) match {
        case Failure(f) =>
          return Failure(f)
        case Success(c) =>
          c
      }
    } yield scalarDatatypeDT -> scalarDatatypeSC

    val (bCs, tCs) = ont.
      getClassesInSignature(Imports.EXCLUDED).
      filter(ont.isDeclared).
      partition { c => isBackboneIRI(c.getIRI) }

    val (bOPs, tOPs) = ont.
      getObjectPropertiesInSignature(Imports.EXCLUDED).
      filter(ont.isDeclared).
      partition { c => isBackboneIRI(c.getIRI) }

    val (bDPs, tDPs) = ont.
      getDataPropertiesInSignature(Imports.EXCLUDED).
      filter(ont.isDeclared).
      partition { c => isBackboneIRI(c.getIRI) }

    Backbone.resolveBackbone(ont, bCs.toSet, bOPs.toSet, bDPs.toSet, resolver.omfStore.ops) match {
      case Failure(t) => Failure(t)
      case Success(_: NoBackbone) =>
        val itboxG = asImmutableTerminologyGraph(tboxG)
        itboxG
      case Success(backbone: OMFBackbone) =>
        resolve(backbone, scalarDatatypeSCs.toMap, tCs.toSet, tOPs.toSet, tDPs.toSet)
    }
  }

  def resolve
  (backbone: OMFBackbone,
   scalarDatatypeSCs: Map[OWLDatatype, types.ModelScalarDataType],
   tCs: Set[OWLClass],
   tOPs: Set[OWLObjectProperty],
   tDPs: Set[OWLDataProperty])
  : Try[ImmutableModelTerminologyGraph] = {

    implicit val _backbone = backbone

    val importClosure: Set[ModelTerminologyGraph] = imports.flatMap(
      terminologyGraphImportClosure[OWLAPIOMF, ModelTerminologyGraph]
        (_, onlyCompatibleKind = true)
        (resolver.omfStore.ops, resolver.omfStore)).toSet[ModelTerminologyGraph]

    //    System.out.println(s"ont: ${backbone.ont.getOntologyID} with ${imports.size} imports")
    //    System.out.println(imports.map(_.ont.getOntologyID.toString).toList.sorted.mkString("\n => imports: ", "\n => imports: ", "\n"))
    //
    //    System.out.println(s"import closure: ${importClosure.size}")
    //    System.out.println(importClosure.map(_.ont.getOntologyID.toString).toList.sorted.mkString("\n => imports: ", "\n => imports: ", "\n"))

    val importedScalarDatatypeDefinitionMaps: Map[OWLDatatype, ModelScalarDataType] =
      importClosure.flatMap(_.getScalarDatatypeDefinitionMap) toMap

    val importedEntityDefinitionMaps: Map[OWLClass, ModelEntityDefinition] =
      importClosure.flatMap(_.getEntityDefinitionMap) toMap

    val allImportedReifiedRelationships: Map[OWLClass, ModelEntityReifiedRelationship] =
      importedEntityDefinitionMaps flatMap {
        case (rrC, rrE: ModelEntityReifiedRelationship) => Some(rrC -> rrE)
        case _ => None
      }

    val reasonerFactory = new StructuralReasonerFactory()
    implicit val reasoner = reasonerFactory.createReasoner(ont)

    val thingCIRIs =
      resolveThingCIRIs(
        reasoner.getSubClasses(backbone.ThingC, false),
        tCs)

    val conceptCIRIs =
      resolveConceptCIRIs(
        reasoner.getSubClasses(backbone.EntityC, false),
        tCs)

    val reifiedObjectPropertyCIRIs =
      resolveReifiedObjectPropertyCIRIs(
        reasoner.getSubClasses(backbone.ReifiedObjectPropertyC, false),
        tCs)

    val reifiedStructuredDataPropertyCIRIs =
      resolveReifiedStructuredDataPropertyCIRIs(reasoner.getSubClasses(backbone.ReifiedStructuredDataPropertyC, false), tCs)

    val structuredDatatypeCIRIs =
      resolveStructuredDatatypeCIRIs(reasoner.getSubClasses(backbone.StructuredDatatypeC, false), tCs)

    val nonAspectCs =
      conceptCIRIs.values ++
        reifiedObjectPropertyCIRIs.values ++
        reifiedStructuredDataPropertyCIRIs.values ++
        structuredDatatypeCIRIs.values

    val ontIRIPrefix = resolver.getOntologyIRI.toString

    val aCs = (tCs -- nonAspectCs).filter { c =>
      c.getIRI.toString.startsWith(ontIRIPrefix)
    }

    val aspectCIRIs =
      resolveAspectCIRIs(
        reasoner.getSubClasses(backbone.AspectC, false),
        aCs)

    val subPropertyChainAxioms = ont.getLogicalAxioms(Imports.EXCLUDED).flatMap {
      case ax: OWLSubPropertyChainOfAxiom => Some(ax)
      case _ => None
    }

    val chains: Chains = for {
      ax: OWLSubPropertyChainOfAxiom <- subPropertyChainAxioms.toSet

      sup: OWLObjectProperty <- ax.getSuperProperty match {
        case op: OWLObjectProperty =>
          Some(op)
        case _ =>
          None
      }

      hasSource: OWLObjectProperty <- ax.getPropertyChain.flatMap {
        case inv: OWLObjectInverseOf =>
          inv.getInverse match {
            case op: OWLObjectProperty =>
              Some(op)
            case _ =>
              None
          }
        case _ =>
          None
      } headOption

      hasTarget: OWLObjectProperty <- ax.getPropertyChain.flatMap {
        case op: OWLObjectProperty =>
          Some(op)
        case _ =>
          None
      } headOption

    } yield Tuple3(sup, hasSource, hasTarget)

    val aspectCMs = for {
      (aspectIRI, aspectC) <- aspectCIRIs
      aspectM = addEntityAspect(
        o = resolver.omfMetadata,
        hasProvenanceFromRule = provenance,
        graph = tboxG,
        aspectName = getIRIFragment(aspectIRI)) match {
        case Failure(f) =>
          return Failure(f)
        case Success(a) =>
          a
      }
    } yield aspectC -> aspectM

    val allAspectsIncludingImported: Map[OWLClass, ModelEntityAspect] = aspectCMs ++
      importedEntityDefinitionMaps flatMap {
      case (aC, aE: ModelEntityAspect) => Some(aC -> aE)
      case _ => None
    }

    val conceptCMs = for {
      (conceptIRI, conceptC) <- conceptCIRIs
      conceptM = addEntityConcept(
        o = resolver.omfMetadata,
        hasProvenanceFromRule = provenance,
        graph = tboxG,
        conceptName = getIRIFragment(conceptIRI),
        isAbstract = isAnnotatedAbstract(conceptIRI)) match {
        case Failure(f) =>
          return Failure(f)
        case Success(c) =>
          c
      }
    } yield conceptC -> conceptM

    val allConceptsIncludingImported: Map[OWLClass, ModelEntityConcept] = conceptCMs ++
      importedEntityDefinitionMaps flatMap {
      case (cC, cE: ModelEntityConcept) => Some(cC -> cE)
      case _ => None
    }

    val structuredDatatypeSCs = for {
      (structuredDatatypeIRI, structuredDatatypeC) <- structuredDatatypeCIRIs
      structuredDatatypeST = addStructuredDataType(
        o = resolver.omfMetadata,
        hasProvenanceFromRule = provenance,
        graph = tboxG,
        structureName = getIRIFragment(structuredDatatypeIRI),
        hasName = getOWLTermShortName(structuredDatatypeIRI),
        hasUUID = getOWLTermUUID(structuredDatatypeIRI)) match {
        case Failure(f) =>
          return Failure(f)
        case Success(c) =>
          c
      }
    } yield structuredDatatypeC -> structuredDatatypeST

    val topReifiedObjectPropertySubOPs =
      reasoner.getSubObjectProperties(backbone.topReifiedObjectPropertyOP, false)

    val reifiedObjectPropertyOPIRIs =
      resolveDomainRangeForObjectProperties(topReifiedObjectPropertySubOPs, tOPs)

    val topReifiedObjectPropertySourceSubOPs =
      reasoner.getSubObjectProperties(backbone.topReifiedObjectPropertySourceOP, false)

    val reifiedObjectPropertySourceOPIRIs =
      resolveDomainRangeForObjectProperties(topReifiedObjectPropertySourceSubOPs, tOPs)

    val topReifiedObjectPropertyTargetSubOPs =
      reasoner.getSubObjectProperties(backbone.topReifiedObjectPropertyTargetOP, false)

    val reifiedObjectPropertyTargetOPIRIs =
      resolveDomainRangeForObjectProperties(topReifiedObjectPropertyTargetSubOPs, tOPs)

    val dataPropertyDPIRIs =
      resolveDataPropertyDPIRIs(reasoner.getSubDataProperties(backbone.topDataPropertyDP, false), tDPs)

    val allEntityDefinitionsExceptRelationships =
      importedEntityDefinitionMaps ++
        aspectCMs ++
        conceptCMs

    for {
      _ <- resolveConceptSubClassAxioms(conceptCMs, allConceptsIncludingImported)
      entityReifiedRelationshipCMs <- resolveEntityDefinitionsForRelationships(
        allEntityDefinitionsExceptRelationships,
        reifiedObjectPropertyCIRIs,
        reifiedObjectPropertyOPIRIs,
        reifiedObjectPropertySourceOPIRIs,
        reifiedObjectPropertyTargetOPIRIs,
        chains,
        Map())
      allEntityDefinitions: Map[OWLClass, ModelEntityDefinition] = allEntityDefinitionsExceptRelationships ++
        entityReifiedRelationshipCMs
      _ <- resolveDefinitionAspectSubClassAxioms(allEntityDefinitions, allAspectsIncludingImported)
      _ <- resolveReifiedRelationshipSubClassAxioms(
        entityReifiedRelationshipCMs, allImportedReifiedRelationships)
      allScalarDefinitions: Map[OWLDatatype, ModelScalarDataType] = importedScalarDatatypeDefinitionMaps ++
        scalarDatatypeSCs
      dataRelationshipsFromEntity2Scalars <- resolveDataRelationshipsFromEntity2Scalars(
        allEntityDefinitions, dataPropertyDPIRIs, allScalarDefinitions)
    //      _ = System.out.println(
    //        s"""TBox: ${resolver.getOntologyIRI}
    //          | aspects: ${aspectCMs.values.size}
    //          | concepts: ${conceptCMs.values.size}
    //          | reifiedRelationships: ${entityReifiedRelationshipCMs.values.size}
    //       """.stripMargin
    //      )
    //      _ = aspectCMs.values.toList.sortBy(_.iri.toString).foreach { a =>
    //        System.out.println(s" a: ${a.iri}")
    //      }
      itboxG <- asImmutableTerminologyGraph(tboxG)

    } yield {

      for {
        importG <- imports
        ok = addTerminologyGraphExtension(extendingG = itboxG, extendedG = importG)
      } ok match {
        case Failure(f) =>
          return Failure(f)
        case Success(_) =>
          ()
      }

      itboxG
    }
  }
}