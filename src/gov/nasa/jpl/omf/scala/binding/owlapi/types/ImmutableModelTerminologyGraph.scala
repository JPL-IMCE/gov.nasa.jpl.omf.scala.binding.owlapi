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
package gov.nasa.jpl.omf.scala.binding.owlapi.types

import java.lang.IllegalArgumentException
import java.lang.System

import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import org.semanticweb.owlapi.reasoner.{NodeSet, OWLReasoner}

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.util.Try
import scala.{Boolean, Enumeration, None, Option, Some, StringContext, Tuple3, Unit}
import scala.Predef.{Map => _, Set => _, _}
import scala.language.postfixOps
import scalaz._
import Scalaz._

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
 override val extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance],
 override protected val aspects: Vector[ModelEntityAspect],
 override protected val concepts: Vector[ModelEntityConcept],
 override protected val reifiedRelationships: Vector[ModelEntityReifiedRelationship],
 override protected val unreifiedRelationships: Vector[ModelEntityUnreifiedRelationship],
 override protected val sc: Vector[ModelScalarDataType],
 override protected val st: Vector[ModelStructuredDataType],
 override protected val e2sc: Vector[ModelDataRelationshipFromEntityToScalar],
 override protected val e2st: Vector[ModelDataRelationshipFromEntityToStructure],
 override protected val s2sc: Vector[ModelDataRelationshipFromStructureToScalar],
 override protected val s2st: Vector[ModelDataRelationshipFromStructureToStructure],
 override protected val ax: Vector[ModelTermAxiom])
(override implicit val ops: OWLAPIOMFOps)
  extends ModelTerminologyGraph(kind, ont, extraProvenanceMetadata)(ops) {

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

  override val mutabilityKind: String = "immutable"
  override val isImmutableModelTerminologyGraph = true
  override val isMutableModelTerminologyGraph = false

  override val kindIRI: IRI = makeKindIRI("immutable")

  val getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition] =
    ((aspects map (a => a.e -> a)) ++
      (concepts map (c => c.e -> c)) ++
      (reifiedRelationships map (r => r.e -> r))) toMap

  val getScalarDatatypeDefinitionMap: Map[OWLDatatype, ModelScalarDataType] =
    sc map (t => t.sc -> t) toMap

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
( omfMetadata: OWLOntology,
  tboxG: MutableModelTerminologyGraph,
  imports: Iterable[ImmutableModelTerminologyGraph],
  ont: OWLOntology,
  omfStore: OWLAPIOMFGraphStore) {

  require(null != omfMetadata)
  require(null != imports)
  require(null != ont)
  require(null != omfStore)

  import omfStore.ops._

  val LOG: Boolean = "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ResolverHelper1")
  val LOG1: Boolean = "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ResolverHelper2")

  implicit val store = omfStore

  def getOntologyIRI: IRI =
    ont.getOntologyID.getOntologyIRI.get

  val provenance = s"load($getOntologyIRI)"

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
      dataPropertyN <- subDPs.to[Iterable]
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
  : Set[java.lang.Throwable] \/ Vector[ModelDataRelationshipFromEntityToScalar] = {

    type Acc = Set[java.lang.Throwable] \/ (Vector[DOPInfo], Vector[ModelDataRelationshipFromEntityToScalar])

    def DOPInfo_E2SC_append
    ( x1: (Vector[DOPInfo], Vector[ModelDataRelationshipFromEntityToScalar]),
      x2: => (Vector[DOPInfo], Vector[ModelDataRelationshipFromEntityToScalar]) )
    : (Vector[DOPInfo], Vector[ModelDataRelationshipFromEntityToScalar])
    = {
      val (dop1, e2sc1) = x1
      val (dop2, e2sc2) = x2
      val rdop = dop1.filterNot(dop2.contains(_))
      (rdop, e2sc1 ++ e2sc2)
    }

    implicit val DOPInfo_E2SC_Semigroup
    : Semigroup[(Vector[DOPInfo], Vector[ModelDataRelationshipFromEntityToScalar])] =
      Semigroup.instance(DOPInfo_E2SC_append _)

    ( (dataPropertyDPIRIs.to[Vector], Vector.empty[ModelDataRelationshipFromEntityToScalar])
      .right[Set[java.lang.Throwable]] /: dataPropertyDPIRIs ) {
      (acc, dataPropertyDPIRI) =>
        val (e2sc_dp, e2sc_source, e2sc_target) = dataPropertyDPIRI
        entityDefinitions.get(e2sc_source).fold[Acc](acc) { e2sc_sourceDef =>
          DTs.get(e2sc_target).fold[Acc]({
            System.out.println(s"DOPInfo_E2SC_append: $e2sc_dp (source: $e2sc_source) failed to find: $e2sc_target")
            acc
          }) { e2sc_targetDef =>
              acc
              .+++(
                tboxG
                  .createDataRelationshipFromEntityToScalar(e2sc_dp, e2sc_sourceDef, e2sc_targetDef)
                  .map(r =>
                    (Vector(dataPropertyDPIRI), Vector(r))
                  )
              )(M1=DOPInfo_E2SC_Semigroup, M2=implicitly)
          }
        }
    }
    .flatMap { case (remainingDataPropertyDPIRIs, e2sc) =>
        if (remainingDataPropertyDPIRIs.isEmpty)
          e2sc.right
        else {
          val message =
            s"resolveDataRelationshipsFromEntity2Scalars: ${e2sc.size} resolved, ${remainingDataPropertyDPIRIs.size} unresolved scalar data property relations" +
              remainingDataPropertyDPIRIs
                .map { case (e2sc_dp, e2sc_source, e2sc_target) =>
                  s"dp: $e2sc_dp domain: $e2sc_source range: $e2sc_target"
                }
                .mkString("\n","\n","\n")
          Set(
            OMFError.omfOpsError(omfStore.ops, message)
          ).left
        }
    }
  }

  // ----------

  type ROPInfo = (IRI, OWLObjectProperty, OWLClass, OWLClass, Option[OWLObjectProperty])

  def ropInfoToString(ropInfo: ROPInfo): String =
    s"""|ROPInfo 
        |iri=${ropInfo._1}
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
  : Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityReifiedRelationship] = {

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

    val m: Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityReifiedRelationship] =
    ( Map[OWLClass, ModelEntityReifiedRelationship]().right[Set[java.lang.Throwable]] /: chains ) {
      case (acc1, (chainOP, chainSource, chainTarget)) =>

        val chainOP_iri = chainOP.getIRI
        val chainSource_iri = chainSource.getIRI
        val chainTarget_iri = chainTarget.getIRI

        (acc1 /: resolvableROPs) {
          case (acc2, (r_iri, r_op, r_source, r_target, r_inv_op)) if
          chainOP_iri == r_iri =>
            if (LOG1) {
              System.out.println(s"r_op: matches!")
              System.out.println(s"r_op: chainSource: ${chainSource.getIRI} chainTarget: ${chainTarget.getIRI}")
              System.out.println(s"r_op: r_source: ${r_source.getIRI} r_target: ${r_target.getIRI}")
            }

            entityDefinitions
            .get(r_source)
            .fold[Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityReifiedRelationship]](
              acc2
            ){ r_sourceDef =>

              val r_source_iri = r_source.getIRI

              entityDefinitions
              .get(r_target)
              .fold[Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityReifiedRelationship]](
                  acc2
              ){ r_targetDef =>

                val r_target_iri = r_target.getIRI

                ( acc2 /: resolvableSourceROPs ) {
                  case (accS, (s_iri, s_op, s_source, s_target, s_inv_op)) if
                    s_iri == chainSource_iri && s_target.getIRI == r_source_iri =>

                    if (LOG1) {
                      System.out.println(s"s_op: ${s_op.getIRI}")
                    }

                    ( accS /: resolvableTargetROPs ) {

                      case (accT, (t_iri, t_op, t_source, t_target, t_inv_op)) if
                      t_iri == chainTarget_iri && t_target.getIRI == r_target_iri && s_source == t_source =>

                        if (LOG1) {
                          System.out.println(s"t_op: ${t_op.getIRI}")
                        }

                        val rc = s_source
                        val resolvedROP = (r_iri, r_op, r_source, r_target, r_inv_op)
                        val resolvedSourceROP = (s_iri, s_op, s_source, s_target, s_inv_op)
                        val resolvedTargetROP = (t_iri, t_op, t_source, t_target, t_inv_op)

                        val newERR
                        : Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityReifiedRelationship] =
                          tboxG
                          .createEntityReifiedRelationship(
                            r = rc,
                            u = r_op, ui = r_inv_op,
                            source = r_sourceDef, rSource = chainSource,
                            target = r_targetDef, rTarget = chainTarget,
                            characteristics = Iterable(),
                            isAbstract = isAnnotatedAbstract(rc.getIRI)
                          )
                          .flatMap { _rr =>

                            val rcIRI = rc.getIRI
                            val entry
                            : Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityReifiedRelationship]
                            = ( tboxG.setTermShortName(_rr, getOWLTermShortName(rcIRI)) +++
                                tboxG.setTermUUID(_rr, getOWLTermUUID(rcIRI)) +++
                                omfStore.registerOMFModelEntityReifiedRelationshipInstance(tboxG, _rr).map(_ => ())
                              ).map[Map[OWLClass, ModelEntityReifiedRelationship]] { _ =>
                                if (LOG1) {
                                  System.out.println(s"rop=$r_iri $s_iri $t_iri")
                                }
                                remainingROPs -= resolvedROP
                                remainingSourceROPs -= resolvedSourceROP
                                remainingTargetROPs -= resolvedTargetROP
                                if (LOG1) {
                                  System.out.println(
                                    s"""|resolveEntityDefinitionsForRelationship:
                                        |${_rr}
                                        |""".stripMargin('|')
                                  )
                                }
                                Map(rc -> _rr)
                              }
                            entry
                          }

                          accT +++ newERR

                        case (
                          accT, _) =>accT
                      }

                    case (accS, _) =>
                      accS
                  }
                }
              }
            case ( acc2, _ ) =>
              acc2
          }
    }

    if ((remainingROPs.isEmpty && (remainingSourceROPs.nonEmpty || remainingTargetROPs.nonEmpty)) ||
        (remainingSourceROPs.isEmpty && (remainingROPs.nonEmpty || remainingTargetROPs.nonEmpty)) ||
        (remainingTargetROPs.isEmpty && (remainingROPs.nonEmpty || remainingSourceROPs.nonEmpty)) ||
        remainingROPs.size != remainingSourceROPs.size ||
        remainingROPs.size != remainingTargetROPs.size) {

      val rops = remainingROPs.toList.sortBy(_._1.toString).map(ropInfoToString).mkString("\n")
      val srops = remainingSourceROPs.toList.sortBy(_._1.toString).map(ropInfoToString).mkString("\n")
      val trops = remainingTargetROPs.toList.sortBy(_._1.toString).map(ropInfoToString).mkString("\n")

      m +++
      Set(
        OMFError
        .omfOpsError(
          omfStore.ops,
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
              |""".stripMargin('|'))
      ).left
    } else
    if (remainingROPs.isEmpty && remainingSourceROPs.isEmpty && remainingTargetROPs.isEmpty &&
        unresolvedROPs.isEmpty && unresolvableSourceROPs.isEmpty && unresolvableTargetROPs.isEmpty)

      m +++ entityReifiedRelationships.right

    else
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
      m.flatMap { _m =>

        if (_m.isEmpty)
          entityReifiedRelationships.right
        else {
          //      System.out.println(s"\n#resolveEntityDefinitionsForRelationships with ${m.size}")
          resolveEntityDefinitionsForRelationships(
            entityDefinitions ++ _m,
            RCs ++ (_m map { case (rc, _) => rc.getIRI -> rc }).toMap,
            unresolvedROPs ++ remainingROPs,
            unresolvableSourceROPs ++ remainingSourceROPs,
            unresolvableTargetROPs ++ remainingTargetROPs,
            chains,
            entityReifiedRelationships ++ _m)
        }
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
  : Set[java.lang.Throwable] \/ Unit = {
    val sub_sup = for {
      (subC, subM) <- conceptCMs
      supC <- reasoner.getSuperClasses(subC, true).getFlattened
      supM <- findEntityConcept(supC.getIRI, allConceptsIncludingImported)
    } yield (subM, supM)

    ( ().right[Set[java.lang.Throwable]] /: sub_sup ) {
      case (acc, (subM, supM)) =>
        acc +++ tboxG.createEntityConceptSubClassAxiom(sub = subM, sup = supM)(omfStore).map(_ => ())
    }
  }

  def resolveReifiedRelationshipSubClassAxioms
  (reifiedRelationshipCMs: Map[OWLClass, ModelEntityReifiedRelationship],
   allReifiedRelationshipsIncludingImported: Map[OWLClass, ModelEntityReifiedRelationship])
  (implicit reasoner: OWLReasoner, backbone: OMFBackbone)
  : Set[java.lang.Throwable] \/ Unit = {
    val sub_sup = for {
      (subC, subM) <- reifiedRelationshipCMs
      supC <- reasoner.getSuperClasses(subC, true).getFlattened
      supM <- findEntityReifiedRelationship(supC.getIRI, allReifiedRelationshipsIncludingImported)
    } yield (subM, supM)

    ( ().right[Set[java.lang.Throwable]] /: sub_sup ) {
      case (acc, (subM, supM)) =>
        acc +++ tboxG.createEntityReifiedRelationshipSubClassAxiom(sub = subM, sup = supM)(omfStore).map(_ => ())
    }
  }

  def resolveDefinitionAspectSubClassAxioms
  (allEntityDefinitions: Map[OWLClass, ModelEntityDefinition],
   allAspectsIncludingImported: Map[OWLClass, ModelEntityAspect])
  (implicit reasoner: OWLReasoner, backbone: OMFBackbone)
  : Set[java.lang.Throwable] \/ Unit = {

    val sub_sup = for {
      (subC, subM) <- allEntityDefinitions
      supC <- reasoner.getSuperClasses(subC, true).getFlattened
      supM <- findEntityAspect(supC.getIRI, allAspectsIncludingImported)
    } yield (subM, supM)

    ( ().right[Set[java.lang.Throwable]] /: sub_sup ) {
      case (acc, (subM, supM)) =>
        acc +++ tboxG.createEntityDefinitionAspectSubClassAxiom(sub = subM, sup = supM)(omfStore).map(_ => ())
    }
  }
}

case class ImmutableModelTerminologyGraphResolver(resolver: ResolverHelper) {

  val LOG: Boolean = "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ImmutableModelTerminologyGraphResolver1")
  val LOG1: Boolean = "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ImmutableModelTerminologyGraphResolver2")

  require(null != resolver)

  import resolver._
  import resolver.omfStore.ops._

  def resolve()
  : Set[java.lang.Throwable] \/ (ImmutableModelTerminologyGraph, Mutable2IMutableTerminologyMap)
  = {
    val dTs = ont.getDatatypesInSignature(Imports.EXCLUDED).filter(ont.isDeclared)

    ( Map[OWLDatatype, ModelScalarDataType]()
      .right[Set[java.lang.Throwable]] /: dTs ) {
      (acc, scalarDatatypeDT) =>
        acc +++
          tboxG
            .createModelScalarDataType(scalarDatatypeDT)
            .flatMap { scalarDatatypeSC =>
              val scIRI = scalarDatatypeDT.getIRI
              (tboxG.setTermShortName(scalarDatatypeSC, getOWLTermShortName(scIRI)) +++
                tboxG.setTermUUID(scalarDatatypeSC, getOWLTermUUID(scIRI)) +++
                resolver.omfStore.registerOMFModelScalarDataTypeInstance(tboxG, scalarDatatypeSC).map(_ => ())
                )
                .map(_ => Map(scalarDatatypeDT -> scalarDatatypeSC))
            }
    }.flatMap { scalarDatatypeSCs =>

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

      Backbone
        .resolveBackbone(ont, bCs.toSet, bOPs.toSet, bDPs.toSet, resolver.omfStore.ops)
        .flatMap {
          case backbone: OMFBackbone =>
            resolve(backbone, scalarDatatypeSCs, tCs.toSet, tOPs.toSet, tDPs.toSet)

          case _: NoBackbone =>
            asImmutableTerminologyGraph(tboxG)
        }
    }
  }

  def resolve
  (backbone: OMFBackbone,
   scalarDatatypeSCs: Map[OWLDatatype, types.ModelScalarDataType],
   tCs: Set[OWLClass],
   tOPs: Set[OWLObjectProperty],
   tDPs: Set[OWLDataProperty])
  : Set[java.lang.Throwable] \/ (ImmutableModelTerminologyGraph, Mutable2IMutableTerminologyMap) = {

    implicit val _backbone = backbone

    val importClosure: Set[ModelTerminologyGraph] = imports.flatMap(
      terminologyGraphImportClosure[OWLAPIOMF, ModelTerminologyGraph]
        (_, onlyCompatibleKind = true)
        (resolver.omfStore.ops, resolver.omfStore)).toSet[ModelTerminologyGraph]

    if (LOG) {
      System.out.println(s"\n\n=>ont: ${backbone.ont.getOntologyID} with ${imports.size} imports")
      System.out.println(imports.map(_.ont.getOntologyID.toString).toList.sorted.mkString("\n => imports: ", "\n => imports: ", "\n"))

      System.out.println(s"import closure: ${importClosure.size}")
      System.out.println(importClosure.map(_.ont.getOntologyID.toString).toList.sorted.mkString("\n => imports: ", "\n => imports: ", "\n"))
    }

    val importedScalarDatatypeDefinitionMaps: Map[OWLDatatype, ModelScalarDataType] =
      importClosure.flatMap(_.getScalarDatatypeDefinitionMap).toMap

    if (LOG) {
      System.out.println(s"importedScalarDatatypeDefinitionMaps: ${importedScalarDatatypeDefinitionMaps.size}")
    }

    val importedEntityDefinitionMaps: Map[OWLClass, ModelEntityDefinition] =
      importClosure.flatMap(_.getEntityDefinitionMap).toMap

    val allImportedDataRelationshipsFromEntityToScalar =
      importClosure.flatMap(_.getDataRelationshipsFromEntityToScalar)

    val allImportedReifiedRelationships: Map[OWLClass, ModelEntityReifiedRelationship] =
      importedEntityDefinitionMaps flatMap {
        case (rrC, rrE: ModelEntityReifiedRelationship) =>
          Some(rrC -> rrE)
        case _ =>
          None
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
      resolveReifiedStructuredDataPropertyCIRIs(
        reasoner.getSubClasses(backbone.ReifiedStructuredDataPropertyC, false),
        tCs)

    val structuredDatatypeCIRIs =
      resolveStructuredDatatypeCIRIs(
        reasoner.getSubClasses(backbone.StructuredDatatypeC, false),
        tCs)

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
      case ax: SWRLRule =>
        Some(ax)
      case _ =>
        None
    }

    val chains: Chains = for {
      rule: SWRLRule <- subPropertyChainAxioms.toSet
      variables: Set[SWRLVariable] = rule.getVariables.toSet
      if 3 == variables.size

      heads: Set[SWRLAtom] = rule.getHead.toSet
      if 1 == heads.size
      head: SWRLObjectPropertyAtom <- heads.head match {
        case opa: SWRLObjectPropertyAtom =>
          Some(opa)
        case _ =>
          None
      }
      head_op: OWLObjectProperty <- head.getPredicate match {
        case op: OWLObjectProperty =>
          Some(op)
        case _ =>
          None
      }
      head_v1: SWRLVariable <- head.getFirstArgument match {
        case v: SWRLVariable => Some(v)
        case _ => None
      }
      head_v2: SWRLVariable <- head.getSecondArgument match {
        case v: SWRLVariable => Some(v)
        case _ => None
      }
      bodies: Set[SWRLAtom] = rule.getBody.toSet
      if 2 == bodies.size
      body1: SWRLObjectPropertyAtom <- bodies.head match {
        case opa: SWRLObjectPropertyAtom =>
          Some(opa)
        case _ =>
          None
      }
      body1_op: OWLObjectProperty <- body1.getPredicate match {
        case op: OWLObjectProperty =>
          Some(op)
        case _ =>
          None
      }
      body1_v1: SWRLVariable <- body1.getFirstArgument match {
        case v: SWRLVariable => Some(v)
        case _ => None
      }
      body1_v2: SWRLVariable <- body1.getSecondArgument match {
        case v: SWRLVariable => Some(v)
        case _ => None
      }
      body2: SWRLObjectPropertyAtom <- bodies.tail.head match {
        case opa: SWRLObjectPropertyAtom =>
          Some(opa)
        case _ =>
          None
      }
      body2_op: OWLObjectProperty <- body2.getPredicate match {
        case op: OWLObjectProperty =>
          Some(op)
        case _ =>
          None
      }
      body2_v1: SWRLVariable <- body2.getFirstArgument match {
        case v: SWRLVariable =>
          Some(v)
        case _ =>
          None
      }
      body2_v2: SWRLVariable <- body2.getSecondArgument match {
        case v: SWRLVariable =>
          Some(v)
        case _ =>
          None
      }
      if body1_v1 == body2_v1

      _ = if (LOG1) {
        System.out.println(s"\nhead op: $head_op, v1: $head_v1, v2: $head_v2")
        System.out.println(s"body1 op: $body1_op, v1: $body1_v1, v2: $body1_v2")
        System.out.println(s"body2 op: $body2_op, v1: $body2_v1, v2: $body2_v2")
      }

      _ = require((head_v1 == body1_v2 && head_v2 == body2_v2) || (head_v1 == body2_v2 && head_v2 == body1_v2))

      hasSource = if (head_v1 == body1_v2 && head_v2 == body2_v2) body1_op else body2_op
      hasTarget = if (head_v1 == body1_v2 && head_v2 == body2_v2) body2_op else body1_op
      _ = if (LOG1) {
        System.out.println(s"hasSource: $hasSource, hasTarget: $hasTarget")
      }
    } yield Tuple3(head_op, hasSource, hasTarget)

    val aspectCMs: Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityAspect] =
      (Map[OWLClass, ModelEntityAspect]().right[Set[java.lang.Throwable]] /: aspectCIRIs) {
        case (acc, (aspectIRI, aspectC)) =>
          acc +++
            tboxG
              .createModelEntityAspect(aspectC)
              .flatMap { aspectM =>

                (tboxG.setTermShortName(aspectM, getOWLTermShortName(aspectIRI)) +++
                  tboxG.setTermUUID(aspectM, getOWLTermUUID(aspectIRI)) +++
                  resolver.omfStore.registerOMFModelEntityAspectInstance(tboxG, aspectM).map(_ => ())
                  )
                  .map { _ =>
                    Map(aspectC -> aspectM)
                  }
              }
      }

    val importedAspectDefinitions: Map[OWLClass, ModelEntityAspect] = importedEntityDefinitionMaps flatMap {
      case (aC, aE: ModelEntityAspect) =>
        Some(aC -> aE)
      case _ =>
        None
    }

    val allAspectsIncludingImported = aspectCMs +++ importedAspectDefinitions.right

    val conceptCMs: Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityConcept] =
      (Map[OWLClass, ModelEntityConcept]().right[Set[java.lang.Throwable]] /: conceptCIRIs) {
        case (acc, (conceptIRI, conceptC)) =>
          acc +++
            tboxG
              .createModelEntityConcept(conceptC, isAnnotatedAbstract(conceptIRI))
              .flatMap { conceptM =>

                (tboxG.setTermShortName(conceptM, getOWLTermShortName(conceptIRI)) +++
                  tboxG.setTermUUID(conceptM, getOWLTermUUID(conceptIRI)) +++
                  resolver.omfStore.registerOMFModelEntityConceptInstance(tboxG, conceptM).map(_ => ())
                  )
                  .map { _ =>
                    Map(conceptC -> conceptM)
                  }
              }
      }

    val importedConceptDefinitions: Map[OWLClass, ModelEntityConcept] = importedEntityDefinitionMaps flatMap {
      case (cC, cE: ModelEntityConcept) =>
        Some(cC -> cE)
      case _ =>
        None
    }

    val allConceptsIncludingImported = conceptCMs +++ importedConceptDefinitions.right

    val structuredDatatypeSCs: Set[java.lang.Throwable] \/ Map[OWLClass, ModelStructuredDataType] =
      (Map[OWLClass, ModelStructuredDataType]().right[Set[java.lang.Throwable]] /: structuredDatatypeCIRIs) {
        case (acc, (structuredDatatypeIRI, structuredDatatypeC)) =>
          acc +++
            tboxG
              .createModelStructuredDataType(structuredDatatypeC)
              .flatMap { structuredDatatypeST =>

                (tboxG.setTermShortName(structuredDatatypeST, getOWLTermShortName(structuredDatatypeIRI)) +++
                  tboxG.setTermUUID(structuredDatatypeST, getOWLTermUUID(structuredDatatypeIRI)) +++
                  resolver.omfStore.registerOMFModelStructuredDataTypeInstance(tboxG, structuredDatatypeST).map(_ => ())
                  )
                  .map { _ =>
                    Map(structuredDatatypeC -> structuredDatatypeST)
                  }
              }
      }

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
      importedEntityDefinitionMaps.right[Set[java.lang.Throwable]] +++
        aspectCMs.map(_.toMap[OWLClass, ModelEntityDefinition]) +++
        conceptCMs.map(_.toMap[OWLClass, ModelEntityDefinition])

    allEntityDefinitionsExceptRelationships.flatMap { _allEntityDefinitionsExceptRelationships =>
      allAspectsIncludingImported.flatMap { _allAspectsIncludingImported =>
        allConceptsIncludingImported.flatMap { _allConceptsIncludingImported =>
          aspectCMs.flatMap { _aspectCMs =>
            conceptCMs.flatMap { _conceptCMs =>
              resolveConceptSubClassAxioms(_conceptCMs, _allConceptsIncludingImported).flatMap { _ =>
                resolveEntityDefinitionsForRelationships(
                  _allEntityDefinitionsExceptRelationships,
                  reifiedObjectPropertyCIRIs,
                  reifiedObjectPropertyOPIRIs,
                  reifiedObjectPropertySourceOPIRIs,
                  reifiedObjectPropertyTargetOPIRIs,
                  chains,
                  Map()
                ).flatMap { _entityReifiedRelationshipCMs =>

                  val _allEntityDefinitions: Map[OWLClass, ModelEntityDefinition] =
                    Map[OWLClass, ModelEntityDefinition]() ++
                      _aspectCMs ++
                      _conceptCMs ++
                      _entityReifiedRelationshipCMs

                  val _allScalarDefinitions: Map[OWLDatatype, ModelScalarDataType] =
                    importedScalarDatatypeDefinitionMaps ++
                      scalarDatatypeSCs

                  (resolveDefinitionAspectSubClassAxioms(_allEntityDefinitions, _allAspectsIncludingImported) +++
                    resolveReifiedRelationshipSubClassAxioms(_entityReifiedRelationshipCMs, allImportedReifiedRelationships)
                    ).flatMap { _ =>

                    resolveDataRelationshipsFromEntity2Scalars(_allEntityDefinitions, dataPropertyDPIRIs, _allScalarDefinitions)
                      .flatMap { dataRelationshipsFromEntity2Scalar =>

                        val allDataRelationshipsFromEntityToScalar =
                          dataRelationshipsFromEntity2Scalar ++ allImportedDataRelationshipsFromEntityToScalar

                        val entityDefinitions
                        : Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityDefinition]
                        = \/-(_allEntityDefinitions)

                        type RestrictionInfoValidation
                        = Set[java.lang.Throwable] \/
                          Set[(ModelEntityDefinition, ModelDataRelationshipFromEntityToScalar, String)]

                        val restrictions
                        : RestrictionInfoValidation
                        = entityDefinitions.flatMap { pairs =>
                          val r0
                          : RestrictionInfoValidation
                          = \/-(Set())

                          val restrictionTuples
                          = pairs.map { case (entityO, entityC) =>
                            val restrictions
                              = getSingleDataPropertyRestrictionsIfAny(resolver.ont, entityO)
                                  .flatMap { restrictions =>
                                    val r1
                                    : RestrictionInfoValidation
                                    = \/-(Set())

                                    val r2
                                    : RestrictionInfoValidation
                                    = (r1 /:
                                      restrictions.map {
                                        case (restrictedC, restrictingDP, restrictingLiteral) =>
                                          val r =
                                            allDataRelationshipsFromEntityToScalar
                                              .find {
                                                _.dp == restrictingDP
                                              }
                                              .fold[RestrictionInfoValidation](
                                              -\/(Set(OMFError.omfError(
                                                s"Unresolved restrincting data property $restrictingDP " +
                                                  s"for entity $restrictedC with " +
                                                  s"literal restriction $restrictingLiteral")))
                                            ) { restrictingSC =>
                                              \/-(Set((entityC, restrictingSC, restrictingLiteral)))
                                            }
                                          r
                                      })
                                    { _ +++ _ }

                                    r2
                                  }
                              restrictions
                          }

                          ( r0 /: restrictionTuples ) { _ +++ _ }
                        }

                        type RestrictionAxiomValidation
                        = Set[java.lang.Throwable] \/
                          Set[ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral]

                        val restrictionAxioms
                        : RestrictionAxiomValidation
                        = restrictions.flatMap { tuples =>

                          val a0
                          : RestrictionAxiomValidation
                          = \/-(Set())

                          val axioms
                          = tuples
                            .map { case ( restrictedC, restrictingE2SC, restrictingLiteral ) =>
                              tboxG
                                .addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(
                                  restrictedC, restrictingE2SC, restrictingLiteral)
                                .map(Set(_))
                            }

                          val aN
                          : RestrictionAxiomValidation
                          = ( a0 /: axioms )
                          { _ +++ _ }

                          aN
                        }

                        restrictionAxioms.flatMap { _ =>

                          asImmutableTerminologyGraph(tboxG)
                            .flatMap { itboxG =>

                              val iimports = fromTerminologyGraph(itboxG._1).imports
                              require(imports.forall(i1 =>
                                iimports.exists(i2 => i2.kindIRI == i1.kindIRI)
                              ))
                              require(iimports.forall(i2 =>
                                imports.exists(i1 => i1.kindIRI == i2.kindIRI)
                              ))

                              itboxG.right

                            }
                        }
                      }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}