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

package gov.nasa.jpl.omf.scala.binding.owlapi.types

import java.lang.System

import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner.{NodeSet, OWLReasoner}

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.{Boolean, None, Option, Some, StringContext, Unit}
import scala.Predef.{Map => _, Set => _, _}
import scala.language.postfixOps
import scalaz._
import Scalaz._

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

  val LOG
  : Boolean
  = "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ResolverHelper1")

  val LOG1
  : Boolean
  = "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ResolverHelper2")

  implicit val store = omfStore

  val getOntologyIRI: IRI = tboxG.iri

  val importClosure
  : Set[ModelTerminologyGraph]
  = terminologyGraphImportClosure[OWLAPIOMF, ModelTerminologyGraph](
    tboxG, onlyCompatibleKind = true)(omfStore.ops, omfStore) + store.getBuiltinDatatypeMapTerminologyGraph

  val provenance = s"load($getOntologyIRI)"

  // Lookup of entity aspects

  def findDirectEntityAspect(iri: IRI, aspects: Map[OWLClass, ModelEntityAspect])
  : Option[ModelEntityAspect]
  = (for {
      (aspectC, aspectM) <- aspects
      if iri == aspectC.getIRI
    } yield aspectM) headOption

  def findImportedEntityAspect(iri: IRI)
  : Option[ModelEntityAspect]
  = (for {
      g <- importClosure
      aspectM <- omfStore.ops.lookupEntityAspect(g, iri, recursively = false)
    } yield aspectM) headOption

  def findEntityAspect(iri: IRI, aspects: Map[OWLClass, ModelEntityAspect])
  : Option[ModelEntityAspect]
  = findDirectEntityAspect(iri, aspects) orElse findImportedEntityAspect(iri)

  // Lookup of entity concepts

  def findDirectEntityConcept(iri: IRI, concepts: Map[OWLClass, ModelEntityConcept])
  : Option[ModelEntityConcept]
  = (for {
      (conceptC, conceptM) <- concepts
      if iri == conceptC.getIRI
    } yield conceptM) headOption

  def findImportedEntityConcept(iri: IRI)
  : Option[ModelEntityConcept]
  = (for {
      g <- importClosure
      conceptM <- omfStore.ops.lookupEntityConcept(g, iri, recursively = false)
    } yield conceptM) headOption

  def findEntityConcept(iri: IRI, concepts: Map[OWLClass, ModelEntityConcept])
  : Option[ModelEntityConcept]
  = findDirectEntityConcept(iri, concepts) orElse findImportedEntityConcept(iri)

  // Lookup of entity relationships

  def findDirectEntityReifiedRelationship
  (iri: IRI, relationships: Map[OWLClass, ModelEntityReifiedRelationship])
  : Option[ModelEntityReifiedRelationship]
  = (for {
      (conceptC, conceptM) <- relationships
      if iri == conceptC.getIRI
    } yield conceptM) headOption

  def findImportedEntityReifiedRelationship
  (iri: IRI)
  : Option[ModelEntityReifiedRelationship]
  = (for {
      g <- importClosure
      conceptM <- omfStore.ops.lookupEntityReifiedRelationship(g, iri, recursively = false)
    } yield conceptM) headOption

  def findEntityReifiedRelationship
  (iri: IRI, relationships: Map[OWLClass, ModelEntityReifiedRelationship])
  : Option[ModelEntityReifiedRelationship]
  = findDirectEntityReifiedRelationship(iri, relationships) orElse findImportedEntityReifiedRelationship(iri)

  // ------

  type DOPInfo = (OWLDataProperty, OWLClass, OWLDatatype)

  def resolveDataPropertyDPIRIs
  (subDPs: NodeSet[OWLDataProperty], tDPs: Set[OWLDataProperty])
  (implicit reasoner: OWLReasoner)
  : Iterable[DOPInfo]
  = for {
      dataPropertyN <- subDPs.to[Iterable]
      dataPropertyDP <- dataPropertyN flatMap { case dp: OWLDataProperty => Some(dp) }
      if tDPs.contains(dataPropertyDP)
      dataPropertyDomain <- reasoner.getDataPropertyDomains(dataPropertyDP, true).entities().toScala[Set]
      dataPropertyRange <- tboxG.ont.dataPropertyRangeAxioms(dataPropertyDP).toScala[Set]
      dataPropertyType = dataPropertyRange.getRange.asOWLDatatype
    } yield (dataPropertyDP, dataPropertyDomain, dataPropertyType)

  def resolveDataRelationshipsFromEntity2Scalars
  (entityDefinitions: Map[OWLClass, ModelEntityDefinition],
   dataPropertyDPIRIs: Iterable[DOPInfo],
   DTs: Map[OWLDatatype, ModelScalarDataType])
  : Set[java.lang.Throwable] \/ Vector[ModelDataRelationshipFromEntityToScalar]
  = {
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
    : Semigroup[(Vector[DOPInfo], Vector[ModelDataRelationshipFromEntityToScalar])]
    = Semigroup.instance(DOPInfo_E2SC_append _)

    dataPropertyDPIRIs
      .foldLeft[Set[java.lang.Throwable] \/ (Vector[DOPInfo], Vector[ModelDataRelationshipFromEntityToScalar])] {
      (dataPropertyDPIRIs.to[Vector], Vector.empty[ModelDataRelationshipFromEntityToScalar])
        .right[Set[java.lang.Throwable]]
      } {
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
            s"resolveDataRelationshipsFromEntity2Scalars: ${e2sc.size} resolved, "+
            s"${remainingDataPropertyDPIRIs.size} unresolved scalar data property relations" +
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

  def ropInfoToString(ropInfo: ROPInfo)
  : String
  = s"""|ROPInfo
        |iri=${ropInfo._1}
        |obj. prop=${ropInfo._2}
        |domain=${ropInfo._3}
        |range=${ropInfo._4}
        |inv o. p.=${ropInfo._5}
        |""".stripMargin('|')

  type Chains = Set[(OWLObjectProperty, OWLObjectProperty, OWLObjectProperty)]

  def chainToString(chain: (OWLObjectProperty, OWLObjectProperty, OWLObjectProperty))
  : String
  = s"""|Chain
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
  : Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityReifiedRelationship]
  = {

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

    val m: Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityReifiedRelationship]
    = ( Map[OWLClass, ModelEntityReifiedRelationship]().right[Set[java.lang.Throwable]] /: chains ) {
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
                          : Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityReifiedRelationship]
                          = tboxG
                              .createEntityReifiedRelationship(
                                r = rc,
                                u = r_op, ui = r_inv_op,
                                source = r_sourceDef, rSource = chainSource,
                                target = r_targetDef, rTarget = chainTarget,
                                characteristics = Iterable()
                              )
                              .flatMap { _rr =>

                                // val rcIRI = rc.getIRI
                                val entry
                                : Set[java.lang.Throwable] \/ Map[OWLClass, ModelEntityReifiedRelationship]
                                = omfStore.registerOMFModelEntityReifiedRelationshipInstance(tboxG, _rr)
                                  .map[Map[OWLClass, ModelEntityReifiedRelationship]] { _ =>
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
  : Map[IRI, OWLClass]
  = (for {
      thingN <- thingSubClasses
      if !thingN.isBottomNode
      thingC = thingN.getRepresentativeElement
      if tCs.contains(thingC)
      thingIRI = thingC.getIRI
    } yield thingIRI -> thingC).toMap

  def resolveAspectCIRIs
  (entitySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass])
  : Map[IRI, OWLClass]
  = (for {
      aspectN <- entitySubClasses
      if !aspectN.isBottomNode
      aspectC = aspectN.getRepresentativeElement
      if tCs.contains(aspectC)
      aspectIRI = aspectC.getIRI
    } yield aspectIRI -> aspectC).toMap

  def resolveConceptCIRIs
  (entitySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass])
  : Map[IRI, OWLClass]
  = (for {
      conceptN <- entitySubClasses
      if !conceptN.isBottomNode
      conceptC = conceptN.getRepresentativeElement
      if tCs.contains(conceptC)
      conceptIRI = conceptC.getIRI
    } yield conceptIRI -> conceptC).toMap

  def resolveReifiedObjectPropertyCIRIs
  (reifiedObjectPropertySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass])
  : Map[IRI, OWLClass]
  = (for {
      reifiedObjectPropertyN <- reifiedObjectPropertySubClasses
      if !reifiedObjectPropertyN.isBottomNode
      reifiedObjectPropertyC = reifiedObjectPropertyN.getRepresentativeElement
      if tCs.contains(reifiedObjectPropertyC)
      reifiedObjectPropertyCIRI = reifiedObjectPropertyC.getIRI
    } yield reifiedObjectPropertyCIRI -> reifiedObjectPropertyC).toMap

  def resolveReifiedStructuredDataPropertyCIRIs
  (reifiedStructuredDataPropertySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass])
  : Map[IRI, OWLClass]
  = (for {
      reifiedStructuredDataPropertyN <- reifiedStructuredDataPropertySubClasses
      if !reifiedStructuredDataPropertyN.isBottomNode
      reifiedStructuredDataPropertyC = reifiedStructuredDataPropertyN.getRepresentativeElement
      if tCs.contains(reifiedStructuredDataPropertyC)
      reifiedStructuredDataPropertyCIRI = reifiedStructuredDataPropertyC.getIRI
    } yield reifiedStructuredDataPropertyCIRI -> reifiedStructuredDataPropertyC).toMap

  def resolveStructuredDatatypeCIRIs
  (structuredDatatypeSubClasses: NodeSet[OWLClass], tCs: Set[OWLClass])
  : Map[IRI, OWLClass]
  = (for {
      structuredDatatypeN <- structuredDatatypeSubClasses
      if !structuredDatatypeN.isBottomNode
      structuredDatatypeC = structuredDatatypeN.getRepresentativeElement
      if tCs.contains(structuredDatatypeC)
      structuredDatatypeCIRI = structuredDatatypeC.getIRI
    } yield structuredDatatypeCIRI -> structuredDatatypeC).toMap

  def resolveDomainRangeForObjectProperties
  (subOPs: NodeSet[OWLObjectPropertyExpression], tOPs: Set[OWLObjectProperty])
  (implicit reasoner: OWLReasoner)
  : Iterable[ROPInfo]
  = (for {
      _n_ <- subOPs
      _op_ <- _n_ flatMap {
        case op: OWLObjectProperty =>
          if (tOPs.contains(op) && !isAnnotatedDerived(tboxG.ont, op.getIRI).fold(_ => false, identity))
            Some(op)
          else
            None
        case inv: OWLObjectInverseOf =>
          inv.getInverse match {
            case op: OWLObjectProperty =>
              if (tOPs.contains(op) && !isAnnotatedDerived(tboxG.ont, op.getIRI).fold(_ => false, identity))
                Some(op)
              else
                None
            case _ =>
              None
          }
      }
      _d_ <- reasoner.getObjectPropertyDomains(_op_, true).entities().toScala[Set]
      _r_ <- reasoner.getObjectPropertyRanges(_op_, true).entities().toScala[Set]
    } yield {
      val INV = (_n_ flatMap {
        case op: OWLObjectProperty =>
          if (tOPs.contains(op) && isAnnotatedDerived(tboxG.ont, op.getIRI).fold(_ => false, identity))
            Some(op)
          else
            None
        case inv: OWLObjectInverseOf =>
          inv.getInverse match {
            case op: OWLObjectProperty =>
              if (tOPs.contains(op) && isAnnotatedDerived(tboxG.ont, op.getIRI).fold(_ => false, identity))
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
  : Set[java.lang.Throwable] \/ Unit
  = {
    val sub_sup = for {
      (subC, subM) <- conceptCMs
      supC <- reasoner.getSuperClasses(subC, true).entities().toScala[Set]
      supM <- findEntityConcept(supC.getIRI, allConceptsIncludingImported)
    } yield (subM, supM)

    ( ().right[Set[java.lang.Throwable]] /: sub_sup ) {
      case (acc, (subM, supM)) =>
        for {
          _ <- acc
          uuid <- conceptSpecializationAxiomUUID(tboxG, subM, supM)
          next <- tboxG.createEntityConceptSubClassAxiom(uuid, sub = subM, sup = supM)(omfStore).map(_ => ())
        } yield next
    }
  }

  def resolveReifiedRelationshipSubClassAxioms
  (reifiedRelationshipCMs: Map[OWLClass, ModelEntityReifiedRelationship],
   allReifiedRelationshipsIncludingImported: Map[OWLClass, ModelEntityReifiedRelationship])
  (implicit reasoner: OWLReasoner, backbone: OMFBackbone)
  : Set[java.lang.Throwable] \/ Unit
  = {
    val sub_sup = for {
      (subC, subM) <- reifiedRelationshipCMs
      supC <- reasoner.getSuperClasses(subC, true).entities().toScala[Set]
      supM <- findEntityReifiedRelationship(supC.getIRI, allReifiedRelationshipsIncludingImported)
    } yield (subM, supM)

    ( ().right[Set[java.lang.Throwable]] /: sub_sup ) {
      case (acc, (subM, supM)) =>
        for {
          _ <- acc
          uuid <- reifiedRelationshipSubClassAxiomUUID(tboxG, subM, supM)
          next <- tboxG.createEntityReifiedRelationshipSubClassAxiom(uuid, sub = subM, sup = supM)(omfStore).map(_ => ())
        } yield next
    }
  }

  def resolveDefinitionAspectSubClassAxioms
  (allEntityDefinitions: Map[OWLClass, ModelEntityDefinition],
   allAspectsIncludingImported: Map[OWLClass, ModelEntityAspect])
  (implicit reasoner: OWLReasoner, backbone: OMFBackbone)
  : Set[java.lang.Throwable] \/ Unit
  = {

    val sub_sup = for {
      (subC, subM) <- allEntityDefinitions
      supC <- reasoner.getSuperClasses(subC, true).entities().toScala[Set]
      supM <- findEntityAspect(supC.getIRI, allAspectsIncludingImported)
    } yield (subM, supM)

    ( ().right[Set[java.lang.Throwable]] /: sub_sup ) {
      case (acc, (subM, supM)) =>
        for {
          _ <- acc
          uuid <- aspectSpecializationAxiomUUID(tboxG, subM, supM)
          next <- tboxG.createEntityDefinitionAspectSubClassAxiom(uuid, sub = subM, sup = supM)(omfStore).map(_ => ())
        } yield next
    }
  }
}