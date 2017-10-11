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

import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies._
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.{NodeSet, OWLReasoner}

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.{Boolean, None, Option, Some, StringContext, Unit}
import scala.Predef.{Map => _, Set => _, _}
import scala.language.postfixOps
import scalaz._
import Scalaz._

case class TerminologyBoxResolverHelper
(omfMetadata: Option[OWLOntology],
 tboxG: MutableTerminologyBox,
 imports: Iterable[ImmutableTerminologyBox],
 ont: OWLOntology,
 omfStore: OWLAPIOMFGraphStore,
 om: OntologyMapping) {

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
  : Set[TerminologyBox]
  = terminologyBoxImportClosure[OWLAPIOMF](tboxG)(omfStore.ops, omfStore)

  val provenance = s"load($getOntologyIRI)"

  // Lookup of entity aspects

  def findDirectEntityAspect(iri: IRI, aspects: Map[OWLClass, Aspect])
  : Option[Aspect]
  = (for {
    (aspectC, aspectM) <- aspects
    if iri == aspectC.getIRI
  } yield aspectM) headOption

  def findImportedEntityAspect(iri: IRI)
  : Option[Aspect]
  = (for {
    g <- importClosure
    aspectM <- omfStore.ops.lookupAspect(g, iri, recursively = false)
  } yield aspectM) headOption

  def findEntityAspect(iri: IRI, aspects: Map[OWLClass, Aspect])
  : Option[Aspect]
  = findDirectEntityAspect(iri, aspects) orElse findImportedEntityAspect(iri)

  // Lookup of entity concepts

  def findDirectEntityConcept(iri: IRI, concepts: Map[OWLClass, Concept])
  : Option[Concept]
  = (for {
    (conceptC, conceptM) <- concepts
    if iri == conceptC.getIRI
  } yield conceptM) headOption

  def findImportedEntityConcept(iri: IRI)
  : Option[Concept]
  = (for {
    g <- importClosure
    conceptM <- omfStore.ops.lookupConcept(g, iri, recursively = false)
  } yield conceptM) headOption

  def findEntityConcept(iri: IRI, concepts: Map[OWLClass, Concept])
  : Option[Concept]
  = findDirectEntityConcept(iri, concepts) orElse findImportedEntityConcept(iri)

  // Lookup of entity relationships

  def findDirectEntityReifiedRelationship
  (iri: IRI, relationships: Map[OWLClass, ReifiedRelationship])
  : Option[ReifiedRelationship]
  = (for {
    (conceptC, conceptM) <- relationships
    if iri == conceptC.getIRI
  } yield conceptM) headOption

  def findImportedEntityReifiedRelationship
  (iri: IRI)
  : Option[ReifiedRelationship]
  = (for {
    g <- importClosure
    conceptM <- omfStore.ops.lookupReifiedRelationship(g, iri, recursively = false)
  } yield conceptM) headOption

  def findEntityReifiedRelationship
  (iri: IRI, relationships: Map[OWLClass, ReifiedRelationship])
  : Option[ReifiedRelationship]
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
  (entityDefinitions: Map[OWLClass, Entity],
   dataPropertyDPIRIs: Iterable[DOPInfo],
   DTs: Map[OWLDatatype, DataRange])
  : Set[java.lang.Throwable] \/ Vector[EntityScalarDataProperty]
  = {
    type Acc = Set[java.lang.Throwable] \/ (Vector[DOPInfo], Vector[EntityScalarDataProperty])

    def DOPInfo_E2SC_append
    (x1: (Vector[DOPInfo], Vector[EntityScalarDataProperty]),
     x2: => (Vector[DOPInfo], Vector[EntityScalarDataProperty]) )
    : (Vector[DOPInfo], Vector[EntityScalarDataProperty])
    = {
      val (dop1, e2sc1) = x1
      val (dop2, e2sc2) = x2
      val rdop = dop1.filterNot(dop2.contains(_))
      (rdop, e2sc1 ++ e2sc2)
    }

    implicit val DOPInfo_E2SC_Semigroup
    : Semigroup[(Vector[DOPInfo], Vector[EntityScalarDataProperty])]
    = Semigroup.instance(DOPInfo_E2SC_append _)

    dataPropertyDPIRIs
      .foldLeft[Set[java.lang.Throwable] \/ (Vector[DOPInfo], Vector[EntityScalarDataProperty])] {
      (dataPropertyDPIRIs.to[Vector], Vector.empty[EntityScalarDataProperty])
        .right[Set[java.lang.Throwable]]
    } {
      (acc, dataPropertyDPIRI) =>
        val (e2sc_dp, e2sc_source, e2sc_target) = dataPropertyDPIRI
        val isIdentityCriteria: Boolean = false // ??? // TODO
        entityDefinitions.get(e2sc_source).fold[Acc](acc) { e2sc_sourceDef =>
          DTs.get(e2sc_target).fold[Acc]({
            System.out.println(s"DOPInfo_E2SC_append: $e2sc_dp (source: $e2sc_source) failed to find: $e2sc_target")
            acc
          }) { e2sc_targetDef =>
            acc
              .+++(
                tboxG
                  .createDataRelationshipFromEntityToScalar(tboxG.uuid, e2sc_dp, isIdentityCriteria, e2sc_sourceDef, e2sc_targetDef)
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

  def hasRelationshipCharacteristic
  (rc: RelationshipCharacteristics.RelationshipCharacteristics,
   required: Map[OWLObjectProperty, Boolean],
   optional: Option[(OWLObjectProperty, Boolean)] = None)
  : Option[RelationshipCharacteristics.RelationshipCharacteristics]
  = {
    if (required.values.forall(true == _)) {
      optional.fold[Option[RelationshipCharacteristics.RelationshipCharacteristics]](Some(rc)) { case (op, flag) =>
        if (!flag) {
          System.out.println(s"ERROR: inverse of $rc inconsistent for: $op")
        }
        Some(rc)
      }
    } else if (required.values.forall(false == _)) {
      optional.fold[Option[RelationshipCharacteristics.RelationshipCharacteristics]](None) { case (op, flag) =>
        if (flag) {
          System.out.println(s"ERROR: inverse of $rc inconsistent for: $op")
        }
        None
      }
    } else if (required.values.exists(true == _)) {
      required.foreach {
        case (_, true) =>
          ()
        case (r, false) =>
          System.out.println(s"WARNING: $rc missing for $r")
      }
      optional.fold[Option[RelationshipCharacteristics.RelationshipCharacteristics]](Some(rc)) { case (op, flag) =>
        if (!flag) {
          System.out.println(s"ERROR: inverse of $rc inconsistent for: $op")
        }
        Some(rc)
      }
    } else {
      optional.fold[Option[RelationshipCharacteristics.RelationshipCharacteristics]](None) { case (op, flag) =>
        if (flag) {
          System.out.println(s"ERROR: inverse of $rc inconsistent for: $op")
        }
        None
      }
    }
  }

  def resolveEntityDefinitionsForRelationships
  (entityDefinitions: Map[OWLClass, Entity],
   RCs: Map[IRI, OWLClass],
   ROPs: Iterable[ROPInfo],
   sourceROPs: Iterable[ROPInfo],
   targetROPs: Iterable[ROPInfo],
   chains: Chains,
   entityReifiedRelationships: Map[OWLClass, ReifiedRelationship])
  : Set[java.lang.Throwable] \/ Map[OWLClass, ReifiedRelationship]
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

    val m: Set[java.lang.Throwable] \/ Map[OWLClass, ReifiedRelationship]
    = ( Map[OWLClass, ReifiedRelationship]().right[Set[java.lang.Throwable]] /: chains ) {
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
              .fold[Set[java.lang.Throwable] \/ Map[OWLClass, ReifiedRelationship]](
              acc2
            ){ r_sourceDef =>

              val r_source_iri = r_source.getIRI

              entityDefinitions
                .get(r_target)
                .fold[Set[java.lang.Throwable] \/ Map[OWLClass, ReifiedRelationship]](
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

                        val maybeFunctional
                        = hasRelationshipCharacteristic(
                          RelationshipCharacteristics.isFunctional,
                          Map(
                            s_op -> ont.inverseFunctionalObjectPropertyAxioms(s_op).iterator().hasNext,
                            r_op -> ont.functionalObjectPropertyAxioms(r_op).iterator().hasNext),
                          r_inv_op.map { ui => ui -> ont.inverseFunctionalObjectPropertyAxioms(ui).iterator().hasNext })

                        val maybeInverseFunctional
                        = hasRelationshipCharacteristic(
                          RelationshipCharacteristics.isInverseFunctional,
                          Map(
                            t_op -> ont.inverseFunctionalObjectPropertyAxioms(t_op).iterator().hasNext,
                            r_op -> ont.inverseFunctionalObjectPropertyAxioms(r_op).iterator().hasNext),
                          r_inv_op.map { ui => ui -> ont.functionalObjectPropertyAxioms(ui).iterator().hasNext })

                        val maybeSymmetric
                        = hasRelationshipCharacteristic(
                          RelationshipCharacteristics.isSymmetric,
                          Map(
                            r_op -> ont.symmetricObjectPropertyAxioms(r_op).iterator().hasNext),
                          r_inv_op.map { ui => ui -> ont.symmetricObjectPropertyAxioms(ui).iterator().hasNext })

                        val maybeAsymmetric
                        = hasRelationshipCharacteristic(
                          RelationshipCharacteristics.isAsymmetric,
                          Map(
                            r_op -> ont.asymmetricObjectPropertyAxioms(r_op).iterator().hasNext),
                          r_inv_op.map { ui => ui -> ont.asymmetricObjectPropertyAxioms(ui).iterator().hasNext })

                        val maybeReflexive
                        = hasRelationshipCharacteristic(
                          RelationshipCharacteristics.isReflexive,
                          Map(
                            r_op -> ont.reflexiveObjectPropertyAxioms(r_op).iterator().hasNext),
                          r_inv_op.map { ui => ui -> ont.reflexiveObjectPropertyAxioms(ui).iterator().hasNext })

                        val maybeIrreflexive
                        = hasRelationshipCharacteristic(
                          RelationshipCharacteristics.isIrreflexive,
                          Map(
                            r_op -> ont.irreflexiveObjectPropertyAxioms(r_op).iterator().hasNext),
                          r_inv_op.map { ui => ui -> ont.irreflexiveObjectPropertyAxioms(ui).iterator().hasNext })

                        val maybeEssential
                        = ont.subClassAxiomsForSubClass(r_source).toScala[Set]
                          .flatMap { ax =>
                            ax.getSuperClass() match {
                              case oex: OWLObjectExactCardinality
                                if 1 == oex.getCardinality &&
                                  r_op == oex.getProperty &&
                                  r_target == oex.getFiller =>
                                Some(RelationshipCharacteristics.isEssential)
                              case _ =>
                                None
                            }
                          }
                          .headOption

                        val maybeInverseEssential
                        = r_inv_op.flatMap { ui =>
                          ont.subClassAxiomsForSubClass(r_target).toScala[Set]
                            .flatMap { ax =>
                              ax.getSuperClass() match {
                                case oex: OWLObjectExactCardinality
                                  if 1 == oex.getCardinality &&
                                    ui == oex.getProperty &&
                                    r_source == oex.getFiller =>
                                  Some(RelationshipCharacteristics.isEssential)
                                case _ =>
                                  None
                              }
                            }
                            .headOption
                        }

                        val newERR
                        : Set[java.lang.Throwable] \/ Map[OWLClass, ReifiedRelationship]
                        = tboxG
                          .createEntityReifiedRelationship(
                            tboxG.uuid,
                            r = rc,
                            u = r_op, ui = r_inv_op,
                            source = r_sourceDef, rSource = chainSource,
                            target = r_targetDef, rTarget = chainTarget,
                            characteristics =
                              Iterable() ++
                                maybeFunctional ++ maybeInverseFunctional ++
                                maybeSymmetric ++ maybeAsymmetric ++
                                maybeReflexive ++ maybeIrreflexive ++
                                maybeEssential ++ maybeInverseEssential
                          )
                          .flatMap { _rr =>

                            // val rcIRI = rc.getIRI
                            val entry
                            : Set[java.lang.Throwable] \/ Map[OWLClass, ReifiedRelationship]
                            = omfStore.registerOMFModelEntityReifiedRelationshipInstance(tboxG, _rr)
                              .map[Map[OWLClass, ReifiedRelationship]] { _ =>
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

  def resolveUnreifiedRelationships
  (subOPs: NodeSet[OWLObjectPropertyExpression],
   entities: Map[OWLClass, Entity])
  (implicit reasoner: OWLReasoner)
  : Set[java.lang.Throwable] \/ Map[OWLObjectProperty, UnreifiedRelationship]
  = subOPs
    .toSeq
    .filterNot(_.isBottomNode)
    .foldLeft[Set[java.lang.Throwable] \/ Map[OWLObjectProperty, UnreifiedRelationship]](Map.empty.right) {
    case (acc, subOP) =>
      for {
        m1 <- acc
        m2 <- subOP.foldLeft[Set[java.lang.Throwable] \/ Map[OWLObjectProperty, UnreifiedRelationship]](m1.right) {
          case (mi, exp) =>
            for {
              mj <- mi
              op <- exp match {
                case o: OWLObjectProperty =>
                  o.right
                case inv: OWLObjectInverseOf =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"resolveUnreifiedRelationships: OWLObjectPropertyInverseOf not allowed: $exp")).left
              }
              d <- reasoner.getObjectPropertyDomains(op, true).entities().toScala[List] match {
                case ld :: Nil =>
                  ld.right
                case lds =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"resolveUnreifiedRelationships: ObjectProperty must have a single domain class: $op, got: ${lds.size}:" +
                    lds.map(_.getIRI).mkString("\n# ", "\n# ", "\n"))).left
              }
              ed <- entities.get(d) match {
                case Some(e) =>
                  e.right
                case None =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"resolveUnreifiedRelationships: ObjectProperty $op domain is not an entity: $d")).left
              }
              r <- reasoner.getObjectPropertyRanges(op, true).entities().toScala[List] match {
                case lr :: Nil =>
                  lr.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"resolveUnreifiedRelationships: ObjectProperty must have a single range class: $op")).left
              }
              er <- entities.get(r) match {
                case Some(e) =>
                  e.right
                case None =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"resolveUnreifiedRelationships: ObjectProperty $op range is not an entity: $r")).left
              }

              maybeFunctional
              = hasRelationshipCharacteristic(
                RelationshipCharacteristics.isFunctional,
                Map(op -> ont.inverseFunctionalObjectPropertyAxioms(op).iterator().hasNext))

              maybeInverseFunctional
              = hasRelationshipCharacteristic(
                RelationshipCharacteristics.isInverseFunctional,
                Map(op -> ont.inverseFunctionalObjectPropertyAxioms(op).iterator().hasNext))

              maybeSymmetric
              = hasRelationshipCharacteristic(
                RelationshipCharacteristics.isSymmetric,
                Map(op -> ont.symmetricObjectPropertyAxioms(op).iterator().hasNext))

              maybeAsymmetric
              = hasRelationshipCharacteristic(
                RelationshipCharacteristics.isAsymmetric,
                Map(op -> ont.asymmetricObjectPropertyAxioms(op).iterator().hasNext))

              maybeReflexive
              = hasRelationshipCharacteristic(
                RelationshipCharacteristics.isReflexive,
                Map(op -> ont.reflexiveObjectPropertyAxioms(op).iterator().hasNext))

              maybeIrreflexive
              = hasRelationshipCharacteristic(
                RelationshipCharacteristics.isIrreflexive,
                Map(op -> ont.irreflexiveObjectPropertyAxioms(op).iterator().hasNext))

              maybeEssential
              = ont.subClassAxiomsForSubClass(d).toScala[Set]
                .flatMap { ax =>
                  ax.getSuperClass() match {
                    case oex: OWLObjectExactCardinality
                      if 1 == oex.getCardinality &&
                        op == oex.getProperty &&
                        r == oex.getFiller =>
                      Some(RelationshipCharacteristics.isEssential)
                    case _ =>
                      None
                  }
                }
                .headOption

              ur <- tboxG
                .createEntityUnreifiedRelationship(
                  tboxG.uuid,
                  op, ed, er,
                  Iterable[RelationshipCharacteristics.RelationshipCharacteristics]() ++
                    maybeFunctional ++
                    maybeInverseFunctional ++
                    maybeSymmetric ++
                    maybeAsymmetric ++
                    maybeReflexive ++
                    maybeIrreflexive ++
                    maybeEssential)
            } yield mj + (op -> ur)
        }
      } yield m2
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

  def owlclassOfCE(ce: Option[OWLClassExpression])
  : Option[OWLClass]
  = ce match {
      case Some(subc: OWLClass) =>
        Some(subc)
      case _ =>
        None
    }

  def resolveConceptSubClassAxioms
  (allConceptsIncludingImported: Map[OWLClass, Concept])
  (implicit reasoner: OWLReasoner, backbone: OMFBackbone)
  : Set[java.lang.Throwable] \/ Unit
  = {
    val subaxs = ont.axioms(AxiomType.SUBCLASS_OF, Imports.EXCLUDED).toScala[Set]

    val sub_sup = for {
      subax <- subaxs
      subC <- owlclassOfCE(Option.apply(subax.getSubClass))
      subM <- allConceptsIncludingImported.get(subC)
      supC <- owlclassOfCE(Option.apply(subax.getSuperClass))
      supM <- allConceptsIncludingImported.get(supC)
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
  (allReifiedRelationshipsIncludingImported: Map[OWLClass, ReifiedRelationship])
  (implicit reasoner: OWLReasoner, backbone: OMFBackbone)
  : Set[java.lang.Throwable] \/ Unit
  = {
    val subaxs = ont.axioms(AxiomType.SUBCLASS_OF, Imports.EXCLUDED).toScala[Set]

    val sub_sup = for {
      subax <- subaxs
      subC <- owlclassOfCE(Option.apply(subax.getSubClass))
      subM <- allReifiedRelationshipsIncludingImported.get(subC)
      supC <- owlclassOfCE(Option.apply(subax.getSuperClass))
      supM <- allReifiedRelationshipsIncludingImported.get(supC)
    } yield (subM, supM)

    ( ().right[Set[java.lang.Throwable]] /: sub_sup ) {
      case (acc, (subM, supM)) =>
        for {
          _ <- acc
          uuid <- reifiedRelationshipSpecializationAxiomUUID(tboxG, subM, supM)
          next <- tboxG.createEntityReifiedRelationshipSubClassAxiom(uuid, sub = subM, sup = supM)(omfStore).map(_ => ())
        } yield next
    }
  }

  def resolveDefinitionAspectSubClassAxioms
  (allEntityDefinitions: Map[OWLClass, Entity],
   allAspectsIncludingImported: Map[OWLClass, Aspect])
  (implicit reasoner: OWLReasoner, backbone: OMFBackbone)
  : Set[java.lang.Throwable] \/ Unit
  = {
    val subaxs = ont.axioms(AxiomType.SUBCLASS_OF, Imports.EXCLUDED).toScala[Set]

    val sub_sup = for {
      subax <- subaxs
      subC <- owlclassOfCE(Option.apply(subax.getSubClass))
      subM <- allEntityDefinitions.get(subC)
      supC <- owlclassOfCE(Option.apply(subax.getSuperClass))
      supM <- allAspectsIncludingImported.get(supC)
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