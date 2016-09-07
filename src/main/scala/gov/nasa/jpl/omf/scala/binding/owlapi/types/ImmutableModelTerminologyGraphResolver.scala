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
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory

import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.{Boolean, None, Some, StringContext, Tuple2, Tuple3, Tuple4, Tuple5, Unit}
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._

case class ImmutableModelTerminologyGraphResolver(resolver: ResolverHelper) {

  val LOG: Boolean = "true" equalsIgnoreCase
    java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ImmutableModelTerminologyGraphResolver1")
  val LOG1: Boolean = "true" equalsIgnoreCase
    java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ImmutableModelTerminologyGraphResolver2")

  require(null != resolver)

  import resolver._
  import resolver.omfStore.ops._

  def resolve()
  : Set[java.lang.Throwable] \/ (ImmutableModelTerminologyGraph, Mutable2IMutableTerminologyMap)
  = {
    val dTs = ont.datatypesInSignature(Imports.EXCLUDED).toScala[Set].filter(ont.isDeclared)

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
        classesInSignature(Imports.EXCLUDED).toScala[Set].
        filter(ont.isDeclared).
        partition { c => isBackboneIRI(c.getIRI) }

      val (bOPs, tOPs) = ont.
        objectPropertiesInSignature(Imports.EXCLUDED).toScala[Set].
        filter(ont.isDeclared).
        partition { c => isBackboneIRI(c.getIRI) }

      val (bDPs, tDPs) = ont.
        dataPropertiesInSignature(Imports.EXCLUDED).toScala[Set].
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

  type RemainingAndMatchedRestrictions =
  Set[java.lang.Throwable] \/
  ( Set[(ModelEntityDefinition, ModelEntityReifiedRelationship, ModelEntityDefinition,  RestrictionKind)],
    Set[(ModelEntityDefinition, ModelEntityReifiedRelationship, ModelEntityDefinition,  RestrictionKind)] )

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
      System.out.println(
        imports
          .map(_.ont.getOntologyID.toString)
          .toList
          .sorted
          .mkString("\n => imports: ", "\n => imports: ", "\n"))

      System.out.println(s"import closure: ${importClosure.size}")
      System.out.println(
        importClosure
          .map(_.ont.getOntologyID.toString)
          .toList
          .sorted
          .mkString("\n => imports: ", "\n => imports: ", "\n"))
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

    val subPropertyChainAxioms = ont.logicalAxioms(Imports.EXCLUDED).toScala[Set].flatMap {
      case ax: SWRLRule =>
        Some(ax)
      case _ =>
        None
    }

    val chains: Chains = for {
      rule: SWRLRule <- subPropertyChainAxioms.toSet
      variables: Set[SWRLVariable] = rule.variables.toScala[Set]
      if 3 == variables.size

      heads: Set[SWRLAtom] = rule.head.toScala[Set]
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
      bodies: Set[SWRLAtom] = rule.body.toScala[Set]
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

                  val _allEntityDefinitionsIncludingImported = _allEntityDefinitions ++ importedEntityDefinitionMaps

                  val _allEntityReifiedRelationshipsIncludingImported = _entityReifiedRelationshipCMs ++ allImportedReifiedRelationships

                  val _allEntityRestrictions
                  : Set[(ModelEntityDefinition, ModelEntityReifiedRelationship, ModelEntityDefinition, Boolean, RestrictionKind)]
                  = {
                    val tuples =
                      for {
                        dPair <- _allEntityDefinitions
                        (d, domain) = dPair

                        // the restriction could be for an entity definition or a datatype
                        restriction <- getObjectPropertyRestrictionsIfAny(resolver.ont, d)
                        (_, op, isInverse, r, k) = restriction

                        // filter restrictions to an entity definition range only
                        range <- _allEntityDefinitionsIncludingImported.get(r)
                        relInfo <- _allEntityReifiedRelationshipsIncludingImported.find { case (relC, relRR) =>
                          relRR.unreified == op
                        }
                        (relC, relRR) = relInfo

                      } yield
                        Tuple5(domain, relRR, range, isInverse, k)
                    tuples.toSet
                  }

                  val inverseRestrictions
                  : Set[(ModelEntityDefinition, ModelEntityReifiedRelationship, ModelEntityDefinition, RestrictionKind)]
                  = _allEntityRestrictions.flatMap {
                    case (domain, rel, range, true, kind) =>
                      Some(Tuple4(domain, rel, range, kind))
                    case _ =>
                      None
                  }

                  val forwardRestrictions
                  : Set[(ModelEntityDefinition, ModelEntityReifiedRelationship, ModelEntityDefinition,  RestrictionKind)]
                  = _allEntityRestrictions.flatMap {
                    case (domain, rel, range, false, kind) =>
                      Some(Tuple4(domain, rel, range, kind))
                    case _ =>
                      None
                  }

                  val relRestrictions0
                  : RemainingAndMatchedRestrictions
                  = \/-(Tuple2(forwardRestrictions, Set.empty))

                  val relRestrictionsN
                  : RemainingAndMatchedRestrictions
                  = inverseRestrictions.foldLeft(relRestrictions0) { case (acc, (idomain, irel, irange, ikind)) =>
                      acc.flatMap { case (fremaining, restrictions) =>

                        val fi = fremaining.find { case (fdomain, frel, frange, fkind) =>
                          fdomain == irange && frel == irel && frange == idomain && fkind == ikind
                        }

                        fi match {
                          case None =>
                            val message = s"Ill-formed relationship restriction (missing forward tuple for inverse):\n" +
                              s"domain=$idomain\n" +
                              s"rel=$irel\n" +
                              s"range=$irange\n" +
                              s"kind=$ikind"
                            -\/(Set(OMFError.omfError(message)))
                          case Some(forward_inverse) =>
                            \/-(Tuple2( fremaining - forward_inverse, restrictions + forward_inverse))

                        }
                      }
                  }

                  val restrictionsAdded
                  : Set[java.lang.Throwable] \/ Unit
                  = relRestrictionsN.flatMap { case (forwardOnly, restrictions) =>

                    val ra
                    : Set[java.lang.Throwable] \/ Unit
                    = \/-(())

                    val rb
                    : Set[java.lang.Throwable] \/ Unit
                    = forwardOnly.foldLeft(ra) { case (acc, (domain, rel, range, kind)) =>
                      acc.flatMap { _ =>
                        kind match {
                          case ExistentialRestrictionKind =>
                            tboxG.addEntityDefinitionExistentialRestrictionAxiom(domain, rel, range).map { _ => () }
                          case UniversalRestrictionKind =>
                            tboxG.addEntityDefinitionUniversalRestrictionAxiom(domain, rel, range).map { _ => () }
                        }
                      }
                    }

                    val rc
                    : Set[java.lang.Throwable] \/ Unit
                    = restrictions.foldLeft(rb) { case (acc, (domain, rel, range, kind)) =>
                      acc.flatMap { _ =>
                        kind match {
                          case ExistentialRestrictionKind =>
                            tboxG.addEntityReifiedRelationshipExistentialRestrictionAxiom(domain, rel, range).map { _ => () }
                          case UniversalRestrictionKind =>
                            tboxG.addEntityReifiedRelationshipUniversalRestrictionAxiom(domain, rel, range).map { _ => () }
                        }
                      }
                    }

                    rc
                  }

                  restrictionsAdded.flatMap { _ =>

                    val _allScalarDefinitions: Map[OWLDatatype, ModelScalarDataType] =
                      importedScalarDatatypeDefinitionMaps ++ scalarDatatypeSCs

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
                                              s"Unresolved restricting data property $restrictingDP " +
                                                s"for entity $restrictedC with " +
                                                s"literal restriction $restrictingLiteral")))
                                          ) { restrictingSC =>
                                            \/-(Set((entityC, restrictingSC, restrictingLiteral)))
                                          }
                                        r
                                    }) {
                                    _ +++ _
                                  }

                                  r2
                                }
                              restrictions
                            }

                            (r0 /: restrictionTuples) {
                              _ +++ _
                            }
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
                              .map { case (restrictedC, restrictingE2SC, restrictingLiteral) =>
                                tboxG
                                  .addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(
                                    restrictedC, restrictingE2SC, restrictingLiteral)
                                  .map(Set(_))
                              }

                            val aN
                            : RestrictionAxiomValidation
                            = (a0 /: axioms) {
                              _ +++ _
                            }

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
}