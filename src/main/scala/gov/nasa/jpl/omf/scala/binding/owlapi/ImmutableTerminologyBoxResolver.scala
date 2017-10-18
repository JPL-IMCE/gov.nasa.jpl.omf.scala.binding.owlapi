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

import java.lang.System

import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.ImmutableTerminologyBox
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory

import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.util.{Failure,Success}
import scala.util.control.Exception._
import scala.{Boolean, Int, None, Option, Some, StringContext, Tuple2, Tuple3, Tuple4, Tuple5, Unit}
import scala.Predef.{ArrowAssoc, String, require}
import scalaz._
import Scalaz._

case class ImmutableTerminologyBoxResolver(resolver: TerminologyBoxResolverHelper)
  extends ImmutableResolver {

  val LOG: Boolean = "true" equalsIgnoreCase
    java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ImmutableModuleResolver1")
  val LOG1: Boolean = "true" equalsIgnoreCase
    java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ImmutableModuleResolver")

  require(null != resolver)

  import resolver._
  import resolver.omfStore.ops._

  val ont = tboxG.ont

  private def getFacetValueIfAny(facets: List[OWLFacetRestriction], facetPrefixedName: String)
  : Option[String]
  = facets.find { f => facetPrefixedName == f.getFacet.getPrefixedName }.map(_.getFacetValue.getLiteral)

  private def getFacetIntValueIfAny(facets: List[OWLFacetRestriction], facetPrefixedName: String)
  : Throwables \/ Option[tables.PositiveIntegerLiteral]
  = facets
    .find { f => facetPrefixedName == f.getFacet.getPrefixedName }
    .map(_.getFacetValue.getLiteral)
    .fold[Throwables \/ Option[tables.PositiveIntegerLiteral]](None.right) { v =>
    nonFatalCatch[Throwables \/ Option[tables.PositiveIntegerLiteral]]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set[java.lang.Throwable](cause).left
      }
      .apply(Some(v).right)
  }

  private def resolveDataRanges
  (resolved: Map[OWLDatatype, DataRange],
   drs: List[OWLDatatype],
   queue: List[OWLDatatype] = List.empty,
   progress: Int = 0)
  : Throwables \/ Map[OWLDatatype, DataRange]
  = drs match {
    case Nil =>
      queue match {
        case Nil =>
          resolved.right
        case _ =>
          if (0 == progress)
            Set[java.lang.Throwable](OMFError.omfError(
              s"resolveDataRanges: no progress")
            ).left
          else
            resolveDataRanges(resolved, queue)
      }
    case dt :: dts =>
      import LiteralConversions.{toLiteralNumber,toLiteralDateTime}

      resolver.omfStore.getBuildInDatatypeMap.lookupBuiltInDataRange(dt.getIRI)(resolver.omfStore.ops) match {
        case Some(dr) =>
          dr match {
            case rdr: BinaryScalarRestriction =>
              tboxG.addBinaryScalarRestriction(dt, rdr)(resolver.omfStore)
            case rdr: IRIScalarRestriction =>
              tboxG.addIRIScalarRestriction(dt, rdr)(resolver.omfStore)
            case rdr: NumericScalarRestriction =>
              tboxG.addNumericScalarRestriction(dt, rdr)(resolver.omfStore)
            case rdr: PlainLiteralScalarRestriction =>
              tboxG.addPlainLiteralScalarRestriction(dt, rdr)(resolver.omfStore)
            case rdr: StringScalarRestriction =>
              tboxG.addStringScalarRestriction(dt, rdr)(resolver.omfStore)
            case rdr: SynonymScalarRestriction =>
              tboxG.addSynonymScalarRestriction(dt, rdr)(resolver.omfStore)
            case rdr: TimeScalarRestriction =>
              tboxG.addTimeScalarRestriction(dt, rdr)(resolver.omfStore)
          }
          resolveDataRanges(resolved + (dt -> dr), dts, queue, 1 + progress)
        case None =>
          val dtDefs = ont.datatypeDefinitions(dt).toScala[List]
          dtDefs match {
            case dtDef :: Nil =>
              dtDef.getDataRange match {
                case oneOf: OWLDataOneOf =>
                  oneOf.values().toScala[List] match {
                    case Nil =>
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"resolveDataRanges: $dt has a definition as a DataOneOf without literals: $oneOf")
                      ).left
                    case ls =>
                      val lsCount = ls.size

                      val allDTs = ls.map(_.getDatatype).to[Set]
                      val dtCount = allDTs.size

                      if (dtCount > 1) {
                        System.out.println(s"resolveDataRanges: $dt has a definition as a DataOneOf with $lsCount literals typed by $dtCount distinct datatypes")
                        allDTs.foreach { dt =>
                          System.out.println(s"- dt: $dt")
                        }
                      }
                      if (dtCount >= 1) {
                        val restrictedDT = allDTs.head
                        resolved.get(restrictedDT) match {
                          case Some(restrictedRange) =>
                            val added = for {
                              scalarOneOf <- tboxG.createScalarOneOfRestriction(tboxG.sig.uuid, dt, restrictedRange)(resolver.omfStore)
                              _ <- ls.foldLeft[types.UnitNES](types.rightUnitNES) { case (acc, li) =>
                                for {
                                  _ <- acc
                                  v <- LiteralConversions.fromOWLLiteral(li, ont.getOWLOntologyManager.getOWLDataFactory) match {
                                    case Success(v) =>
                                      \/-(v)
                                    case Failure(t) =>
                                      -\/(Set(t))
                                  }
                                  _ <- tboxG.createScalarOneOfLiteralAxiom(scalarOneOf, v, Some(restrictedRange))
                                } yield ()
                              }
                            } yield scalarOneOf
                            added match {
                              case \/-(sc) =>
                                resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                              case -\/(errors) =>
                                errors.left
                            }
                          case None =>
                            resolveDataRanges(resolved, dts, dt :: queue, progress)
                        }
                      } else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"resolveDataRanges: $dt has a definition as a DataOneOf with some untyped literals: $oneOf")
                        ).left

                  }
                case r: OWLDatatypeRestriction =>
                  resolved.get(r.getDatatype) match {
                    case Some(restrictedRange) =>

                      val facets = r.facetRestrictions().toScala[List]

                      val len: Throwables \/ Option[tables.PositiveIntegerLiteral] = getFacetIntValueIfAny(facets, "xsd:length")
                      val minL: Throwables \/ Option[tables.PositiveIntegerLiteral] = getFacetIntValueIfAny(facets, "xsd:minLength")
                      val maxL: Throwables \/ Option[tables.PositiveIntegerLiteral] = getFacetIntValueIfAny(facets, "xsd:maxLength")
                      val patt: Option[String] = getFacetValueIfAny(facets, "xsd:pattern")
                      val minI: Option[String] = getFacetValueIfAny(facets, "xsd:minInclusive")
                      val maxI: Option[String] = getFacetValueIfAny(facets, "xsd:maxInclusive")
                      val minE: Option[String] = getFacetValueIfAny(facets, "xsd:minExclusive")
                      val maxE: Option[String] = getFacetValueIfAny(facets, "xsd:maxExclusive")
                      val lang: Option[String] = getFacetValueIfAny(facets, "rdf:LangRange")

                      if (resolver.omfStore.isBinaryKind(restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 3 && Seq(patt, minI, maxI, minE, maxE, lang).forall(_.isEmpty))
                            tboxG.createBinaryScalarRestriction(tboxG.uuid, dt, restrictedRange, l, minl, maxl)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed binary restriction per OWL2 section 4.5: $r")
                            ).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else if (resolver.omfStore.isIRIKind(restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 4 && Seq(minI, maxI, minE, maxE, lang).forall(_.isEmpty))
                            tboxG.createIRIScalarRestriction(tboxG.uuid, dt, restrictedRange, l, minl, maxl, patt)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed IRI restriction per OWL2 section 4.6: $r")
                            ).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else if (resolver.omfStore.isNumericKind(restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 4 && Seq(l, minl, maxl, patt, lang).forall(_.isEmpty))
                            tboxG.createNumericScalarRestriction(tboxG.uuid, dt, restrictedRange,
                              minI, maxI, minE, maxE)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed numeric restriction per OWL2 sections 4.1, 4.2: $r")
                            ).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else if (resolver.omfStore.isPlainLiteralKind(restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 5 && Seq(minI, maxI, minE, maxE).forall(_.isEmpty))
                            tboxG.createPlainLiteralScalarRestriction(tboxG.uuid, dt, restrictedRange, l,
                              minl, maxl, patt, lang)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed PlainLiteral restriction per OWL2 section 4.3: $r")
                            ).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else if (resolver.omfStore.isStringKind(restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 4 && Seq(minI, maxI, minE, maxE, lang).forall(_.isEmpty))
                            tboxG.createStringScalarRestriction(tboxG.uuid, dt, restrictedRange, l, minl, maxl, patt)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed String restriction per OWL2 section 4.3: $r")
                            ).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else if (resolver.omfStore.isTimeKind(restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 4 && Seq(l, minl, maxl, patt, lang).forall(_.isEmpty))
                            tboxG.createTimeScalarRestriction(
                              tboxG.uuid, dt, restrictedRange,
                              minI, maxI, minE, maxE)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed time restriction per OWL2 section 4.7: $r")
                            ).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else if (resolver.omfStore.isAnyAtomicType(restrictedRange)) {
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"resolveDataRanges: $dt has a definition as a DatatypeRestriction on anyAtomicype: $r")
                        ).left
                      } else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"resolveDataRanges: $dt has a definition as a DatatypeRestriction outside of OWL2: $r")
                        ).left
                    case None =>
                      resolveDataRanges(resolved, dts, dt :: queue, progress)
                  }
                case dr: OWLDatatype =>
                  resolved.get(dr) match {
                    case Some(restrictedRange) =>
                      tboxG.createSynonymScalarRestriction(tboxG.uuid, dt, restrictedRange)(resolver.omfStore) match {
                        case \/-(sc) =>
                          resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                        case -\/(errors) =>
                          -\/(errors)
                      }
                    case None =>
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"resolveDataRanges: $dt has a definition as a DatatypeRestriction to an unrecognized datatype: $dr")
                      ).left
                  }
                case _ =>
                  val eqDTs = ont.axioms(dt).toScala[List]
                  eqDTs match {
                    case eqDT :: Nil =>
                      System.out.println(s"Found $eqDT for $dt")
                    case Nil =>
                      System.out.println(s"No eq for $dt")
                    case _ =>
                      System.out.println(s"Found ${eqDTs.size} eq for $dt")
                  }
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"resolveDataRanges: $dt should have a definition as a DataOneOf or DataRestriction; got: $dtDef")
                  ).left
              }
            case _ =>
              val dtRanges: List[OWLDataRange] = dtDefs.map(_.getDataRange)
              val drRestrictions: List[OWLDatatypeRestriction] = dtRanges.flatMap {
                case r: OWLDatatypeRestriction =>
                  Some(r)
                case _ =>
                  None
              }
              val drRestrictedRanges: Set[OWLDatatype] = drRestrictions.map(_.getDatatype).to[Set]
              if (drRestrictions.size == dtRanges.size && 1 == drRestrictedRanges.size) {
                resolved.get(drRestrictedRanges.head) match {
                  case Some(restrictedRange) =>
                    val facets = drRestrictions.flatMap(_.facetRestrictions().toScala[List])

                    val len: Throwables \/ Option[tables.PositiveIntegerLiteral] = getFacetIntValueIfAny(facets, "xsd:length")
                    val minL: Throwables \/ Option[tables.PositiveIntegerLiteral] = getFacetIntValueIfAny(facets, "xsd:minLength")
                    val maxL: Throwables \/ Option[tables.PositiveIntegerLiteral] = getFacetIntValueIfAny(facets, "xsd:maxLength")
                    val patt: Option[String] = getFacetValueIfAny(facets, "xsd:pattern")
                    val minI: Option[String] = getFacetValueIfAny(facets, "xsd:minInclusive")
                    val maxI: Option[String] = getFacetValueIfAny(facets, "xsd:maxInclusive")
                    val minE: Option[String] = getFacetValueIfAny(facets, "xsd:minExclusive")
                    val maxE: Option[String] = getFacetValueIfAny(facets, "xsd:maxExclusive")
                    val lang: Option[String] = getFacetValueIfAny(facets, "rdf:LangRange")

                    if (resolver.omfStore.isBinaryKind(restrictedRange)) {
                      val added = for {
                        l <- len
                        minl <- minL
                        maxl <- maxL
                        sc <- if (facets.size <= 3 && Seq(patt, minI, maxI, minE, maxE, lang).forall(_.isEmpty))
                          tboxG.createBinaryScalarRestriction(tboxG.uuid, dt, restrictedRange, l, minl, maxl)(resolver.omfStore)
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"resolveDataRanges: $dt ill-formed binary restriction per OWL2 section 4.5: $dt")
                          ).left
                      } yield sc
                      added match {
                        case \/-(sc) =>
                          resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                        case -\/(errors) =>
                          -\/(errors)
                      }
                    } else if (resolver.omfStore.isIRIKind(restrictedRange)) {
                      val added = for {
                        l <- len
                        minl <- minL
                        maxl <- maxL
                        sc <- if (facets.size <= 4 && Seq(minI, maxI, minE, maxE, lang).forall(_.isEmpty))
                          tboxG.createIRIScalarRestriction(tboxG.uuid, dt, restrictedRange, l, minl, maxl, patt)(resolver.omfStore)
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"resolveDataRanges: $dt ill-formed IRI restriction per OWL2 section 4.6: $dt")
                          ).left
                      } yield sc
                      added match {
                        case \/-(sc) =>
                          resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                        case -\/(errors) =>
                          -\/(errors)
                      }
                    } else if (resolver.omfStore.isNumericKind(restrictedRange)) {
                      val added = for {
                        l <- len
                        minl <- minL
                        maxl <- maxL
                        sc <- if (facets.size <= 4 && Seq(l, minl, maxl, patt, lang).forall(_.isEmpty))
                          tboxG.createNumericScalarRestriction(tboxG.uuid, dt, restrictedRange, minI, maxI, minE, maxE)(resolver.omfStore)
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"resolveDataRanges: $dt ill-formed numeric restriction per OWL2 sections 4.1, 4.2: $dt")
                          ).left
                      } yield sc
                      added match {
                        case \/-(sc) =>
                          resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                        case -\/(errors) =>
                          -\/(errors)
                      }
                    } else if (resolver.omfStore.isPlainLiteralKind(restrictedRange)) {
                      val added = for {
                        l <- len
                        minl <- minL
                        maxl <- maxL
                        sc <- if (facets.size <= 5 && Seq(minI, maxI, minE, maxE).forall(_.isEmpty))
                          tboxG.createPlainLiteralScalarRestriction(tboxG.uuid, dt, restrictedRange, l, minl, maxl, patt, lang)(resolver.omfStore)
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"resolveDataRanges: $dt ill-formed PlainLiteral restriction per OWL2 section 4.3: $dt")
                          ).left
                      } yield sc
                      added match {
                        case \/-(sc) =>
                          resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                        case -\/(errors) =>
                          -\/(errors)
                      }
                    } else if (resolver.omfStore.isStringKind(restrictedRange)) {
                      val added = for {
                        l <- len
                        minl <- minL
                        maxl <- maxL
                        sc <- if (facets.size <= 4 && Seq(minI, maxI, minE, maxE, lang).forall(_.isEmpty))
                          tboxG.createStringScalarRestriction(tboxG.uuid, dt, restrictedRange, l, minl, maxl, patt)(resolver.omfStore)
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"resolveDataRanges: $dt ill-formed String restriction per OWL2 section 4.3: $dt")
                          ).left
                      } yield sc
                      added match {
                        case \/-(sc) =>
                          resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                        case -\/(errors) =>
                          -\/(errors)
                      }
                    } else if (resolver.omfStore.isTimeKind(restrictedRange)) {
                      val added = for {
                        l <- len
                        minl <- minL
                        maxl <- maxL
                        sc <- if (facets.size <= 4 && Seq(l, minl, maxl, patt, lang).forall(_.isEmpty))
                          tboxG.createTimeScalarRestriction(tboxG.uuid, dt, restrictedRange,
                            minI, maxI, minE, maxE)(resolver.omfStore)
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"resolveDataRanges: $dt ill-formed time restriction per OWL2 section 4.7: $dt")
                          ).left
                      } yield sc
                      added match {
                        case \/-(sc) =>
                          resolveDataRanges(resolved + (dt -> sc), dts, queue, 1 + progress)
                        case -\/(errors) =>
                          -\/(errors)
                      }
                    } else
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"resolveDataRanges: $dt has a definition as a DatatypeRestriction outside of OWL2: $dt")
                      ).left
                  case None =>
                    resolveDataRanges(resolved, dts, dt :: queue, progress)
                }
              } else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"resolveDataRanges: $dt should have a definition as a single DataOneOf or a homogeneous list of DataRestrictions")
                ).left
          }
      }
  }

  def resolve()
  : Throwables \/ (ImmutableTerminologyBox, OntologyMapping)
  = {
    val builtInDatatypeMap = resolver.omfStore.getBuildInDatatypeMap.dataRanges.map { dr => dr.e -> dr }.toMap

    val importedScalarDatatypeDefinitionMaps: Map[OWLDatatype, DataRange]
    = resolver
      .importClosure
      .map(_.getScalarDatatypeDefinitionMap)
      .foldLeft(Map.empty[OWLDatatype, DataRange])(_ ++ _)

    val effectiveDatatypeMap = builtInDatatypeMap ++ importedScalarDatatypeDefinitionMaps

    val dTs = ont.datatypesInSignature(Imports.EXCLUDED).toScala[List].filter(ont.isDeclared)

    for {
      withDeclaredScalars <-
      dTs
        .filter { dt => 0 == ont.datatypeDefinitions(dt).count() }
        .foldLeft[Throwables \/ Map[OWLDatatype, DataRange]](
        effectiveDatatypeMap.right
      ) { case (acc, dt) =>
        for {
          m <- acc
          dr <- tboxG.createModelScalarDataType(tboxG.uuid, dt)
          _ <- resolver.omfStore.registerOMFModelScalarDataTypeInstance(tboxG, dr)
        } yield m + (dt -> dr)
      }

      dataRanges <-
      resolveDataRanges(
        withDeclaredScalars,
        dTs.filter { dt => ont.datatypeDefinitions(dt).count() > 0 })

      (bCs, tCs) =
      ont
        .classesInSignature(Imports.EXCLUDED)
        .toScala[Set]
        .filter(ont.isDeclared)
        .partition { c => isBackboneIRI(c.getIRI) }

      (bOPs, tOPs) =
      ont
        .objectPropertiesInSignature(Imports.EXCLUDED)
        .toScala[Set]
        .filter(ont.isDeclared)
        .partition { c => isBackboneIRI(c.getIRI) }

      (bDPs, tDPs) =
      ont
        .dataPropertiesInSignature(Imports.EXCLUDED)
        .toScala[Set]
        .filter(ont.isDeclared)
        .partition { c => isBackboneIRI(c.getIRI) }

      b <- Backbone.resolveTerminologyBoxBackbone(ont, bCs, bOPs, bDPs, resolver.omfStore.ops, resolver.ontOps)

      resolved <- b match {
        case backbone: OMFBackbone =>
          resolve(backbone, dataRanges, importedScalarDatatypeDefinitionMaps, tCs, tOPs, tDPs)

        case _: NoBackbone =>
          asImmutableTerminologyBox(tboxG, om.m2i)
      }

      (i, m2i) = resolved

      result = i -> OntologyMapping.initialize(m2i, om.drc)
    } yield result
  }

  type RemainingAndMatchedRestrictions =
    Throwables \/
      ( Set[(Entity, ReifiedRelationship, Entity,  ObjectRestrictionKind)],
        Set[(Entity, ReifiedRelationship, Entity,  ObjectRestrictionKind)] )

  type ResolverResult =
    Throwables \/ (ImmutableTerminologyBox, Mutable2ImmutableModuleMap)

  def resolve
  (backbone: OMFBackbone,
   dataRanges: Map[OWLDatatype, DataRange],
   importedScalarDatatypeDefinitionMaps: Map[OWLDatatype, DataRange],
   tCs: Set[OWLClass],
   tOPs: Set[OWLObjectProperty],
   tDPs: Set[OWLDataProperty])
  : ResolverResult
  = {
    implicit val _backbone = backbone

    if (LOG) {
      System.out.println(s"import closure: ${importClosure.size}")
      System.out.println(
        importClosure
          .map(_.ont.getOntologyID.toString)
          .toList
          .sorted
          .mkString("\n => imports: ", "\n => imports: ", "\n"))
    }


    if (LOG) {
      System.out.println(s"importedScalarDatatypeDefinitionMaps: ${importedScalarDatatypeDefinitionMaps.size}")
    }

    val importedEntityDefinitionMaps: Map[OWLClass, Entity]
    = resolver.importClosure.flatMap(_.getEntityDefinitionMap).toMap

    val allImportedDataRelationshipsFromEntityToScalar
    = resolver.importClosure.flatMap(_.getDataRelationshipsFromEntityToScalar)

    val allImportedReifiedRelationships: Map[OWLClass, ReifiedRelationship]
    = importedEntityDefinitionMaps flatMap {
      case (rrC, rrE: ReifiedRelationship) =>
        Some(rrC -> rrE)
      case _ =>
        None
    }

    val allImportedUnreifiedRelationships: Map[OWLObjectProperty, UnreifiedRelationship]
    = resolver.importClosure.flatMap(_.sig.unreifiedRelationships.map { ur => ur.e -> ur }).toMap

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

    val subPropertyChainAxioms: Set[SWRLRule] = ont.logicalAxioms(Imports.EXCLUDED).toScala[Set].flatMap {
      case ax: SWRLRule =>
        Some(ax)
      case _ =>
        None
    }

    val (chains, otherRules)
    : (Chains, Set[SWRLRule])
    = subPropertyChainAxioms.foldLeft[(Chains, Set[SWRLRule])](Set.empty, Set.empty) {
      case ((ci, ri), r) =>

        val c: Option[Chain] = for {
          rule <- Option(r)
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

        c match {
          case Some(cj) =>
            (ci + cj) -> ri
          case None =>
            ci -> (ri + r)
        }
    }

    System.out.println(s"#-------------------")
    System.out.println(s"# Rules: ${chains.size} ROP SWRL rule chains and ${otherRules.size} inference rules.")
    System.out.println(s"#-------------------")

    val aspectCMs: Throwables \/ Map[OWLClass, Aspect] =
      (Map[OWLClass, Aspect]().right[Set[java.lang.Throwable]] /: aspectCIRIs) {
        case (acc, (aspectIRI, aspectC)) =>
          acc +++
            tboxG
              .createModelEntityAspect(tboxG.uuid, aspectC)
              .flatMap { aspectM =>
                resolver.omfStore.registerOMFModelEntityAspectInstance(tboxG, aspectM)
                  .map { _ =>
                    Map(aspectC -> aspectM)
                  }
              }
      }

    val importedAspectDefinitions: Map[OWLClass, Aspect] = importedEntityDefinitionMaps flatMap {
      case (aC, aE: Aspect) =>
        Some(aC -> aE)
      case _ =>
        None
    }

    val allAspectsIncludingImported = aspectCMs +++ importedAspectDefinitions.right

    val conceptCMs: Throwables \/ Map[OWLClass, Concept] =
      (Map[OWLClass, Concept]().right[Set[java.lang.Throwable]] /: conceptCIRIs) {
        case (acc, (conceptIRI, conceptC)) =>
          acc +++
            tboxG
              .createModelEntityConcept(tboxG.uuid, conceptC)
              .flatMap { conceptM =>
                resolver.omfStore.registerOMFModelEntityConceptInstance(tboxG, conceptM)
                  .map { _ =>
                    Map(conceptC -> conceptM)
                  }
              }
      }

    val importedConceptDefinitions: Map[OWLClass, Concept] = importedEntityDefinitionMaps flatMap {
      case (cC, cE: Concept) =>
        Some(cC -> cE)
      case _ =>
        None
    }

    val allConceptsIncludingImported = conceptCMs +++ importedConceptDefinitions.right

    val structuredDatatypeSCs: Throwables \/ Map[OWLClass, Structure] =
      (Map[OWLClass, Structure]().right[Set[java.lang.Throwable]] /: structuredDatatypeCIRIs) {
        case (acc, (structuredDatatypeIRI, structuredDatatypeC)) =>
          acc +++
            tboxG
              .createModelStructuredDataType(tboxG.uuid, structuredDatatypeC)
              .flatMap { structuredDatatypeST =>
                resolver.omfStore.registerOMFModelStructuredDataTypeInstance(tboxG, structuredDatatypeST)
                  .map(_ => Map(structuredDatatypeC -> structuredDatatypeST))
              }
      }

    val topUnreifiedObjectPropertySubOPs =
      reasoner
        .getSubObjectProperties(backbone.topUnreifiedObjectPropertyOP, false)

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
        aspectCMs.map(_.toMap[OWLClass, Entity]) +++
        conceptCMs.map(_.toMap[OWLClass, Entity])

    allEntityDefinitionsExceptRelationships.flatMap { _allEntityDefinitionsExceptRelationships =>
      allAspectsIncludingImported.flatMap { _allAspectsIncludingImported =>
        allConceptsIncludingImported.flatMap { _allConceptsIncludingImported =>
          aspectCMs.flatMap { _aspectCMs =>
            conceptCMs.flatMap { _conceptCMs =>
              resolveConceptSubClassAxioms(_allConceptsIncludingImported).flatMap { _ =>
                resolveEntityDefinitionsForRelationships(
                  _allEntityDefinitionsExceptRelationships,
                  reifiedObjectPropertyCIRIs,
                  reifiedObjectPropertyOPIRIs,
                  reifiedObjectPropertySourceOPIRIs,
                  reifiedObjectPropertyTargetOPIRIs,
                  chains,
                  Map()
                ).flatMap { _entityReifiedRelationshipCMs =>

                  val _allEntityDefinitions: Map[OWLClass, Entity] =
                    Map[OWLClass, Entity]() ++
                      _aspectCMs ++
                      _conceptCMs ++
                      _entityReifiedRelationshipCMs

                  val _allEntityReifiedRelationshipsIncludingImported =
                    _entityReifiedRelationshipCMs ++ allImportedReifiedRelationships

                  val _allObjectPropertiesOfEntityReifiedRelationshipsIncludingImported
                  : Map[OWLObjectProperty, ReifiedRelationship] =
                    _allEntityReifiedRelationshipsIncludingImported.flatMap { case (_, rr) =>
                      Map(rr.unreified -> rr, rr.rSource -> rr, rr.rTarget -> rr) ++
                        rr.inverse.map { inv => inv -> rr }
                    }

                  val _allEntityDefinitionsIncludingImported =
                    _allEntityDefinitions ++ importedEntityDefinitionMaps ++ _allEntityReifiedRelationshipsIncludingImported

                  (resolveDefinitionAspectSubClassAxioms(_allEntityDefinitionsIncludingImported, _allAspectsIncludingImported) +++
                    resolveReifiedRelationshipSubClassAxioms(_allEntityReifiedRelationshipsIncludingImported)
                    ).flatMap { _ =>

                    resolveUnreifiedRelationships(
                      topUnreifiedObjectPropertySubOPs,
                      _allEntityDefinitionsIncludingImported
                    ).flatMap { resolvedUnreifiedRelationships: Map[OWLObjectProperty, UnreifiedRelationship] =>

                      val _allUnreifiedRelationships
                      : Map[OWLObjectProperty, UnreifiedRelationship]
                      = resolvedUnreifiedRelationships ++ allImportedUnreifiedRelationships

                      val rulesAdded
                      : Throwables \/ Unit
                      = otherRules.foldLeft[Throwables \/ Unit](\/-(())) { case (acc, r) =>
                        for {
                          _ <- acc
                          _ <- resolveImplicationRule(
                            \/-(r),
                            _allUnreifiedRelationships,
                            _allObjectPropertiesOfEntityReifiedRelationshipsIncludingImported,
                            _allEntityDefinitionsIncludingImported)
                        } yield ()
                      }

                      rulesAdded.flatMap { _ =>

                        val _allEntityObjectRestrictions
                        : Set[(Entity, ReifiedRelationship, Entity, Boolean, ObjectRestrictionKind)]
                        = {
                          val tuples =
                            for {
                              dPair <- _allEntityDefinitions
                              (d, domain) = dPair

                              // the restriction could be for an entity definition or a structured datatype
                              restriction <- types.getObjectPropertyRestrictionsIfAny(resolver.tboxG.ont, d)
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
                        : Set[(Entity, ReifiedRelationship, Entity, ObjectRestrictionKind)]
                        = _allEntityObjectRestrictions.flatMap {
                          case (domain, rel, range, true, kind) =>
                            Some(Tuple4(domain, rel, range, kind))
                          case _ =>
                            None
                        }

                        val forwardRestrictions
                        : Set[(Entity, ReifiedRelationship, Entity, ObjectRestrictionKind)]
                        = _allEntityObjectRestrictions.flatMap {
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
                                \/-(Tuple2(fremaining - forward_inverse, restrictions + forward_inverse))

                            }
                          }
                        }

                        val restrictionsAdded
                        : Throwables \/ Unit
                        = relRestrictionsN.flatMap { case (forwardOnly, restrictions) =>

                          val ra
                          : Throwables \/ Unit
                          = \/-(())

                          val rb
                          : Throwables \/ Unit
                          = forwardOnly.foldLeft(ra) { case (acc, (domain, rel, range, kind)) =>
                            acc.flatMap { _ =>
                              kind match {
                                case ExistentialObjectRestrictionKind =>
                                  tboxG.addEntityDefinitionExistentialRestrictionAxiom(domain, rel, range).map { _ => () }
                                case UniversalObjectRestrictionKind =>
                                  tboxG.addEntityDefinitionUniversalRestrictionAxiom(domain, rel, range).map { _ => () }
                              }
                            }
                          }

                          rb
                        }

                        restrictionsAdded.flatMap { _ =>

                          val _allScalarDefinitions: Map[OWLDatatype, DataRange] =
                            importedScalarDatatypeDefinitionMaps ++ dataRanges

                          resolveDataRelationshipsFromEntity2Scalars(_allEntityDefinitions, dataPropertyDPIRIs, _allScalarDefinitions)
                            .flatMap { dataRelationshipsFromEntity2Scalar =>

                              val allDataRelationshipsFromEntityToScalar =
                                dataRelationshipsFromEntity2Scalar ++ allImportedDataRelationshipsFromEntityToScalar

                              type RestrictionInfoValidation
                              = Throwables \/ Unit

                              val withRestrictions
                              : Throwables \/ Unit
                              = _allEntityDefinitions.foldLeft[Throwables \/ Unit](
                                ().right
                              ) { case (acc, (entityO, entityC)) =>

                                val restrictions
                                = types.getDataPropertyRestrictionsIfAny(resolver.tboxG.ont, entityO)

                                restrictions.foldLeft[RestrictionInfoValidation](acc) {
                                  case (acc, (restrictedDP, kind)) =>
                                    allDataRelationshipsFromEntityToScalar
                                      .find {
                                        _.e == restrictedDP
                                      }
                                      .fold[RestrictionInfoValidation](
                                      -\/(Set(OMFError.omfError(
                                        s"Unresolved restricting data property $restrictedDP " +
                                          s"for entity $entityC with kind=$kind")))
                                    ) { restrictingSC =>

                                      kind match {
                                        case ExistentialOWLDataRestrictionKind(rangeDT) =>
                                          dataRanges
                                            .get(rangeDT)
                                            .fold[RestrictionInfoValidation](
                                            -\/(Set(OMFError.omfError(
                                              s"Unresolved restricted data range $rangeDT " +
                                                s"for entity $entityC and " +
                                                s"restricting data property $restrictedDP with kind=$kind")))
                                          ) { rangeSC =>
                                            acc +++
                                              addEntityScalarDataPropertyExistentialRestrictionAxiom(
                                                tboxG, entityC, restrictingSC, rangeSC
                                              ).map(_ => ())
                                          }

                                        case UniversalOWLDataRestrictionKind(rangeDT) =>
                                          dataRanges
                                            .get(rangeDT)
                                            .fold[RestrictionInfoValidation](
                                            -\/(Set(OMFError.omfError(
                                              s"Unresolved restricted data range $rangeDT " +
                                                s"for entity $entityC and " +
                                                s"restricting data property $restrictedDP with kind=$kind")))
                                          ) { rangeSC =>
                                            acc +++
                                              addEntityScalarDataPropertyUniversalRestrictionAxiom(
                                                tboxG, entityC, restrictingSC, rangeSC
                                              ).map(_ => ())
                                          }

                                        case ParticularOWLDataRestrictionKind(value, Some(valueTypeDT)) =>
                                          dataRanges
                                            .get(valueTypeDT)
                                            .fold[RestrictionInfoValidation](
                                            -\/(Set(OMFError.omfError(
                                              s"Unresolved restricted data range $valueTypeDT " +
                                                s"for entity $entityC and " +
                                                s"restricting data property $restrictedDP with kind=$kind")))
                                          ) { valueType =>
                                            acc +++
                                              addEntityScalarDataPropertyParticularRestrictionAxiom(
                                                tboxG, entityC, restrictingSC,
                                                tables.LiteralValue(tables.LiteralStringType, value),
                                                Some(valueType)
                                              ).map(_ => ())
                                          }

                                        case ParticularOWLDataRestrictionKind(value, None) =>
                                          acc +++
                                            addEntityScalarDataPropertyParticularRestrictionAxiom(
                                              tboxG, entityC, restrictingSC,
                                              tables.LiteralValue(tables.LiteralStringType, value),
                                              None
                                            ).map(_ => ())
                                      }
                                    }
                                }
                              }

                              withRestrictions.flatMap { _ =>
                                asImmutableTerminologyBox(tboxG, resolver.om.m2i)
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

  sealed trait EntityScalarDataPropertyRestriction

  case class EntityScalarDataPropertyExistentialRestriction
  (entity: Entity, scalarDataProperty: EntityScalarDataProperty, range: DataRange)
    extends EntityScalarDataPropertyRestriction

  case class EntityScalarDataPropertyUniversalRestriction
  (entity: Entity, scalarDataProperty: EntityScalarDataProperty, range: DataRange)
    extends EntityScalarDataPropertyRestriction

  case class EntityScalarDataPropertyParticularRestriction
  (entity: Entity, scalarDataProperty: EntityScalarDataProperty, value: String)
    extends EntityScalarDataPropertyRestriction

}