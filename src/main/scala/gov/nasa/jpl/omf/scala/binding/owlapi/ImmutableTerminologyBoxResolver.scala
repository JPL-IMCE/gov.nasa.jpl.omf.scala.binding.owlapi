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
import gov.nasa.jpl.omf.scala.binding.owlapi.common.ImmutableModule
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.ImmutableTerminologyBox
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory

import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.util.{Failure, Success}
import scala.util.control.Exception._
import scala.{Boolean, Int, None, Option, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, String, require}
import scalaz._
import Scalaz._
import org.semanticweb.owlapi.reasoner.OWLReasoner

class ImmutableTerminologyBoxResolver(resolver: TerminologyBoxResolverHelper)
    extends ImmutableResolver {

  val LOG: Boolean = "true" equalsIgnoreCase
    java.lang.System.getProperty(
      "gov.nasa.jpl.omf.scala.binding.owlapi.log.ImmutableModuleResolver1")
  val LOG1: Boolean = "true" equalsIgnoreCase
    java.lang.System.getProperty(
      "gov.nasa.jpl.omf.scala.binding.owlapi.log.ImmutableModuleResolver")

  require(null != resolver)

  import resolver._
  import resolver.omfStore.ops._

  implicit val ont: OWLOntology = tboxG.ont

  private def getFacetValueIfAny(facets: List[OWLFacetRestriction],
                                 facetPrefixedName: String): Option[String] =
    facets
      .find { f =>
        facetPrefixedName == f.getFacet.getPrefixedName
      }
      .map(_.getFacetValue.getLiteral)

  private def getFacetIntValueIfAny(facets: List[OWLFacetRestriction],
                                    facetPrefixedName: String)
    : Throwables \/ Option[tables.taggedTypes.PositiveIntegerLiteral] =
    facets
      .find { f =>
        facetPrefixedName == f.getFacet.getPrefixedName
      }
      .map(_.getFacetValue.getLiteral)
      .fold[Throwables \/ Option[tables.taggedTypes.PositiveIntegerLiteral]](
        None.right) { v =>
        nonFatalCatch[
          Throwables \/ Option[tables.taggedTypes.PositiveIntegerLiteral]]
          .withApply { (cause: java.lang.Throwable) =>
            Set[java.lang.Throwable](cause).left
          }
          .apply(Some(tables.taggedTypes.positiveIntegerLiteral(v)).right)
      }

  private def resolveDataRanges(
      resolved: Map[OWLDatatype, DataRange],
      drs: List[OWLDatatype],
      queue: List[OWLDatatype] = List.empty,
      progress: Int = 0): Throwables \/ Map[OWLDatatype, DataRange] =
    drs match {
      case Nil =>
        queue match {
          case Nil =>
            resolved.right
          case _ =>
            if (0 == progress)
              Set[java.lang.Throwable](
                OMFError.omfError(s"resolveDataRanges: no progress")).left
            else
              resolveDataRanges(resolved, queue)
        }
      case dt :: dts =>
        import LiteralConversions.{toLiteralNumber, toLiteralDateTime}

        resolver.omfStore.getBuildInDatatypeMap
          .lookupBuiltInDataRange(dt.getIRI)(resolver.omfStore.ops) match {
          case Some(dr) =>
            dr match {
              case rdr: BinaryScalarRestriction =>
                tboxG.addBinaryScalarRestriction(dt, rdr)(resolver.omfStore)
              case rdr: IRIScalarRestriction =>
                tboxG.addIRIScalarRestriction(dt, rdr)(resolver.omfStore)
              case rdr: NumericScalarRestriction =>
                tboxG.addNumericScalarRestriction(dt, rdr)(resolver.omfStore)
              case rdr: PlainLiteralScalarRestriction =>
                tboxG.addPlainLiteralScalarRestriction(dt, rdr)(
                  resolver.omfStore)
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
                          s"resolveDataRanges: $dt has a definition as a DataOneOf without literals: $oneOf")).left
                      case ls =>
                        val lsCount = ls.size

                        val allDTs = ls.map(_.getDatatype).to[Set]
                        val dtCount = allDTs.size

                        if (dtCount > 1) {
                          System.out.println(
                            s"resolveDataRanges: $dt has a definition as a DataOneOf with $lsCount literals typed by $dtCount distinct datatypes")
                          allDTs.foreach { dt =>
                            System.out.println(s"- dt: $dt")
                          }
                        }
                        if (dtCount >= 1) {
                          val restrictedDT = allDTs.head
                          resolved.get(restrictedDT) match {
                            case Some(restrictedRange) =>
                              val added = for {
                                scalarOneOf <- tboxG
                                  .createScalarOneOfRestriction(
                                    tboxG.sig.uuid,
                                    dt,
                                    restrictedRange)(resolver.omfStore)
                                _ <- ls.foldLeft[types.UnitNES](
                                  types.rightUnitNES) {
                                  case (acc, li) =>
                                    for {
                                      _ <- acc
                                      v <- LiteralConversions.fromOWLLiteral(
                                        li,
                                        ont.getOWLOntologyManager.getOWLDataFactory) match {
                                        case Success(v) =>
                                          \/-(v)
                                        case Failure(t) =>
                                          -\/(Set(t))
                                      }
                                      _ <- tboxG.createScalarOneOfLiteralAxiom(
                                        scalarOneOf,
                                        v,
                                        Some(restrictedRange))
                                    } yield ()
                                }
                              } yield scalarOneOf
                              added match {
                                case \/-(sc) =>
                                  resolveDataRanges(resolved + (dt -> sc),
                                                    dts,
                                                    queue,
                                                    1 + progress)
                                case -\/(errors) =>
                                  errors.left
                              }
                            case None =>
                              resolveDataRanges(resolved,
                                                dts,
                                                dt :: queue,
                                                progress)
                          }
                        } else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"resolveDataRanges: $dt has a definition as a DataOneOf with some untyped literals: $oneOf")).left

                    }
                  case r: OWLDatatypeRestriction =>
                    resolved.get(r.getDatatype) match {
                      case Some(restrictedRange) =>
                        val facets = r.facetRestrictions().toScala[List]

                        val len: Throwables \/ Option[
                          tables.taggedTypes.PositiveIntegerLiteral] =
                          getFacetIntValueIfAny(facets, "xsd:length")
                        val minL: Throwables \/ Option[
                          tables.taggedTypes.PositiveIntegerLiteral] =
                          getFacetIntValueIfAny(facets, "xsd:minLength")
                        val maxL: Throwables \/ Option[
                          tables.taggedTypes.PositiveIntegerLiteral] =
                          getFacetIntValueIfAny(facets, "xsd:maxLength")
                        val patt: Option[tables.taggedTypes.LiteralPattern] =
                          getFacetValueIfAny(facets, "xsd:pattern").map(
                            tables.taggedTypes.literalPattern)
                        val minI
                          : Option[tables.taggedTypes.PositiveIntegerLiteral] =
                          getFacetValueIfAny(facets, "xsd:minInclusive").map(
                            tables.taggedTypes.positiveIntegerLiteral)
                        val maxI
                          : Option[tables.taggedTypes.PositiveIntegerLiteral] =
                          getFacetValueIfAny(facets, "xsd:maxInclusive").map(
                            tables.taggedTypes.positiveIntegerLiteral)
                        val minE
                          : Option[tables.taggedTypes.PositiveIntegerLiteral] =
                          getFacetValueIfAny(facets, "xsd:minExclusive").map(
                            tables.taggedTypes.positiveIntegerLiteral)
                        val maxE
                          : Option[tables.taggedTypes.PositiveIntegerLiteral] =
                          getFacetValueIfAny(facets, "xsd:maxExclusive").map(
                            tables.taggedTypes.positiveIntegerLiteral)
                        val lang
                          : Option[tables.taggedTypes.LanguageTagDataType] =
                          getFacetValueIfAny(facets, "rdf:LangRange").map(
                            tables.taggedTypes.languageTagDataType)

                        if (resolver.omfStore.isBinaryKind(restrictedRange)) {
                          val added = for {
                            l <- len
                            minl <- minL
                            maxl <- maxL
                            sc <- if (facets.size <= 3 && Seq(patt,
                                                              minI,
                                                              maxI,
                                                              minE,
                                                              maxE,
                                                              lang).forall(
                                        _.isEmpty))
                              tboxG.createBinaryScalarRestriction(
                                tboxG.uuid,
                                dt,
                                restrictedRange,
                                l,
                                minl,
                                maxl)(resolver.omfStore)
                            else
                              Set[java.lang.Throwable](OMFError.omfError(
                                s"resolveDataRanges: $dt ill-formed binary restriction per OWL2 section 4.5: $r")).left
                          } yield sc
                          added match {
                            case \/-(sc) =>
                              resolveDataRanges(resolved + (dt -> sc),
                                                dts,
                                                queue,
                                                1 + progress)
                            case -\/(errors) =>
                              -\/(errors)
                          }
                        } else if (resolver.omfStore.isIRIKind(restrictedRange)) {
                          val added = for {
                            l <- len
                            minl <- minL
                            maxl <- maxL
                            sc <- if (facets.size <= 4 && Seq(minI,
                                                              maxI,
                                                              minE,
                                                              maxE,
                                                              lang).forall(
                                        _.isEmpty))
                              tboxG.createIRIScalarRestriction(
                                tboxG.uuid,
                                dt,
                                restrictedRange,
                                l,
                                minl,
                                maxl,
                                patt)(resolver.omfStore)
                            else
                              Set[java.lang.Throwable](OMFError.omfError(
                                s"resolveDataRanges: $dt ill-formed IRI restriction per OWL2 section 4.6: $r")).left
                          } yield sc
                          added match {
                            case \/-(sc) =>
                              resolveDataRanges(resolved + (dt -> sc),
                                                dts,
                                                queue,
                                                1 + progress)
                            case -\/(errors) =>
                              -\/(errors)
                          }
                        } else if (resolver.omfStore.isNumericKind(
                                     restrictedRange)) {
                          val added = for {
                            l <- len
                            minl <- minL
                            maxl <- maxL
                            sc <- if (facets.size <= 4 && Seq(l,
                                                              minl,
                                                              maxl,
                                                              patt,
                                                              lang).forall(
                                        _.isEmpty))
                              tboxG.createNumericScalarRestriction(
                                tboxG.uuid,
                                dt,
                                restrictedRange,
                                minI,
                                maxI,
                                minE,
                                maxE)(resolver.omfStore)
                            else
                              Set[java.lang.Throwable](OMFError.omfError(
                                s"resolveDataRanges: $dt ill-formed numeric restriction per OWL2 sections 4.1, 4.2: $r")).left
                          } yield sc
                          added match {
                            case \/-(sc) =>
                              resolveDataRanges(resolved + (dt -> sc),
                                                dts,
                                                queue,
                                                1 + progress)
                            case -\/(errors) =>
                              -\/(errors)
                          }
                        } else if (resolver.omfStore.isPlainLiteralKind(
                                     restrictedRange)) {
                          val added = for {
                            l <- len
                            minl <- minL
                            maxl <- maxL
                            sc <- if (facets.size <= 5 && Seq(minI,
                                                              maxI,
                                                              minE,
                                                              maxE).forall(
                                        _.isEmpty))
                              tboxG.createPlainLiteralScalarRestriction(
                                tboxG.uuid,
                                dt,
                                restrictedRange,
                                l,
                                minl,
                                maxl,
                                patt,
                                lang)(resolver.omfStore)
                            else
                              Set[java.lang.Throwable](OMFError.omfError(
                                s"resolveDataRanges: $dt ill-formed PlainLiteral restriction per OWL2 section 4.3: $r")).left
                          } yield sc
                          added match {
                            case \/-(sc) =>
                              resolveDataRanges(resolved + (dt -> sc),
                                                dts,
                                                queue,
                                                1 + progress)
                            case -\/(errors) =>
                              -\/(errors)
                          }
                        } else if (resolver.omfStore.isStringKind(
                                     restrictedRange)) {
                          val added = for {
                            l <- len
                            minl <- minL
                            maxl <- maxL
                            sc <- if (facets.size <= 4 && Seq(minI,
                                                              maxI,
                                                              minE,
                                                              maxE,
                                                              lang).forall(
                                        _.isEmpty))
                              tboxG.createStringScalarRestriction(
                                tboxG.uuid,
                                dt,
                                restrictedRange,
                                l,
                                minl,
                                maxl,
                                patt)(resolver.omfStore)
                            else
                              Set[java.lang.Throwable](OMFError.omfError(
                                s"resolveDataRanges: $dt ill-formed String restriction per OWL2 section 4.3: $r")).left
                          } yield sc
                          added match {
                            case \/-(sc) =>
                              resolveDataRanges(resolved + (dt -> sc),
                                                dts,
                                                queue,
                                                1 + progress)
                            case -\/(errors) =>
                              -\/(errors)
                          }
                        } else if (resolver.omfStore.isTimeKind(
                                     restrictedRange)) {
                          val added = for {
                            l <- len
                            minl <- minL
                            maxl <- maxL
                            sc <- if (facets.size <= 4 && Seq(l,
                                                              minl,
                                                              maxl,
                                                              patt,
                                                              lang).forall(
                                        _.isEmpty))
                              tboxG.createTimeScalarRestriction(
                                tboxG.uuid,
                                dt,
                                restrictedRange,
                                minI,
                                maxI,
                                minE,
                                maxE)(resolver.omfStore)
                            else
                              Set[java.lang.Throwable](OMFError.omfError(
                                s"resolveDataRanges: $dt ill-formed time restriction per OWL2 section 4.7: $r")).left
                          } yield sc
                          added match {
                            case \/-(sc) =>
                              resolveDataRanges(resolved + (dt -> sc),
                                                dts,
                                                queue,
                                                1 + progress)
                            case -\/(errors) =>
                              -\/(errors)
                          }
                        } else if (resolver.omfStore.isAnyAtomicType(
                                     restrictedRange)) {
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"resolveDataRanges: $dt has a definition as a DatatypeRestriction on anyAtomicype: $r")).left
                        } else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"resolveDataRanges: $dt has a definition as a DatatypeRestriction outside of OWL2: $r")).left
                      case None =>
                        resolveDataRanges(resolved, dts, dt :: queue, progress)
                    }
                  case dr: OWLDatatype =>
                    resolved.get(dr) match {
                      case Some(restrictedRange) =>
                        tboxG.createSynonymScalarRestriction(
                          tboxG.uuid,
                          dt,
                          restrictedRange)(resolver.omfStore) match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc),
                                              dts,
                                              queue,
                                              1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      case None =>
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"resolveDataRanges: $dt has a definition as a DatatypeRestriction to an unrecognized datatype: $dr")).left
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
                      s"resolveDataRanges: $dt should have a definition as a DataOneOf or DataRestriction; got: $dtDef")).left
                }
              case _ =>
                val dtRanges: List[OWLDataRange] = dtDefs.map(_.getDataRange)
                val drRestrictions: List[OWLDatatypeRestriction] =
                  dtRanges.flatMap {
                    case r: OWLDatatypeRestriction =>
                      Some(r)
                    case _ =>
                      None
                  }
                val drRestrictedRanges: Set[OWLDatatype] =
                  drRestrictions.map(_.getDatatype).to[Set]
                if (drRestrictions.size == dtRanges.size && 1 == drRestrictedRanges.size) {
                  resolved.get(drRestrictedRanges.head) match {
                    case Some(restrictedRange) =>
                      val facets = drRestrictions.flatMap(
                        _.facetRestrictions().toScala[List])

                      val len: Throwables \/ Option[
                        tables.taggedTypes.PositiveIntegerLiteral] =
                        getFacetIntValueIfAny(facets, "xsd:length")
                      val minL: Throwables \/ Option[
                        tables.taggedTypes.PositiveIntegerLiteral] =
                        getFacetIntValueIfAny(facets, "xsd:minLength")
                      val maxL: Throwables \/ Option[
                        tables.taggedTypes.PositiveIntegerLiteral] =
                        getFacetIntValueIfAny(facets, "xsd:maxLength")
                      val patt: Option[tables.taggedTypes.LiteralPattern] =
                        getFacetValueIfAny(facets, "xsd:pattern").map(
                          tables.taggedTypes.literalPattern)
                      val minI
                        : Option[tables.taggedTypes.PositiveIntegerLiteral] =
                        getFacetValueIfAny(facets, "xsd:minInclusive").map(
                          tables.taggedTypes.positiveIntegerLiteral)
                      val maxI
                        : Option[tables.taggedTypes.PositiveIntegerLiteral] =
                        getFacetValueIfAny(facets, "xsd:maxInclusive").map(
                          tables.taggedTypes.positiveIntegerLiteral)
                      val minE
                        : Option[tables.taggedTypes.PositiveIntegerLiteral] =
                        getFacetValueIfAny(facets, "xsd:minExclusive").map(
                          tables.taggedTypes.positiveIntegerLiteral)
                      val maxE
                        : Option[tables.taggedTypes.PositiveIntegerLiteral] =
                        getFacetValueIfAny(facets, "xsd:maxExclusive").map(
                          tables.taggedTypes.positiveIntegerLiteral)
                      val lang: Option[tables.taggedTypes.LanguageTagDataType] =
                        getFacetValueIfAny(facets, "rdf:LangRange").map(
                          tables.taggedTypes.languageTagDataType)

                      if (resolver.omfStore.isBinaryKind(restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 3 && Seq(patt,
                                                            minI,
                                                            maxI,
                                                            minE,
                                                            maxE,
                                                            lang).forall(
                                      _.isEmpty))
                            tboxG.createBinaryScalarRestriction(
                              tboxG.uuid,
                              dt,
                              restrictedRange,
                              l,
                              minl,
                              maxl)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed binary restriction per OWL2 section 4.5: $dt")).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc),
                                              dts,
                                              queue,
                                              1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else if (resolver.omfStore.isIRIKind(restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 4 && Seq(minI,
                                                            maxI,
                                                            minE,
                                                            maxE,
                                                            lang).forall(
                                      _.isEmpty))
                            tboxG.createIRIScalarRestriction(
                              tboxG.uuid,
                              dt,
                              restrictedRange,
                              l,
                              minl,
                              maxl,
                              patt)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed IRI restriction per OWL2 section 4.6: $dt")).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc),
                                              dts,
                                              queue,
                                              1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else if (resolver.omfStore.isNumericKind(
                                   restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 4 && Seq(l,
                                                            minl,
                                                            maxl,
                                                            patt,
                                                            lang).forall(
                                      _.isEmpty))
                            tboxG.createNumericScalarRestriction(
                              tboxG.uuid,
                              dt,
                              restrictedRange,
                              minI,
                              maxI,
                              minE,
                              maxE)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed numeric restriction per OWL2 sections 4.1, 4.2: $dt")).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc),
                                              dts,
                                              queue,
                                              1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else if (resolver.omfStore.isPlainLiteralKind(
                                   restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 5 && Seq(minI,
                                                            maxI,
                                                            minE,
                                                            maxE).forall(
                                      _.isEmpty))
                            tboxG.createPlainLiteralScalarRestriction(
                              tboxG.uuid,
                              dt,
                              restrictedRange,
                              l,
                              minl,
                              maxl,
                              patt,
                              lang)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed PlainLiteral restriction per OWL2 section 4.3: $dt")).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc),
                                              dts,
                                              queue,
                                              1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else if (resolver.omfStore.isStringKind(
                                   restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 4 && Seq(minI,
                                                            maxI,
                                                            minE,
                                                            maxE,
                                                            lang).forall(
                                      _.isEmpty))
                            tboxG.createStringScalarRestriction(
                              tboxG.uuid,
                              dt,
                              restrictedRange,
                              l,
                              minl,
                              maxl,
                              patt)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed String restriction per OWL2 section 4.3: $dt")).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc),
                                              dts,
                                              queue,
                                              1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else if (resolver.omfStore.isTimeKind(restrictedRange)) {
                        val added = for {
                          l <- len
                          minl <- minL
                          maxl <- maxL
                          sc <- if (facets.size <= 4 && Seq(l,
                                                            minl,
                                                            maxl,
                                                            patt,
                                                            lang).forall(
                                      _.isEmpty))
                            tboxG.createTimeScalarRestriction(
                              tboxG.uuid,
                              dt,
                              restrictedRange,
                              minI,
                              maxI,
                              minE,
                              maxE)(resolver.omfStore)
                          else
                            Set[java.lang.Throwable](OMFError.omfError(
                              s"resolveDataRanges: $dt ill-formed time restriction per OWL2 section 4.7: $dt")).left
                        } yield sc
                        added match {
                          case \/-(sc) =>
                            resolveDataRanges(resolved + (dt -> sc),
                                              dts,
                                              queue,
                                              1 + progress)
                          case -\/(errors) =>
                            -\/(errors)
                        }
                      } else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"resolveDataRanges: $dt has a definition as a DatatypeRestriction outside of OWL2: $dt")).left
                    case None =>
                      resolveDataRanges(resolved, dts, dt :: queue, progress)
                  }
                } else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"resolveDataRanges: $dt should have a definition as a single DataOneOf or a homogeneous list of DataRestrictions")).left
            }
        }
    }

  def resolve(): Throwables \/ (ImmutableModule, OntologyMapping) = {
    val builtInDatatypeMap =
      resolver.omfStore.getBuildInDatatypeMap.dataRanges.map { dr =>
        dr.e -> dr
      }.toMap

    val importedScalarDatatypeDefinitionMaps: Map[OWLDatatype, DataRange] =
      resolver.importClosure
        .map(_.getScalarDatatypeDefinitionMap)
        .foldLeft(Map.empty[OWLDatatype, DataRange])(_ ++ _)

    val effectiveDatatypeMap = builtInDatatypeMap ++ importedScalarDatatypeDefinitionMaps

    val dTs = ont
      .datatypesInSignature(Imports.EXCLUDED)
      .toScala[List]
      .filter(ont.isDeclared)

    for {
      withDeclaredScalars <- dTs
        .filter { dt =>
          0 == ont.datatypeDefinitions(dt).count()
        }
        .foldLeft[Throwables \/ Map[OWLDatatype, DataRange]](
          effectiveDatatypeMap.right
        ) {
          case (acc, dt) =>
            for {
              m <- acc
              dr <- tboxG.createModelScalarDataType(tboxG.uuid, dt)
            } yield m + (dt -> dr)
        }

      dataRanges <- resolveDataRanges(withDeclaredScalars, dTs.filter { dt =>
        ont.datatypeDefinitions(dt).count() > 0
      })

      (bCs, tCs) = ont
        .classesInSignature(Imports.EXCLUDED)
        .toScala[Set]
        .filter(ont.isDeclared)
        .partition { c =>
          isBackboneIRI(c.getIRI)
        }

      (bOPs, tOPs) = ont
        .objectPropertiesInSignature(Imports.EXCLUDED)
        .toScala[Set]
        .filter(ont.isDeclared)
        .partition { c =>
          isBackboneIRI(c.getIRI)
        }

      (bDPs, tDPs) = ont
        .dataPropertiesInSignature(Imports.EXCLUDED)
        .toScala[Set]
        .filter(ont.isDeclared)
        .partition { c =>
          isBackboneIRI(c.getIRI)
        }

      b <- Backbone.resolveTerminologyBoxBackbone(ont,
                                                  bCs,
                                                  bOPs,
                                                  bDPs,
                                                  resolver.omfStore.ops,
                                                  resolver.ontOps)

      resolved <- b match {
        case backbone: OMFBackbone =>
          resolve(backbone,
                  dataRanges,
                  importedScalarDatatypeDefinitionMaps,
                  tCs,
                  tOPs,
                  tDPs)

        case _: NoBackbone =>
          asImmutableTerminologyBox(tboxG, om)
      }

    } yield resolved
  }

  type RemainingAndMatchedRestrictions =
    (Set[
       (Entity,
         RestrictableRelationship,
        OWLObjectProperty,
        Entity,
        ObjectRestrictionKind)],
     Set[
       (Entity,
         RestrictableRelationship,
        OWLObjectProperty,
        Entity,
        ObjectRestrictionKind)])

  type ResolverResult =
    Throwables \/ (ImmutableTerminologyBox, OntologyMapping)

  def resolve(backbone: OMFBackbone,
              dataRanges: Map[OWLDatatype, DataRange],
              importedScalarDatatypeDefinitionMaps: Map[OWLDatatype, DataRange],
              tCs: Set[OWLClass],
              tOPs: Set[OWLObjectProperty],
              tDPs: Set[OWLDataProperty]): ResolverResult = {
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
      System.out.println(
        s"importedScalarDatatypeDefinitionMaps: ${importedScalarDatatypeDefinitionMaps.size}")
    }

    val importedEntityDefinitionMaps: Map[OWLClass, Entity] =
      resolver.importClosure.flatMap(_.getEntityDefinitionMap).toMap

    val importedRestrictableRelationshipMaps: Map[OWLObjectProperty, RestrictableRelationship] =
      resolver.importClosure.flatMap(_.getRestrictableRelationshipMap).toMap

    val allImportedDataRelationshipsFromEntityToScalar
    : Map[OWLDataProperty, EntityScalarDataProperty] = resolver.importClosure
      .flatMap(_.getDataRelationshipsFromEntityToScalar)
      .map(sc => sc.e -> sc)
      .toMap

    val allImportedUnreifiedRelationships
    : Map[OWLObjectProperty, UnreifiedRelationship] = resolver.importClosure
      .flatMap(_.sig.unreifiedRelationships.map { ur =>
        ur.e -> ur
      })
      .toMap

    val reasonerFactory = new StructuralReasonerFactory()
    implicit val reasoner: OWLReasoner = reasonerFactory.createReasoner(ont)

    val things = reasoner.getSubClasses(backbone.ThingC, false)
    val aspectKinds = reasoner.getSubClasses(backbone.AspectKindC, false)
    val conceptKinds = reasoner.getSubClasses(backbone.ConceptKindC, false)
    val conceptualRelationships = reasoner.getSubClasses(backbone.ConceptualRelationshipKindC, false)
    val structuredDataProperties = reasoner.getSubClasses(backbone.ReifiedStructuredDataPropertyC, false)
    val structuredDatatypes = reasoner.getSubClasses(backbone.StructuredDatatypeC, false)

    //    val thingCIRIs
    //    : Map[IRI, OWLClass]
    //    = resolveThingCIRIs(things, tCs)

    val conceptKindCIRIs
    : Map[IRI, OWLClass]
    = resolveConceptCIRIs(conceptKinds, tCs)

    val (conceptCIRIs, cardinalityRestrictedConceptCIRIs)
    = conceptKindCIRIs.partition {
      case (_, conceptKindC) =>
        ont
          .subClassAxiomsForSubClass(conceptKindC)
          .toScala[Set]
          .flatMap { ax: OWLSubClassOfAxiom =>
            ax.getSuperClass match {
              case cr: OWLObjectCardinalityRestriction =>
                Some(cr)
              case _ =>
                None
            }
          }
          .isEmpty
    }

    val conceptualRelationshipKindCIRIs
    : Map[IRI, OWLClass]
    = resolveConceptRelationshipCIRIs(conceptualRelationships, tCs)

    val (reifiedRelationshipCIRIs, cardinalityRestrictedReifiedRelationshipCIRIs)
    = conceptualRelationshipKindCIRIs.partition {
      case (_, conceptualRelationshipKindC) =>
        ont
          .subClassAxiomsForSubClass(conceptualRelationshipKindC)
          .toScala[Set]
          .flatMap { ax: OWLSubClassOfAxiom =>
            ax.getSuperClass match {
              case cr: OWLObjectCardinalityRestriction =>
                Some(cr)
              case _ =>
                None
            }
          }
          .isEmpty
    }

    val reifiedStructuredDataPropertyCIRIs
    : Map[IRI, OWLClass]
    = resolveReifiedStructuredDataPropertyCIRIs(structuredDataProperties, tCs)

    val structuredDatatypeCIRIs
    : Map[IRI, OWLClass]
    = resolveStructuredDatatypeCIRIs(structuredDatatypes, tCs)

    val nonAspectCs =
      conceptKindCIRIs.values ++
        conceptualRelationshipKindCIRIs.values ++
        reifiedStructuredDataPropertyCIRIs.values ++
        structuredDatatypeCIRIs.values

    val ontIRIPrefix = resolver.getOntologyIRI.toString

    val aCs = (tCs -- nonAspectCs).filter { c =>
      c.getIRI.toString.startsWith(ontIRIPrefix)
    }

    val aspectKindCIRIs
    : Map[IRI, OWLClass]
    = resolveAspectKindCIRIs(aspectKinds, aCs)

    val (aspectCIRIs, cardinalityRestrictedAspectCIRIs)
    = aspectKindCIRIs.partition {
      case (_, aspectKindC) =>
        ont
          .subClassAxiomsForSubClass(aspectKindC)
          .toScala[Set]
          .flatMap { ax: OWLSubClassOfAxiom =>
            ax.getSuperClass match {
              case cr: OWLObjectCardinalityRestriction =>
                Some(cr)
              case _ =>
                None
            }
          }
          .isEmpty
    }

    val rules
    : Set[SWRLRule]
    = ont.logicalAxioms(Imports.EXCLUDED).toScala[Set].flatMap {
      case ax: SWRLRule =>
        Some(ax)
      case _ =>
        None
    }

    val (chains,
    recognizedRules,
    ignoredSourceOPs,
    ignoredTargetOPs,
    otherRules)
    : (TerminologyBoxResolverHelper.Chains,
      Set[SWRLRule],
      Set[OWLObjectProperty],
      Set[OWLObjectProperty],
      Set[SWRLRule])
    = rules.foldLeft[(TerminologyBoxResolverHelper.Chains,
      Set[SWRLRule],
      Set[OWLObjectProperty],
      Set[OWLObjectProperty],
      Set[SWRLRule])] {
      (Set.empty, Set.empty, Set.empty, Set.empty, Set.empty)
    } {
      case ((ci, ri, isi, iti, oi), s) =>
        val c: Option[TerminologyBoxResolverHelper.Chain] = for {
          rule <- Option(s)
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
            System.out.println(
              s"\nhead op: $head_op, v1: $head_v1, v2: $head_v2")
            System.out.println(
              s"body1 op: $body1_op, v1: $body1_v1, v2: $body1_v2")
            System.out.println(
              s"body2 op: $body2_op, v1: $body2_v1, v2: $body2_v2")
          }

          _ = require(
            (head_v1 == body1_v2 && head_v2 == body2_v2) || (head_v1 == body2_v2 && head_v2 == body1_v2))

          hasSource = if (head_v1 == body1_v2 && head_v2 == body2_v2) body1_op
          else body2_op
          hasTarget = if (head_v1 == body1_v2 && head_v2 == body2_v2) body2_op
          else body1_op
          _ = if (LOG1) {
            System.out.println(s"hasSource: $hasSource, hasTarget: $hasTarget")
          }
        } yield (head_op, hasSource, hasTarget)

        val (cj, rj, isj, itj, oj) = c match {
          case Some(_c) =>
            // https://github.com/JPL-IMCE/gov.nasa.jpl.imce.ontologies.public/issues/35
            // There may be multiple rules with the same head (_1) but different source/target (_2, _3)
            ci.find(_._1 == _c._1) match {
              case Some(ch) =>
                // There is already a triple, ch.
                if (s.annotations(ontOps.df.getRDFSLabel()).count() == 1L)
                // Prefer _c because it is a labelled rule.
                  (ci - ch + _c,
                    ri + s,
                    if (ch._2 == _c._2) isi else isi + ch._2,
                    if (ch._3 == _c._3) iti else iti + ch._3,
                    oi)
                else
                // Ignore _c because it is an unlabelled rule.
                  (ci,
                    ri + s,
                    if (ch._2 == _c._2) isi else isi + _c._2,
                    if (ch._3 == _c._3) iti else iti + _c._3,
                    oi)
              case None =>
                (ci + _c, ri + s, isi, iti, oi)
            }
          case None =>
            (ci, ri, isi, iti, oi + s)
        }

        val ni = ri.size + oi.size
        val nj = rj.size + oj.size
        if ((ni + 1) != nj)
          require((ni + 1) == nj)
        (cj, rj, isj, itj, oj)
    }

    require((recognizedRules.size + otherRules.size) == rules.size)

    val aspectCMs
    : Throwables \/ Map[OWLClass, AspectKind]
    = (Map[OWLClass, AspectKind]().right[Set[java.lang.Throwable]] /: aspectCIRIs) {
      case (acc, (aspectIRI, aspectC)) =>
        acc +++
          tboxG
            .createModelEntityAspect(tboxG.uuid, aspectC)
            .map { aspectM =>
              Map(aspectC -> aspectM)
            }
    }

    val conceptCMs
    : Throwables \/ Map[OWLClass, ConceptKind]
    = (Map[OWLClass, ConceptKind]().right[Set[java.lang.Throwable]] /: conceptCIRIs) {
      case (acc, (conceptIRI, conceptC)) =>
        acc +++
          tboxG
            .createModelEntityConcept(tboxG.uuid, conceptC)
            .map { conceptM =>
              Map(conceptC -> conceptM)
            }
    }

    val structuredDatatypeSCs
    : Throwables \/ Map[OWLClass, Structure]
    = (Map[OWLClass, Structure]().right[Set[java.lang.Throwable]] /: structuredDatatypeCIRIs) {
      case (acc, (structuredDatatypeIRI, structuredDatatypeC)) =>
        acc +++
          tboxG
            .createModelStructuredDataType(tboxG.uuid, structuredDatatypeC)
            .map { structuredDatatypeST =>
              Map(structuredDatatypeC -> structuredDatatypeST)
            }
    }

    val unreifiedObjectPropertyOPs
    : Set[(OWLClass, OWLObjectProperty, OWLClass)]
    = reasoner
      .getSubObjectProperties(backbone.topUnreifiedObjectPropertyOP, false)
      .entities()
      .toScala[Set]
      .flatMap {
        case op: OWLObjectProperty =>
          val ds = reasoner.getObjectPropertyDomains(op, true).entities().toScala[List]
          val rs = reasoner.getObjectPropertyRanges(op, true).entities().toScala[List]

          (ds, rs) match {
            case (d :: Nil, r :: Nil) =>
              Some((d, op, r))
            case _ =>
              // TODO: report error (this shouldn't happen for generated OWL)
              None
          }
        case _ =>
          // TODO: report error (this shouldn't happen for generated OWL)
          None
      }

    val topReifiedObjectPropertySubOPs
    = reasoner.getSubObjectProperties(backbone.topReifiedObjectPropertyOP,
      false)

    val reifiedObjectPropertyOPIRIs
    = resolveDomainRangeForObjectProperties(topReifiedObjectPropertySubOPs, tOPs)

    val topReifiedObjectPropertySourceSubOPs
    = reasoner.getSubObjectProperties(backbone.topReifiedObjectPropertySourceOP,
      false)

    val reifiedObjectPropertySourceOPIRIs
    = resolveDomainRangeForObjectProperties(
      topReifiedObjectPropertySourceSubOPs,
      tOPs,
      ignoredSourceOPs)

    val topReifiedObjectPropertyTargetSubOPs
    = reasoner.getSubObjectProperties(backbone.topReifiedObjectPropertyTargetOP,
      false)

    val reifiedObjectPropertyTargetOPIRIs
    = resolveDomainRangeForObjectProperties(
      topReifiedObjectPropertyTargetSubOPs,
      tOPs,
      ignoredTargetOPs)

    val dataPropertyDPIRIs
    = resolveDataPropertyDPIRIs(
      reasoner.getSubDataProperties(backbone.topDataPropertyDP, false),
      tDPs)

    val allEntityDefinitionsExceptRelationships
    = importedEntityDefinitionMaps.right[Set[java.lang.Throwable]] +++
      aspectCMs.map(_.toMap[OWLClass, Entity]) +++
      conceptCMs.map(_.toMap[OWLClass, Entity])

    val subClassAxioms
    = ont
      .axioms(AxiomType.SUBCLASS_OF, Imports.EXCLUDED)
      .toScala[Set]
      .filterNot { ax =>
        (TerminologyBoxResolverHelper.owlclassOfCE(Option.apply(ax.getSubClass)),
          TerminologyBoxResolverHelper.owlclassOfCE(Option.apply(ax.getSuperClass))) match {
          case (Some(sub), Some(sup)) =>
            backbone.isBackboneClass(sub) || backbone.isBackboneClass(sup)
          case (None, Some(sup)) =>
            backbone.isBackboneClass(sup)
          case (Some(sub), None) =>
            backbone.isBackboneClass(sub)
          case (None, None) =>
            require(false, s"Invalid complex subclass axiom: $ax")
            true
        }
      }.groupBy {
      ax => TerminologyBoxResolverHelper.owlclassOfCE(Option.apply(ax.getSubClass))
    }.flatMap {
      case (Some(c), axs) =>
        Some(c -> axs)
      case (None, _) =>
        None
    }

    val subObjectPropertyAxioms
    : Set[(OWLObjectProperty, OWLObjectProperty)]
    = ont
      .axioms(AxiomType.SUB_OBJECT_PROPERTY, Imports.EXCLUDED)
      .toScala[Set]
      .flatMap { ax =>
        (ax.getSubProperty, ax.getSuperProperty) match {
          case (op_sub: OWLObjectProperty, op_sup: OWLObjectProperty) =>
            if (!backbone.isBackboneObjectProperty(op_sub) && !backbone.isBackboneObjectProperty(op_sup))
              Some(op_sub -> op_sup)
            else
              None
          case _ =>
            None
        }
      }

    val subDataPropertyAxioms
    : Set[(OWLDataProperty, OWLDataProperty)]
    = ont
      .axioms(AxiomType.SUB_DATA_PROPERTY, Imports.EXCLUDED)
      .toScala[Set]
      .flatMap { ax =>
        (ax.getSubProperty, ax.getSuperProperty) match {
          case (dp_sub: OWLDataProperty, dp_sup: OWLDataProperty) =>
            if (!backbone.isBackboneDataProperty(dp_sub) && !backbone.isBackboneDataProperty(dp_sup))
              Some(dp_sub -> dp_sup)
            else
              None
          case _ =>
            None
        }
      }

    val result
    : ResolverResult
    = allEntityDefinitionsExceptRelationships.flatMap {
      _allEntityDefinitionsExceptRelationships =>
        val s0 = TerminologyBoxResolverHelper.IncrementalResolverState(
          _allEntityDefinitionsExceptRelationships,
          importedRestrictableRelationshipMaps,
          importedScalarDatatypeDefinitionMaps ++ dataRanges,
          allImportedDataRelationshipsFromEntityToScalar,
          reifiedRelationshipCIRIs,
          reifiedObjectPropertyOPIRIs,
          reifiedObjectPropertySourceOPIRIs,
          reifiedObjectPropertyTargetOPIRIs,
          chains,
          unreifiedObjectPropertyOPs,
          cardinalityRestrictedAspectCIRIs,
          cardinalityRestrictedConceptCIRIs,
          cardinalityRestrictedReifiedRelationshipCIRIs,
          subClassAxioms,
          subObjectPropertyAxioms,
          subDataPropertyAxioms,
          dataPropertyDPIRIs)

        val s1 = resolveEntityDefinitionsForRelationshipsRestrictionsAndSpecializations(\/-(s0))

        val s2 = otherRules.foldLeft(s1)(resolveImplicationRule)

        s2.flatMap { s =>

          val withRestrictions: Throwables \/ Unit
          = s.allEntities.foldLeft[Throwables \/ Unit](().right) {
            case (acc, (entityO, entityC)) =>
              val restrictions = types.getDataPropertyRestrictionsIfAny(ont, entityO)

              restrictions.foldLeft[Throwables \/ Unit](acc) {
                case (acc, (restrictedDP, kind)) =>
                  s.dataRelationshipsFromEntityToScalar
                    .get(restrictedDP)
                    .fold[Throwables \/ Unit](
                    -\/(Set(
                      OMFError
                        .omfError(
                          s"Unresolved restricting data property $restrictedDP " +
                            s"for entity $entityC with kind=$kind")))
                  ) { restrictingSC =>
                    kind match {
                      case ExistentialOWLDataRestrictionKind(rangeDT) =>
                        s.dataRanges
                          .get(rangeDT)
                          .fold[Throwables \/ Unit](
                          -\/(Set(OMFError.omfError(
                            s"Unresolved restricted data range $rangeDT " +
                              s"for entity $entityC and " +
                              s"restricting data property $restrictedDP with kind=$kind")))
                        ) {
                          rangeSC =>
                            acc +++
                              addEntityScalarDataPropertyExistentialRestrictionAxiom(
                                tboxG,
                                entityC,
                                restrictingSC,
                                rangeSC
                              ).map(_ => ())
                        }

                      case UniversalOWLDataRestrictionKind(rangeDT) =>
                        dataRanges
                          .get(rangeDT)
                          .fold[Throwables \/ Unit](
                          -\/(Set(OMFError.omfError(
                            s"Unresolved restricted data range $rangeDT " +
                              s"for entity $entityC and " +
                              s"restricting data property $restrictedDP with kind=$kind")))
                        ) {
                          rangeSC =>
                            acc +++
                              addEntityScalarDataPropertyUniversalRestrictionAxiom(
                                tboxG,
                                entityC,
                                restrictingSC,
                                rangeSC
                              ).map(_ => ())
                        }

                      case ParticularOWLDataRestrictionKind(value, Some(valueTypeDT)) =>
                        dataRanges
                          .get(valueTypeDT)
                          .fold[Throwables \/ Unit](
                          -\/(Set(OMFError.omfError(
                            s"Unresolved restricted data range $valueTypeDT " +
                              s"for entity $entityC and " +
                              s"restricting data property $restrictedDP with kind=$kind")))
                        ) {
                          valueType =>
                            acc +++
                              addEntityScalarDataPropertyParticularRestrictionAxiom(
                                tboxG,
                                entityC,
                                restrictingSC,
                                tables.LiteralValue(
                                  tables.LiteralStringType,
                                  value),
                                Some(valueType)
                              ).map(_ => ())
                        }

                      case ParticularOWLDataRestrictionKind(value, None) =>
                        acc +++
                          addEntityScalarDataPropertyParticularRestrictionAxiom(
                            tboxG,
                            entityC,
                            restrictingSC,
                            tables
                              .LiteralValue(
                                tables.LiteralStringType,
                                value),
                            None
                          ).map(_ => ())
                    }
                  }
              }
          }

          withRestrictions.flatMap { _ =>
            asImmutableTerminologyBox(tboxG, resolver.om)
          }
        }
    }

    result
  }

  sealed trait EntityScalarDataPropertyRestriction

  case class EntityScalarDataPropertyExistentialRestriction(
      entity: Entity,
      scalarDataProperty: EntityScalarDataProperty,
      range: DataRange)
      extends EntityScalarDataPropertyRestriction

  case class EntityScalarDataPropertyUniversalRestriction(
      entity: Entity,
      scalarDataProperty: EntityScalarDataProperty,
      range: DataRange)
      extends EntityScalarDataPropertyRestriction

  case class EntityScalarDataPropertyParticularRestriction(
      entity: Entity,
      scalarDataProperty: EntityScalarDataProperty,
      value: String)
      extends EntityScalarDataPropertyRestriction

}
