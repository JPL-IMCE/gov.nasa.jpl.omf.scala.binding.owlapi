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

package gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms

import java.util.UUID

import gov.nasa.jpl.omf.scala.binding.owlapi.types.Axiom
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms.Scalar
import gov.nasa.jpl.omf.scala.core._

import scala.collection.immutable._
import scala.{Any,Boolean,Int}
import scala.Predef.require

case class ScalarDataTypeFacetRestrictionAxiom
(override val uuid: UUID,
 sub: Scalar,
 sup: Scalar,
 fundamentalFacets: Iterable[FundamentalFacet],
 constrainingFacets: Iterable[ConstrainingFacet] )
  extends Axiom {

  require(null != sub)
  require(null != sup)
  require(null != fundamentalFacets)
  require(null != constrainingFacets)

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ScalarDataTypeFacetRestrictionAxiom => true
    case _ => false
  }

  override val hashCode: Int = (uuid, sub, sup, fundamentalFacets, constrainingFacets).##

  override def equals(other: Any): Boolean = other match {
    case that: ScalarDataTypeFacetRestrictionAxiom =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.sub == that.sub) &&
        (this.sup == that.sup) &&
        (this.fundamentalFacets == that.fundamentalFacets) &&
        (this.constrainingFacets == that.constrainingFacets)
    case _ =>
      false
  }
}