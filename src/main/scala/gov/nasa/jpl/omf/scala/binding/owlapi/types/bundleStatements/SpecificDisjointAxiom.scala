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

package gov.nasa.jpl.omf.scala.binding.owlapi.types.bundleStatements

import java.util.UUID

import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.Bundle
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms.Concept

import scala.{Any,Boolean,Int}
import scala.Predef.require

case class SpecificDisjointAxiom
(override val uuid: UUID,
 terminologyBundle: Bundle,
 override val disjointTaxonomyParent: ConceptTreeDisjunction,
 disjointLeaf: Concept)
 extends DisjointUnionOfConceptsAxiom {

  require(null != terminologyBundle)
  require(null != disjointTaxonomyParent)
  require(null != disjointLeaf)

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: SpecificDisjointAxiom => true
    case _ => false
  }

  override val hashCode: Int = (uuid, terminologyBundle, disjointTaxonomyParent, disjointLeaf).##

  override def equals(other: Any): Boolean = other match {
    case that: SpecificDisjointAxiom =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.terminologyBundle == that.terminologyBundle) &&
        (this.disjointTaxonomyParent == that.disjointTaxonomyParent) &&
        (this.disjointLeaf == that.disjointLeaf)
    case _ =>
      false
  }
}
