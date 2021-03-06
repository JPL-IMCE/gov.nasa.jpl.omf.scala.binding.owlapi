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

import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.omf.scala.binding.owlapi.types.TerminologyBundleStatement
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.Bundle
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms.Concept

import scala.{Any,Boolean,Int}
import scala.Predef.require

case class RootConceptTaxonomyAxiom
(override val uuid: api.taggedTypes.RootConceptTaxonomyAxiomUUID,
 terminologyBundle: Bundle,
 root: Concept)
  extends TerminologyBundleStatement with ConceptTreeDisjunction {

  require(null != terminologyBundle)
  require(null != root)

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: RootConceptTaxonomyAxiom => true
    case _ => false
  }

  override val hashCode: Int = (uuid, terminologyBundle, root).##

  override def equals(other: Any): Boolean = other match {
    case that: RootConceptTaxonomyAxiom =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.terminologyBundle == that.terminologyBundle) &&
        (this.root == that.root)
    case _ =>
      false
  }
}
