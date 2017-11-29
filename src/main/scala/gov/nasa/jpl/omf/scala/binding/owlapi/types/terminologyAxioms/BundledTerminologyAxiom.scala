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

package gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms

import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.omf.scala.binding.owlapi.common.Module
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.TerminologyBox

import scala.{Any, Boolean, Int}
import scala.Predef.require

case class BundledTerminologyAxiom
(override val uuid: api.taggedTypes.BundledTerminologyAxiomUUID,
 terminologyBundle: api.taggedTypes.BundleUUID,
 bundledTerminology: TerminologyBox)
extends TerminologyBundleAxiom {

  require( null != terminologyBundle )
  require( null != bundledTerminology )

  override val sourceModule: api.taggedTypes.BundleUUID = terminologyBundle

  override val targetModule: Module = bundledTerminology

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: BundledTerminologyAxiom => true
    case _ => false
  }

  override val hashCode: Int = (uuid, terminologyBundle, bundledTerminology).##

  override def equals(other: Any): Boolean = other match {
    case that: BundledTerminologyAxiom =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.terminologyBundle == that.terminologyBundle) &&
        (this.bundledTerminology == that.bundledTerminology)
    case _ =>
      false
  }

}
