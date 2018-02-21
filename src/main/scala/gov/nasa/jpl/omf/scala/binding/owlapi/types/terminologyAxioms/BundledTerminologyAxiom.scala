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
import org.semanticweb.owlapi.model.IRI

import scala.{Any, Boolean, Int}
import scala.Predef.require

/**
  *
  * @param uuid
  * @param terminologyBundle source Bundle
  * @param targetModuleIRI target bundled TerminologyBox
  */
case class BundledTerminologyAxiom
(override val uuid: api.taggedTypes.BundledTerminologyAxiomUUID,
 terminologyBundle: api.taggedTypes.BundleUUID,
 override val targetModuleIRI: IRI)
extends TerminologyBundleAxiom {

  require( null != terminologyBundle )
  require( null != targetModuleIRI )

  override val sourceModule: api.taggedTypes.BundleUUID = terminologyBundle

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: BundledTerminologyAxiom => true
    case _ => false
  }

  override val hashCode: Int = (uuid, terminologyBundle, targetModuleIRI).##

  override def equals(other: Any): Boolean = other match {
    case that: BundledTerminologyAxiom =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.terminologyBundle == that.terminologyBundle) &&
        (this.targetModuleIRI == that.targetModuleIRI)
    case _ =>
      false
  }

}
