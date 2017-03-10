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

package gov.nasa.jpl.omf.scala.binding.owlapi.types.terms

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.LocalName
import org.semanticweb.owlapi.model.OWLDataProperty

import scala.{Any,Boolean,Int}
import scala.Predef.require

case class ScalarDataProperty
(override val e: OWLDataProperty,
 override val name: LocalName,
 override val uuid: UUID,
 override val domain: Structure,
 override val range: DataRange)
  extends DataRelationship
    with DataRelationshipFromStructure
    with DataRelationshipToScalar {

  require(null != domain)
  require(null != range)

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ScalarDataProperty => true
    case _ => false
  }

  override val hashCode
  : Int
  = (uuid, name, domain, range, e).##

  override def equals(other: Any): Boolean = other match {
    case that: ScalarDataProperty =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.name == that.name) &&
        (this.domain == that.domain) &&
        (this.range == that.range) &&
        (this.e == that.e)
    case _ =>
      false
  }
}