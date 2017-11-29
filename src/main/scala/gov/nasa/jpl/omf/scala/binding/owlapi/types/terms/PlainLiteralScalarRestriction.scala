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

import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName
import gov.nasa.jpl.imce.oml.tables
import org.semanticweb.owlapi.model.{IRI, OWLDatatype}

import scala.{Any, Boolean, Int, Option}

case class PlainLiteralScalarRestriction
(override val e: OWLDatatype,
 override val iri: IRI,
 override val uuid: api.taggedTypes.PlainLiteralScalarRestrictionUUID,
 override val name: LocalName,
 override val restrictedDataRange: DataRange,
 length: Option[tables.taggedTypes.PositiveIntegerLiteral],
 minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
 maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
 pattern: Option[tables.taggedTypes.LiteralPattern],
 language: Option[tables.taggedTypes.LanguageTagDataType])
  extends RestrictedDataRange {

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: PlainLiteralScalarRestriction => true
    case _ => false
  }

  override val hashCode
  : Int
  = (uuid, name, e).##

  override def equals(other: Any): Boolean = other match {
    case that: PlainLiteralScalarRestriction =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.name == that.name) &&
        (this.restrictedDataRange == that.restrictedDataRange) &&
        (this.e == that.e) &&
        (this.length == that.length) &&
        (this.minLength == that.minLength) &&
        (this.maxLength == that.maxLength) &&
        (this.pattern == that.pattern) &&
        (this.language == that.language)
    case _ =>
      false
  }
}


