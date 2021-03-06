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

import org.semanticweb.owlapi.model.OWLDatatype

import scala.{Option,StringContext}
import scala.Predef.String

trait OWLDataRestrictionKind

case class ExistentialOWLDataRestrictionKind
(range: OWLDatatype)
  extends OWLDataRestrictionKind {

  override def toString: String = s"Existential data property restriction for some values from $range"
}

case class UniversalOWLDataRestrictionKind
(range: OWLDatatype)
  extends OWLDataRestrictionKind {

  override def toString: String = s"Universal data property restriction for all values from $range"
}

case class ParticularOWLDataRestrictionKind
(literal: String,
 valueType: Option[OWLDatatype])
  extends OWLDataRestrictionKind {

  override def toString: String = s"Particular data property restriction with value $literal, valueType=${valueType.map(_.getIRI)}"
}
