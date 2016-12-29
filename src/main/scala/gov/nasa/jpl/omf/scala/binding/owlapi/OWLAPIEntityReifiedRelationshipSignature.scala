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

import java.util.UUID

import gov.nasa.jpl.imce.omf.schema.tables.LocalName
import gov.nasa.jpl.omf.scala.core.EntityReifiedRelationshipSignature
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics.RelationshipCharacteristics

import scala.collection.immutable.Iterable
import scala.{Boolean,Option}
import scala.Predef.require

case class OWLAPIEntityReifiedRelationshipSignature
(override val uuid: UUID,
 override val name: LocalName,
 override val unreifiedPropertyName: LocalName,
 override val unreifiedInversePropertyName: Option[LocalName],
 override val iri: OWLAPIOMF#IRI,
 override val source: OWLAPIOMF#Entity,
 override val target: OWLAPIOMF#Entity,
 override val characteristics: Iterable[RelationshipCharacteristics],
 override val isAbstract: Boolean )
  extends EntityReifiedRelationshipSignature[OWLAPIOMF]
{
  require(null != iri)
  require(null != source)
  require(null != target)
  require(null != characteristics)
}