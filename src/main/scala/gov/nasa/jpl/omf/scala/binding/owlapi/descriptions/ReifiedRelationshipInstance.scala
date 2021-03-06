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

package gov.nasa.jpl.omf.scala.binding.owlapi.descriptions

import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms.ConceptualRelationship
import org.semanticweb.owlapi.model.OWLNamedIndividual

case class ReifiedRelationshipInstance
(override val iri: OWLAPIOMF#IRI,
 override val uuid: api.taggedTypes.ReifiedRelationshipInstanceUUID,
 override val name: LocalName,
 override val ni: OWLNamedIndividual,
 relationshipType: ConceptualRelationship)
  extends ConceptualEntitySingletonInstance