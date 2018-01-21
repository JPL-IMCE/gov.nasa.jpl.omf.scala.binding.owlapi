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

import gov.nasa.jpl.omf.scala.binding.owlapi.AxiomExceptionKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.AxiomScopeAccessKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.ElementExceptionKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.RelationshipScopeAccessKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.common.Resource
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms.TerminologyAxiom
import gov.nasa.jpl.omf.scala.binding.owlapi.types.Axiom
import gov.nasa.jpl.omf.scala.core.OMFError
import org.semanticweb.owlapi.model.IRI

import scala.StringContext
import scala.Predef.{String, augmentString, require}
import scala.collection.immutable.Map

sealed abstract class MutableTerminologyBoxException
(override val message: String)
  extends OMFError.OMFException(message, OMFError.emptyThrowables) {
  require(null != message)

  def this(kind: ElementExceptionKind, iri: IRI, message: String)
  = this(s"Cannot create $kind with IRI=$iri because "+message)

  def this(kind: AxiomExceptionKind, message: String)
  = this(s"Cannot create $kind because "+message)
}

case class ResouceAlreadyDefinedException
(kind: ElementExceptionKind,
 iri: IRI,
 resource: Resource)
  extends MutableTerminologyBoxException(kind, iri, s"it is already defined as: $resource") {

  require(null != kind)
  require(null != iri)
  require(null != resource)
}

case class ResourceConflictException
(kind: ElementExceptionKind,
 iri: IRI,
 conflictingResource: Resource)
  extends MutableTerminologyBoxException(kind, iri, s"this IRI refers to: $conflictingResource") {

  require(null != kind)
  require(null != iri)
  require(null != conflictingResource)
}

case class ResourceScopeException
(kind: ElementExceptionKind,
 iri: IRI,
 unaccessibleResources: Map[RelationshipScopeAccessKind, Resource])
  extends MutableTerminologyBoxException(kind, iri,
    s"""|there are ${unaccessibleResources.size} resources out of scope of
        |the graph: """.stripMargin +
      (unaccessibleResources.map { case (k, t) => s"$k: $t" } mkString ", "))

case class AxiomScopeException
(kind: AxiomExceptionKind,
 unaccessibleResources: Map[AxiomScopeAccessKind, Resource])
  extends MutableTerminologyBoxException(kind,
    s"""|there are ${unaccessibleResources.size} resources out of scope of
        |the graph: """.stripMargin +
      (unaccessibleResources.map { case (k, t) => s"$k: $t" } mkString ", "))

case class DuplicateTermAxiomException
(kind: AxiomExceptionKind,
 axiom: Axiom)
  extends MutableTerminologyBoxException(kind, s"the axiom is already asserted $axiom")

case class DuplicateTerminologyBoxAxiomException
(kind: AxiomExceptionKind,
 axiom: TerminologyAxiom)
  extends MutableTerminologyBoxException(kind, s"the axiom is already asserted $axiom")
