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
import gov.nasa.jpl.omf.scala.binding.owlapi.ElementExceptionKind._
import gov.nasa.jpl.omf.scala.core.OMFError
import org.semanticweb.owlapi.model.IRI

import scala.StringContext
import scala.Predef.{String, require}

sealed abstract class MutableDescriptionBoxException
(override val message: String)
extends OMFError.OMFException(message, OMFError.emptyThrowables) {

  require(null != message)

  def this(kind: ElementExceptionKind, iri: IRI, message: String)
  = this(s"Cannot create $kind with IRI=$iri because "+message)

  def this(kind: AxiomExceptionKind, message: String)
  = this(s"Cannot create $kind because "+message)
}

case class ConceptInstanceAlreadyDefinedException
(kind: ElementExceptionKind,
 iri: IRI,
 ci: descriptions.ConceptInstance)
extends MutableDescriptionBoxException(kind, iri, s"it is already defined as: $ci") {
  require (null != kind)
  require (null != iri)
  require (null != ci)
}

case class ReifiedRelationshipInstanceAlreadyDefinedException
(kind: ElementExceptionKind,
 iri: IRI,
 rri: descriptions.ReifiedRelationshipInstance)
  extends MutableDescriptionBoxException(kind, iri, s"it is already defined as: $rri") {
  require (null != kind)
  require (null != iri)
  require (null != rri)
}

case class ReifiedRelationshipInstanceDomainAlreadyDefinedException
(kind: AxiomExceptionKind,
 rri: descriptions.ReifiedRelationshipInstanceDomain)
  extends MutableDescriptionBoxException(kind, s"it is already defined as: $rri") {
  require (null != kind)
  require (null != rri)
}

case class ReifiedRelationshipInstanceRangeAlreadyDefinedException
(kind: AxiomExceptionKind,
 rri: descriptions.ReifiedRelationshipInstanceRange)
  extends MutableDescriptionBoxException(kind, s"it is already defined as: $rri") {
  require (null != kind)
  require (null != rri)
}

case class UnreifiedRelationshipInstanceTupleAlreadyDefinedException
(kind: AxiomExceptionKind,
 rri: descriptions.UnreifiedRelationshipInstanceTuple)
  extends MutableDescriptionBoxException(kind, s"it is already defined as: $rri") {
  require (null != kind)
  require (null != rri)
}

case class SingletonInstanceScalarDataPropertyValueAlreadyDefinedException
(kind: AxiomExceptionKind,
 axv: descriptions.SingletonInstanceScalarDataPropertyValue)
  extends MutableDescriptionBoxException(kind, s"it is already defined as: $axv") {
  require (null != kind)
  require (null != axv)
}

case class SingletonInstanceStructuredDataPropertyValueAlreadyDefinedException
(kind: AxiomExceptionKind,
 axv: descriptions.SingletonInstanceStructuredDataPropertyValue)
  extends MutableDescriptionBoxException(kind, s"it is already defined as: $axv") {
  require (null != kind)
  require (null != axv)
}

case class ScalarDataPropertyValueAlreadyDefinedException
(kind: AxiomExceptionKind,
 axv: descriptions.ScalarDataPropertyValue)
  extends MutableDescriptionBoxException(kind, s"it is already defined as: $axv") {
  require (null != kind)
  require (null != axv)
}

case class StructuredDataPropertyTupleAlreadyDefinedException
(kind: AxiomExceptionKind,
 axv: descriptions.StructuredDataPropertyTuple)
  extends MutableDescriptionBoxException(kind, s"it is already defined as: $axv") {
  require (null != kind)
  require (null != axv)
}

case class InstanceConflictException
(kind: ElementExceptionKind,
 iri: IRI,
 conflictingInstance: descriptions.ConceptualEntitySingletonInstance)
  extends MutableDescriptionBoxException(kind, iri, s"this IRI refers to: $conflictingInstance") {

  require(null != kind)
  require(null != iri)
  require(null != conflictingInstance)
}