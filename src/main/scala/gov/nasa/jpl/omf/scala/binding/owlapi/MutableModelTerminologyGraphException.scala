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
import gov.nasa.jpl.omf.scala.binding.owlapi.EntityExceptionKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.RelationshipScopeAccessKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms.TerminologyAxiom
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{Axiom, Term}
import gov.nasa.jpl.omf.scala.core.OMFError
import org.semanticweb.owlapi.model.IRI

import scala.StringContext
import scala.Predef.{String, augmentString, require}
import scala.collection.immutable.Map

sealed abstract class MutableModelTerminologyGraphException
(override val message: String)
  extends OMFError.OMFException(message, OMFError.emptyThrowables) {
  require(null != message)

  def this(kind: EntityExceptionKind, iri: IRI, message: String)
  = this(s"Cannot create $kind with IRI=$iri because "+message)

  def this(kind: AxiomExceptionKind, message: String)
  = this(s"Cannot create $kind because "+message)
}

case class EntityAlreadyDefinedException
(kind: EntityExceptionKind,
 iri: IRI,
 term: Term)
  extends MutableModelTerminologyGraphException(kind, iri, s"it is already defined as: $term") {

  require(null != kind)
  require(null != iri)
  require(null != term)
}

case class EntityConflictException
(kind: EntityExceptionKind,
 iri: IRI,
 conflictingTerm: Term)
  extends MutableModelTerminologyGraphException(kind, iri, s"this IRI refers to: $conflictingTerm") {

  require(null != kind)
  require(null != iri)
  require(null != conflictingTerm)
}

case class EntityScopeException
(kind: EntityExceptionKind,
 iri: IRI,
 unaccessibleTerms: Map[RelationshipScopeAccessKind, Term])
  extends MutableModelTerminologyGraphException(kind, iri,
    s"""|there are ${unaccessibleTerms.size} terms out of scope of
        |the graph: """.stripMargin +
      (unaccessibleTerms.map { case (k, t) => s"$k: $t" } mkString ", "))

case class AxiomScopeException
(kind: AxiomExceptionKind,
 unaccessibleTerms: Map[AxiomScopeAccessKind, Term])
  extends MutableModelTerminologyGraphException(kind,
    s"""|there are ${unaccessibleTerms.size} terms out of scope of
        |the graph: """.stripMargin +
      (unaccessibleTerms.map { case (k, t) => s"$k: $t" } mkString ", "))

case class DuplicateModelTermAxiomException
(kind: AxiomExceptionKind,
 axiom: Axiom)
  extends MutableModelTerminologyGraphException(kind, s"the axiom is already asserted $axiom")

case class DuplicateTerminologyGraphAxiomException
(kind: AxiomExceptionKind,
 axiom: TerminologyAxiom)
  extends MutableModelTerminologyGraphException(kind, s"the axiom is already asserted $axiom")