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

package gov.nasa.jpl.omf.scala.binding

import gov.nasa.jpl.omf.scala.core.OMFError
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model.{OWLOntologyChange, OWLOntologyManager}

import scala.collection.immutable.Set
import scala.{Option,None,StringContext,Unit}
import scala.Predef.String
import scalaz._, Scalaz._

package object owlapi {

  def catalogURIMapperException
  (message: String,
   cause: OMFError.Throwables = OMFError.emptyThrowables)
  : java.lang.Throwable
  = new CatalogURIMapperException(message, cause)

  def catalogURIMapperException
  (message: String,
   cause: java.lang.Throwable)
  : java.lang.Throwable
  = new CatalogURIMapperException(message, Set[java.lang.Throwable](cause))


  def applyOntologyChange
  (ontManager: OWLOntologyManager,
   ontChange: OWLOntologyChange,
   ifError: String,
   ifSuccess: Option[() => Unit] = None)
  : Set[java.lang.Throwable] \/ Unit
  = ontManager.applyChange(ontChange) match {
    case ChangeApplied.SUCCESSFULLY =>
      ifSuccess
      .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ){ callback =>
        \/.fromTryCatchNonFatal[Unit](callback())
        .fold[Set[java.lang.Throwable] \/ Unit](
        l = (t: java.lang.Throwable) =>
          -\/(
            Set(OMFError.omfBindingException(ifError, t))
          ),
        r = (_: Unit) =>
          \/-(())
        )
      }
    case ChangeApplied.NO_OPERATION =>
      Set(
        OMFError.omfBindingError(s"$ifError (no-operation change)")
      ).left
    case ChangeApplied.UNSUCCESSFULLY =>
      Set(
        OMFError.omfBindingError(s"$ifError (unsuccessful change)")
      ).left
  }

  def applyOntologyChangeOrNoOp
  (ontManager: OWLOntologyManager,
   ontChange: OWLOntologyChange,
   ifError: => String,
   ifSuccess: => Option[() => Unit] = None)
  : Set[java.lang.Throwable] \/ Unit
  = ontManager.applyChange(ontChange) match {
      case ChangeApplied.SUCCESSFULLY | ChangeApplied.NO_OPERATION =>
        ifSuccess
          .fold[Set[java.lang.Throwable] \/ Unit](
          \/-(())
        ){ callback =>
          \/.fromTryCatchNonFatal[Unit](callback())
            .fold[Set[java.lang.Throwable] \/ Unit](
            l = (t: java.lang.Throwable) =>
              -\/(
                Set(OMFError.omfBindingException(ifError, t))
              ),
            r = (_: Unit) =>
              \/-(())
          )
        }
      case ChangeApplied.UNSUCCESSFULLY =>
        Set(
          OMFError.omfBindingError(s"$ifError (unsuccessful change)")
        ).left
    }

}