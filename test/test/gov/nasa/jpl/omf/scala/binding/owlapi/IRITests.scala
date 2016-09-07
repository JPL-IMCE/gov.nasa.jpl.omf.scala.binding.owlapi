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

package test.gov.nasa.jpl.omf.scala.binding.owlapi

import org.apache.xml.resolver.CatalogManager

import gov.nasa.jpl.omf.scala.binding.owlapi._

import test.gov.nasa.jpl.omf.scala.core.{ functionalAPI => testFunctionalAPI }

import scala.collection.immutable.Set
import scala.StringContext

class IRIBasicTests 
extends testFunctionalAPI.IRITests[OWLAPIOMF]()({
  val om = OWLAPIOMFModule.owlAPIOMFModule(new CatalogManager())
    .valueOr { (errors: Set[java.lang.Throwable]) =>
      val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
      throw new scala.IllegalArgumentException(message)
    }
  om.ops
})