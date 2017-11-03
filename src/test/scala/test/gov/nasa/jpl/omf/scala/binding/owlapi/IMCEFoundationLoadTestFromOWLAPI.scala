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

import org.semanticweb.owlapi.apibinding.OWLManager
import gov.nasa.jpl.omf.scala.binding.owlapi._
import test.gov.nasa.jpl.omf.scala.core.{functionalAPI => testFunctionalAPI}
import org.apache.xml.resolver.CatalogManager

import scala.Predef._
import scala.collection.immutable.Set
import scala.{Option, StringContext, Unit, transient}
import java.lang.IllegalArgumentException

import org.apache.xml.resolver.tools.CatalogResolver

abstract class IMCEFoundationLoadTestFromOWLAPI
( override val loadStore: OWLAPIOMFGraphStore )
  extends testFunctionalAPI.IMCEFoundationLoadTest[OWLAPIOMF](
      loadStore, loadStore.omfModule.ops )

abstract class IMCEFoundationLoadTestFromOWLAPICatalog( @transient val catalogManager: CatalogManager )
  extends IMCEFoundationLoadTestFromOWLAPI(
      loadStore = OWLAPIOMFGraphStore.initGraphStore(
        OWLAPIOMFModule.owlAPIOMFModule(catalogManager).valueOr { (errors: Set[java.lang.Throwable]) =>
          val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
          throw new scala.IllegalArgumentException(message)
        },
        OWLManager.createOWLOntologyManager(),
        new CatalogResolver(catalogManager),
        catalogManager.getCatalog))

class IMCEFoundationLoadTestFromOWLAPILocalCatalog
  extends IMCEFoundationLoadTestFromOWLAPICatalog( catalogManager = new CatalogManager() ) {

  val catalogFile = "/ontologies/oml.catalog.xml"

  Option.apply(classOf[IMCEFoundationLoadTestFromOWLAPILocalCatalog].getResource(catalogFile))
    .fold[Unit]({
    Option.apply(java.nio.file.Paths.get("ontologies", "imce.local.catalog.xml"))
      .fold[Unit]({
      throw new IllegalArgumentException(s"There should be a '$catalogFile' resource on the classpath")
    }) { p =>
      if (p.toFile.exists() && p.toFile.canRead)
        loadStore.catalogIRIMapper.parseCatalog(p.toFile.toURI)
          .valueOr { (errors: Set[java.lang.Throwable]) =>
            val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
            throw new scala.IllegalArgumentException(message)
          }
      else
        throw new IllegalArgumentException(s"There should be a '$catalogFile' resource on the classpath")
    }
  }){ testCatalogURL =>
    loadStore.catalogIRIMapper.parseCatalog(testCatalogURL.toURI)
      .valueOr { (errors: Set[java.lang.Throwable]) =>
        val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
        throw new scala.IllegalArgumentException(message)
      }
  }

}