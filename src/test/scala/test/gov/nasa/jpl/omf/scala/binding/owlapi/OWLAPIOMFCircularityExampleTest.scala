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

import gov.nasa.jpl.omf.scala.binding.owlapi._
import org.apache.xml.resolver.CatalogManager
import org.apache.xml.resolver.tools.CatalogResolver
import org.semanticweb.owlapi.apibinding.OWLManager
import test.gov.nasa.jpl.omf.scala.core.{functionalAPI => testFunctionalAPI}

import scala.Predef._
import scala.{Option, StringContext, Unit, transient}
import java.lang.IllegalArgumentException

import scala.collection.immutable.Set

abstract class OWLAPIOMFCircularityExampleTest(
                                        override val saveStore: OWLAPIOMFGraphStore,
                                        override val loadStore: OWLAPIOMFGraphStore )
  extends testFunctionalAPI.IMCECircularExample[OWLAPIOMF](
      saveStore, saveStore.omfModule.ops,
      loadStore, loadStore.omfModule.ops )

abstract class OWLAPIOMFCircularityExampleCatalogTest(@transient val catalogManager: CatalogManager )
  extends OWLAPIOMFCircularityExampleTest(
      saveStore = OWLAPIOMFGraphStore.initGraphStore(
        OWLAPIOMFModule.owlAPIOMFModule(catalogManager).valueOr { (errors: Set[java.lang.Throwable]) =>
          val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
          throw new scala.IllegalArgumentException(message)
        },
        OWLManager.createOWLOntologyManager(),
        new CatalogResolver(catalogManager),
        catalogManager.getPrivateCatalog,
        excludeOMLContent = false,
        excludeOMLImports = false,
        excludePurlImports = false),
      loadStore = OWLAPIOMFGraphStore.initGraphStore(
        OWLAPIOMFModule.owlAPIOMFModule(catalogManager).valueOr { (errors: Set[java.lang.Throwable]) =>
          val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
          throw new scala.IllegalArgumentException(message)
        },
        OWLManager.createOWLOntologyManager(),
        new CatalogResolver(catalogManager),
        catalogManager.getPrivateCatalog,
        excludeOMLContent = false,
        excludeOMLImports = false,
        excludePurlImports = false) )

class OWLAPIOWFCircularityExampleTestLocalCatalog
  extends OWLAPIOMFCircularityExampleCatalogTest( catalogManager = new CatalogManager() ) {

  val catalogFile = "/ontologies/oml.catalog.xml"

  Option.apply(classOf[OWLAPIOWFVocabularyMutabilityTestLocalCatalog].getResource(catalogFile))
    .fold[Unit]({
    Option.apply(java.nio.file.Paths.get("ontologies", "imce.local.catalog.xml"))
      .fold[Unit]({
      throw new IllegalArgumentException(s"There should be a '$catalogFile' resource on the classpath")
    }) { p =>
      if (p.toFile.exists() && p.toFile.canRead)
        saveStore.catalogIRIMapper.parseCatalog(p.toFile.toURI)
          .valueOr { (errors: Set[java.lang.Throwable]) =>
            val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
            throw new scala.IllegalArgumentException(message)
          }
      else
        throw new IllegalArgumentException(s"There should be a '$catalogFile' resource on the classpath")
    }
  }){ testCatalogURL =>
    saveStore.catalogIRIMapper.parseCatalog(testCatalogURL.toURI)
      .valueOr { (errors: Set[java.lang.Throwable]) =>
        val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
        throw new scala.IllegalArgumentException(message)
      }
  }

  override def preOMFSave(): Unit = {
  }

  override def postOMFSave(): Unit = {
  }

  Option.apply(classOf[OWLAPIOWFVocabularyMutabilityTestLocalCatalog].getResource(catalogFile))
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

  override def preOMFLoad(): Unit = {
  }

  override def postOMFLoad(): Unit = {
  }
}