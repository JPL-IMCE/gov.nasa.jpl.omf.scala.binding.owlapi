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
import test.gov.nasa.jpl.omf.scala.core.{ functionalAPI => testFunctionalAPI }
import org.apache.xml.resolver.CatalogManager
import scala.Predef._
import scala.{transient,Option,StringContext,Unit}
import java.lang.IllegalArgumentException

abstract class OWLAPIOMFVocabularyImmutabilityTest(
                                        override val saveStore: OWLAPIOMFGraphStore,
                                        override val loadStore: OWLAPIOMFGraphStore )
  extends testFunctionalAPI.OMFVocabularyImmutabilityTest[OWLAPIOMF]( 
      saveStore, saveStore.omfModule.ops,
      loadStore, loadStore.omfModule.ops )
      
abstract class OWLAPIOMFVocabularyImmutabilityCatalogTest( @transient val catalogManager: CatalogManager )
  extends OWLAPIOMFVocabularyImmutabilityTest(
      saveStore = OWLAPIOMFGraphStore(
        OWLAPIOMFModule.owlAPIOMFModule(catalogManager).valueOr { (errors: Set[java.lang.Throwable]) =>
          val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
          throw new scala.IllegalArgumentException(message)
        },
        OWLManager.createOWLOntologyManager() ),
      loadStore = OWLAPIOMFGraphStore(
        OWLAPIOMFModule.owlAPIOMFModule(catalogManager).valueOr { (errors: Set[java.lang.Throwable]) =>
          val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
          throw new scala.IllegalArgumentException(message)
        },
        OWLManager.createOWLOntologyManager() ) )

class OWLAPIOWFVocabularyImmutabilityTestLocalCatalog
  extends OWLAPIOMFVocabularyImmutabilityCatalogTest( catalogManager = new CatalogManager() ) {

  val catalogFile = "/ontologies/imce.local.catalog.xml"

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

  val saveMetadataIRI =
    saveStore.omfModule.ops.makeIRI("http://imce.jpl.nasa.gov/test/OWLAPIOMFVocabularySave")
    .valueOr { (errors: Set[java.lang.Throwable]) =>
      val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
      throw new scala.IllegalArgumentException(message)
    }
  val saveMetadataOnt = saveStore.ontManager.createOntology( saveMetadataIRI )
  saveStore.setOMFMetadataOntology( saveMetadataOnt )

  override def preOMFSave(): Unit = {
  }

  override def postOMFSave(): Unit = {
    val saveIRI = saveStore.catalogIRIMapper.resolveIRI(saveMetadataIRI, saveStore.catalogIRIMapper.saveResolutionStrategy)
    saveStore.saveOMFMetadataOntology(saveIRI).fold[Unit](
      (nels: Set[java.lang.Throwable]) =>
        fail("Errors during saving the metadata ontology", nels.head),
      identity
    )
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

  val loadMetadataIRI =
    loadStore.omfModule.ops.makeIRI("http://imce.jpl.nasa.gov/test/OWLAPIOMFVocabularyLoad")
    .valueOr { (errors: Set[java.lang.Throwable]) =>
      val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
      throw new scala.IllegalArgumentException(message)
    }
  val loadMetadataOnt = loadStore.ontManager.createOntology( loadMetadataIRI )
  loadStore.setOMFMetadataOntology( loadMetadataOnt )

  override def preOMFLoad(): Unit = {
  }

  override def postOMFLoad(): Unit = {
    val loadIRI = loadStore.catalogIRIMapper.resolveIRI(loadMetadataIRI, saveStore.catalogIRIMapper.saveResolutionStrategy)
    loadStore.saveOMFMetadataOntology(loadIRI).fold[Unit](
      (nels: Set[java.lang.Throwable]) =>
        fail("Errors during saving the metadata ontology", nels.head),
      identity
    )
  }


}