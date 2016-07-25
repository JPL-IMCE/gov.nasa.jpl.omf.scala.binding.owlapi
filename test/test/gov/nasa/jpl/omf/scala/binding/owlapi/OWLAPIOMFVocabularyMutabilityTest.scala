/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package test.gov.nasa.jpl.omf.scala.binding.owlapi

import org.semanticweb.owlapi.apibinding.OWLManager
import gov.nasa.jpl.omf.scala.binding.owlapi._
import test.gov.nasa.jpl.omf.scala.core.{ functionalAPI => testFunctionalAPI }
import org.apache.xml.resolver.CatalogManager
import scala.Predef._
import scala.collection.immutable.Set
import scala.{transient,Option,StringContext,Unit}
import java.lang.IllegalArgumentException

abstract class OWLAPIOMFVocabularyMutabilityTest
( override val saveStore: OWLAPIOMFGraphStore,
  override val loadStore: OWLAPIOMFGraphStore )
  extends testFunctionalAPI.OMFVocabularyMutabilityTest[OWLAPIOMF](
    saveStore, saveStore.omfModule.ops,
    loadStore, loadStore.omfModule.ops )

abstract class OWLAPIOMFVocabularyMutabilityCatalogTest( @transient val catalogManager: CatalogManager )
  extends OWLAPIOMFVocabularyMutabilityTest(
    saveStore = OWLAPIOMFGraphStore(
      OWLAPIOMFModule.owlAPIOMFModule(catalogManager).valueOr { (errors: Set[java.lang.Throwable]) =>
        val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
        throw new scala.IllegalArgumentException(message)
      },
      OWLManager.createOWLOntologyManager()),
    loadStore = OWLAPIOMFGraphStore(
      OWLAPIOMFModule.owlAPIOMFModule(catalogManager).valueOr { (errors: Set[java.lang.Throwable]) =>
        val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
        throw new scala.IllegalArgumentException(message)
      },
      OWLManager.createOWLOntologyManager()) )

class OWLAPIOWFVocabularyMutabilityTestLocalCatalog
  extends OWLAPIOMFVocabularyMutabilityCatalogTest( catalogManager = new CatalogManager() ) {

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