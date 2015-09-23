/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2015, California Institute of Technology ("Caltech").
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

import org.scalatest.{Failed,Outcome}
import org.semanticweb.owlapi.apibinding.OWLManager
import gov.nasa.jpl.omf.scala.binding.owlapi._
import test.gov.nasa.jpl.omf.scala.core.{ functionalAPI => testFunctionalAPI }
import org.apache.xml.resolver.CatalogManager
import scala.util.Failure
import scala.util.Success
import scala.Predef._
import scala.{transient,Option,None,Some,StringContext,Unit}
import java.lang.IllegalArgumentException

abstract class OWLAPIOMFVocabularyImmutabilityTest(
                                        override val saveStore: OWLAPIOMFGraphStore,
                                        override val loadStore: OWLAPIOMFGraphStore )
  extends testFunctionalAPI.OMFVocabularyImmutabilityTest[OWLAPIOMF]( 
      saveStore, saveStore.omfModule.ops,
      loadStore, loadStore.omfModule.ops )
      
abstract class OWLAPIOMFVocabularyImmutabilityCatalogTest( @transient val catalogManager: CatalogManager )
  extends OWLAPIOMFVocabularyImmutabilityTest(
      saveStore = OWLAPIOMFGraphStore( OWLAPIOMFModule(Some(catalogManager)), OWLManager.createOWLOntologyManager() ),      
      loadStore = OWLAPIOMFGraphStore( OWLAPIOMFModule(Some(catalogManager)), OWLManager.createOWLOntologyManager() ) )

class OWLAPIOWFVocabularyImmutabilityTestLocalCatalog
  extends OWLAPIOMFVocabularyImmutabilityCatalogTest( catalogManager = new CatalogManager() ) {

  val catalogFile = "/ontologies/imce.local.catalog.xml"
  saveStore.catalogIRIMapper match {
      case None => 
        throw new IllegalArgumentException(
          "There should be a catalog IRI mapper since the store was constructed with a catalog manager")
      
      case Some( catalogIRImapper ) =>
        classOf[OWLAPIOWFVocabularyImmutabilityTestLocalCatalog].getResource(catalogFile) match {
          case null =>
            Option.apply(java.nio.file.Paths.get("ontologies", "imce.local.catalog.xml")) match {
              case Some(p)
                if p.toFile.exists() && p.toFile.canRead =>
                catalogIRImapper.parseCatalog( p.toFile.toURI ) match {
                  case Failure( t ) =>
                    throw new IllegalArgumentException(s"Cannot parse the test catalog: '${p.toFile.toURI}'", t )
                  case Success( _ ) =>
                    ()
                }
              case _ =>
                throw new IllegalArgumentException(s"There should be a '$catalogFile' resource on the classpath")
            }
          case testCatalogURL =>
            catalogIRImapper.parseCatalog( testCatalogURL.toURI ) match {
              case Failure( t ) => 
                throw new IllegalArgumentException(s"Cannot parse the test catalog: '$testCatalogURL'", t )
              case Success( _ ) =>
                ()              
            }
        }       
    }

  val saveMetadataIRI = saveStore.omfModule.ops.makeIRI("http://imce.jpl.nasa.gov/test/OWLAPIOMFVocabularySave")
  val saveMetadataOnt = saveStore.ontManager.createOntology( saveMetadataIRI )
  saveStore.setOMFMetadataOntology( saveMetadataOnt )

  override def preOMFSave(): Unit = {
  }

  override def postOMFSave(): Unit = {
    saveStore.catalogIRIMapper match {
      case None =>
        throw new IllegalArgumentException(
          "There should be a catalog IRI mapper since the store was constructed with a catalog manager")

      case Some(catalogIRImapper) =>
        val saveIRI = catalogIRImapper.resolveIRI(saveMetadataIRI, catalogIRImapper.saveResolutionStrategy)
        saveStore.ontManager.saveOntology(saveMetadataOnt, saveIRI)
    }
  }


  loadStore.catalogIRIMapper match {
      case None => 
        throw new IllegalArgumentException(
          "There should be a catalog IRI mapper since the store was constructed with a catalog manager")
      
      case Some( catalogIRImapper ) =>
        classOf[OWLAPIOWFVocabularyImmutabilityTestLocalCatalog].getResource(catalogFile) match {
          case null =>
            Option.apply(java.nio.file.Paths.get("ontologies", "imce.local.catalog.xml")) match {
              case Some(p)
                if p.toFile.exists() && p.toFile.canRead =>
                catalogIRImapper.parseCatalog( p.toFile.toURI ) match {
                  case Failure( t ) =>
                    throw new IllegalArgumentException(s"Cannot parse the test catalog: '${p.toFile.toURI}'", t )
                  case Success( _ ) =>
                    ()
                }
              case _ =>
                throw new IllegalArgumentException(s"There should be a '$catalogFile' resource on the classpath")
            }
          case testCatalogURL =>
            catalogIRImapper.parseCatalog( testCatalogURL.toURI ) match {
              case Failure( t ) =>
                throw new IllegalArgumentException(s"Cannot parse the test catalog: '$testCatalogURL'", t )
              case Success( _ ) =>
                ()              
            }
        }       
    }

  val loadMetadataIRI = loadStore.omfModule.ops.makeIRI("http://imce.jpl.nasa.gov/test/OWLAPIOMFVocabularyLoad")
  val loadMetadataOnt = loadStore.ontManager.createOntology( loadMetadataIRI )
  loadStore.setOMFMetadataOntology( loadMetadataOnt )

  override def preOMFLoad(): Unit = {
  }

  override def postOMFLoad(): Unit = {
    loadStore.catalogIRIMapper match {
      case None =>
        throw new IllegalArgumentException(
          "There should be a catalog IRI mapper since the store was constructed with a catalog manager")

      case Some(catalogIRImapper) =>
        val loadIRI = catalogIRImapper.resolveIRI(loadMetadataIRI, catalogIRImapper.saveResolutionStrategy)
        loadStore.ontManager.saveOntology(loadMetadataOnt, loadIRI)
    }
  }


}