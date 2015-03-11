/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package test.gov.nasa.jpl.omf.scala.binding.owlapi

import org.semanticweb.owlapi.apibinding.OWLManager
import gov.nasa.jpl.omf.scala.binding.owlapi._
import test.gov.nasa.jpl.omf.scala.core.{ functionalAPI => testFunctionalAPI }
import org.apache.xml.resolver.CatalogManager
import scala.util.Failure
import scala.util.Success

abstract class IMCE_OWL2_MOF2_LoadTestFromOWLAPI( val loadStore: OWLAPIOMFGraphStore )
  extends testFunctionalAPI.IMCE_OWL2_MOF2_LoadTest[OWLAPIOMF]( 
      loadStore, loadStore.omfModule.ops )
      
abstract class IMCE_OWL2_MOF2_LoadTestFromOWLAPICatalog( @transient val catalogManager: CatalogManager )
  extends IMCE_OWL2_MOF2_LoadTestFromOWLAPI( 
      loadStore = OWLAPIOMFGraphStore( OWLAPIOMFModule(Some(catalogManager)), OWLManager.createOWLOntologyManager() ) )

class IMCE_OWL2_MOF2_LoadTestFromOWLAPILocalCatalog
  extends IMCE_OWL2_MOF2_LoadTestFromOWLAPICatalog( catalogManager = new CatalogManager() ) {
  
  val catalogFile = "/ontologies/imce.local.catalog.xml"
  loadStore.catalogIRIMapper match {
      case None => 
        throw new IllegalArgumentException("There should be a catalog IRI mapper since the store was constructed with a catalog manager")
      
      case Some( catalogIRImapper ) =>
        classOf[OWLAPIOWFVocabularyTestLocalCatalog].getResource(catalogFile) match {
          case null => 
            throw new IllegalArgumentException(s"There should be a '${catalogFile}' resource on the classpath")
          case testCatalogURL =>
            catalogIRImapper.parseCatalog( testCatalogURL.toURI ) match {
              case Failure( t ) => 
                throw new IllegalArgumentException(s"Cannot parse the test catalog: '${testCatalogURL}'", t )
              case Success( _ ) =>
                ()              
            }
        }       
    }
}