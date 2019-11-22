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

abstract class OWLAPIOMFNestedGraphTest
(override val saveStore: OWLAPIOMFGraphStore,
 override val loadStore: OWLAPIOMFGraphStore )
  extends testFunctionalAPI.OMFNestedGraphTest[OWLAPIOMF](
    "NestedGraphTest",
    saveStore, saveStore.omfModule.ops,
    loadStore, loadStore.omfModule.ops )

abstract class OWLAPIOMFNestedGraphCatalogTest(@transient val catalogManager: CatalogManager )
  extends OWLAPIOMFNestedGraphTest(
    saveStore = OWLAPIOMFGraphStore.initGraphStore(
      OWLAPIOMFModule.owlAPIOMFModule(catalogManager, withOMFMetadata = false).valueOr { (errors: Set[java.lang.Throwable]) =>
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
      OWLAPIOMFModule.owlAPIOMFModule(catalogManager, withOMFMetadata = false).valueOr { (errors: Set[java.lang.Throwable]) =>
        val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
        throw new scala.IllegalArgumentException(message)
      },
      OWLManager.createOWLOntologyManager(),
      new CatalogResolver(catalogManager),
      catalogManager.getPrivateCatalog,
      excludeOMLContent = false,
      excludeOMLImports = false,
      excludePurlImports = false) )

class OWLAPIOWFNestedGraphTestLocalCatalog
  extends OWLAPIOMFNestedGraphCatalogTest( catalogManager = new CatalogManager() ) {

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

  Option.apply(classOf[OWLAPIOMFNestedGraphTest].getResource(catalogFile))
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