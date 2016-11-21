package converters

import java.lang.System
import java.net.URI
import java.nio.file.{Paths,Path}

import gov.nasa.jpl.omf.scala.binding.owlapi._
import org.semanticweb.owlapi.model.IRI

import scala.{Array,Either,Left,Option,None,Right,Some,Unit}
import scala.Predef.{genericWrapArray,String}
import scala.collection.immutable.{Seq,Set}
import scala.util.control.Exception._

import scalaz._, Scalaz._

object ConvertOWL2Tables {

  def loadOntologies(catalogFile: Path, iri: IRI)
  : Set[java.lang.Throwable] \/ (types.ImmutableModelTerminologyGraph, types.Mutable2ImmutableTerminologyMap)
  = for {
    store <- createOMFGraphStore()
    _ <- loadCatalog(store, catalogFile)
    result <- store.loadTerminologyGraph(iri)
  } yield result

  case class Config
  ( catalogFile: Option[Path] = None,
    iri: Option[IRI] = None,
    outputDir: Option[Path] = None ) {

    def validate: Seq[String] \/ ValidConfig
    = {

      val c = catalogFile.fold[Seq[String] \/ Path](-\/(Seq("Missing catalogFile argument")))(\/-(_))
      val i = iri.fold[Seq[String] \/ IRI](-\/(Seq("Missing iri argument")))(\/-(_))
      val d = outputDir.fold[Seq[String] \/ Path](-\/(Seq("Missing outputDir argument")))(\/-(_))

      val s =
        c.swap.toOption.getOrElse(Seq.empty) ++
        i.swap.toOption.getOrElse(Seq.empty) ++
        d.swap.toOption.getOrElse(Seq.empty)

      if (s.nonEmpty)
        -\/(s)
      else
        for {
          _c <- c
          _i <- i
          _d <- d
          _r = ValidConfig(_c, _i, _d)
        } yield _r
    }
  }

  case class ValidConfig
  ( catalogFile: Path,
    iri: IRI,
    outputDir: Path )

  def parseExistingFilePath(s: String)
  : Either[String,Path]
  = nonFatalCatch[Either[String,Path]]
    .withApply { (t: java.lang.Throwable) => Left[String,Path](t.getMessage) }
    .apply {
      val p = Paths.get(URI.create(s))
      val f = p.toFile
      if (f.exists && f.isFile && f.canRead)
        Right[String,Path](p)
      else
        Left[String,Path]("Non-existent or unreadable file path: " + s)
    }

  def parseExistingDirectoryPath(s: String)
  : Either[String,Path]
  = nonFatalCatch[Either[String,Path]]
    .withApply { (t: java.lang.Throwable) => Left[String,Path](t.getMessage) }
    .apply {
      val p = Paths.get(URI.create(s))
      val d = p.toFile
      if (d.exists && d.isDirectory && d.canRead && d.canWrite)
        Right[String,Path](p)
      else
        Left[String,Path]("Non-existent or non-writeable directory path: " + s)
    }

  def parseIRI(s: String)
  : Either[String,IRI]
  = nonFatalCatch[Either[String,IRI]]
    .withApply { (t: java.lang.Throwable) => Left[String,IRI](t.getMessage) }
    .apply { Right[String,IRI](IRI.create(s)) }

  val configParser = new scopt.OptionParser[Config]("convertOntologies2Tables") {
    head("convert ontologies to OMF tables")

    opt[Either[String,Path]]('c', "catalog")(scopt.Read.reads { parseExistingFilePath })
      .validate(x => x.map(_ => ()))
      .action( (x, c) => x.fold(
        fa = (_: String) => c,
        fb = (p: Path) => c.copy(catalogFile = Some(p))
      ))
      .text("catalog is a required Path property to an existing catalog file")

    opt[Either[String, IRI]]('i', "iri")(scopt.Read.reads { parseIRI })
      .validate(x => x.map(_ => ()))
      .action( (x, c) => x.fold(
        fa = (_: String) => c,
        fb = (i: IRI) => c.copy(iri=Some(i))
      ))
      .text("iri is a required IRI property")

    opt[Either[String,Path]]('o', "out")(scopt.Read.reads { parseExistingDirectoryPath })
      .validate(x => x.map(_ => ()))
      .action( (x, c) => x.fold(
        fa = (_: String) => c,
        fb = (p: Path) => c.copy(outputDir = Some(p))
      ))
      .text("out is a required Path property to a writeable directory")

  }

  def main
  (args: Array[String])
  : Unit
  = configParser.parse(args, Config()) match {
    case Some(c) =>
      c.validate match {
        case -\/(errors) =>
          System.err.println(errors.size + " argument errors")
          errors.foreach(System.err.println)
          System.exit(-1)
        case \/-(vc) =>
          runMain(vc)
      }

    case None =>
      System.out.println("None...")
      // bad arguments; error message will have been displayed.
  }

  def runMain(vc: ValidConfig)
  : Unit
  = {
    System.out.println(vc)
  }
}
