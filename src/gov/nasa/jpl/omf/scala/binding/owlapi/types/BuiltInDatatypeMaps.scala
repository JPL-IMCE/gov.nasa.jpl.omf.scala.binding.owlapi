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
package gov.nasa.jpl.omf.scala.binding.owlapi.types

import gov.nasa.jpl.omf.scala.binding.owlapi.{OWLAPIOMFGraphStore, types}
import gov.nasa.jpl.omf.scala.core.ConstrainingFacet._
import gov.nasa.jpl.omf.scala.core.ConstrainingFacet.ExplicitTimezoneConstraint._
import gov.nasa.jpl.omf.scala.core.ConstrainingFacet.WhiteSpaceConstraint._
import gov.nasa.jpl.omf.scala.core.FundamentalFacet._
import gov.nasa.jpl.omf.scala.core.FundamentalFacet.CardinalityConstraint._
import gov.nasa.jpl.omf.scala.core.FundamentalFacet.OrderedConstraint._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

import scala.{Option, Some}
import scala.collection.immutable._
import scalaz._

/**
  * @see http://www.w3.org/TR/xmlschema11-2/#built-in-datatypes
  */
object BuiltInDatatypeMaps {

  /**
    *
    * @param omfStore OMF Storage provider
    * @return The normative built-in datatype maps defined for OWL, RDFS, XML Schema 1.1
    */
  def loadBuiltinDatatypeMap
  ()
  (implicit omfStore: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/
    Option[(types.ImmutableModelTerminologyGraph, types.Mutable2IMutableTerminologyMap)] = {
    import omfStore.omfModule.ops._
    for {
      xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
      xsd_mgraph <- makeTerminologyGraph(xsd_iri, isDefinition)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#anyAtomicType
      anyAtomicType <- addScalarDataType(xsd_mgraph, "anyAtomicType")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#anyURI
      anyURI <- addScalarDataType(xsd_mgraph, "anyURI")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, anyURI, anyAtomicType,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(whiteSpace(fixedCollapse)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#base64Binary
      base64Binary <- addScalarDataType(xsd_mgraph, "base64Binary")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, base64Binary, anyAtomicType,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(whiteSpace(fixedCollapse)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#boolean
      boolean <- addScalarDataType(xsd_mgraph, "boolean")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, boolean, anyAtomicType,
        Seq(ordered(_false), bounded(false), cardinality(finite), numeric(false)),
        Seq(whiteSpace(fixedCollapse)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#date
      date <- addScalarDataType(xsd_mgraph, "date")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, date, anyAtomicType,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(whiteSpace(fixedCollapse)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dateTime
      dateTime <- addScalarDataType(xsd_mgraph, "dateTime")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, dateTime, anyAtomicType,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(whiteSpace(fixedCollapse), explicitTimezone(nonFixedOptional)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dateTimeStamp
      dateTimeStamp <- addScalarDataType(xsd_mgraph, "dateTimeStamp")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, dateTimeStamp, dateTime,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(whiteSpace(fixedCollapse), explicitTimezone(fixedRequired)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#decimal
      decimal <- addScalarDataType(xsd_mgraph, "decimal")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, decimal, anyAtomicType,
        Seq(ordered(total), bounded(false), cardinality(countablyInfinite), numeric(true)),
        Seq(whiteSpace(fixedCollapse)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#integer
      integer <- addScalarDataType(xsd_mgraph, "integer")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, integer, decimal,
        Seq(ordered(total), bounded(false), cardinality(countablyInfinite), numeric(true)),
        Seq(fixedFractionDigits(0), whiteSpace(fixedCollapse), pattern("[\\-+]?[0-9]+")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#long
      long <- addScalarDataType(xsd_mgraph, "long")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, long, integer,
        Seq(ordered(total), bounded(true), cardinality(finite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMaxInclusive("9223372036854775807"),
          nonFixedMinInclusive("-9223372036854775808")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#int

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#short

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#byte

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#nonNegativeInteger

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#positiveInteger

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedLong

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedInt

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedShort

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedByte

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#nonPositiveInteger

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#negativeInteger

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#double

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#duration

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dayTimeDuration

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#yearMonthDuration

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#float

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gDay

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gMonth

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gMonthDay

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gYear

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gYearMonth

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#hexBinary

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: this is not intended for direct use
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NOTATION

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: requires an enclosing XML document context
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#QName

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#string

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#normalizedString

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#token

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#language

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#name

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NCName

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: requires an enclosing XML document context
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#ENTITY

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: this is intended for cross-references within an XML document
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#ID

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: this is intended for cross-references within an XML document
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#IDREF

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NMTOKEN

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#time

      rdfs_iri <- makeIRI("http://www.w3.org/1999/02/22-rdf-syntax-ns")
      rdfs_mgraph <- makeTerminologyGraph(rdfs_iri, isDefinition)
      _ <- addTerminologyGraphExtension(rdfs_mgraph, xsd_mgraph)

      // @see http://www.w3.org/TR/rdf11-concepts/#section-html
      // rdf:HTML

      // @see http://www.w3.org/TR/rdf11-concepts/#section-XMLLiteral
      // rdf:XMLLiteral

      owl_iri <- makeIRI("http://www.w3.org/2002/07/owl")
      owl_mgraph <- makeTerminologyGraph(owl_iri, isDefinition)
      _ <- addTerminologyGraphExtension(owl_mgraph, rdfs_mgraph)

      // @see http://www.w3.org/TR/owl2-syntax/#Datatype_Maps
      // owl:real

      // @see http://www.w3.org/TR/owl2-syntax/#Datatype_Maps
      // owl:rational

      result <- omfStore.asImmutableTerminologyGraph(owl_mgraph)
    } yield Some(result)
  }
}