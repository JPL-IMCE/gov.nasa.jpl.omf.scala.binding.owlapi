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

package gov.nasa.jpl.omf.scala.binding.owlapi

import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.omf.scala.core.OMFError
import org.semanticweb.owlapi.model.{OWLDataFactory, OWLDatatype, OWLLiteral}

import scala.{Int, Option, StringContext}
import scala.Predef.{String, augmentString}
import scala.util.{Failure, Success, Try}

object LiteralConversions {

  implicit def toPositiveIntegerLiteral
  (i: Option[Int])
  : Option[tables.taggedTypes.PositiveIntegerLiteral]
  = i.map { n =>
    if (n >= 0)
      tables.taggedTypes.positiveIntegerLiteral(n.toString)
    else
      throw OMFError.omfError("A PositiveIntegerLiteral must be a positive number; got: "+n)
  }

  implicit def toLiteralNumber
  (v: Option[String])
  : Option[tables.LiteralNumber]
  = v.flatMap(tables.LiteralNumber.parseLiteralNumber)

  implicit def toLiteralDateTime
  (v: Option[String])
  : Option[tables.LiteralDateTime]
  = v.flatMap(tables.LiteralDateTime.parseDateTime)

  def toOWLLiteral
  (l: tables.LiteralDateTime,
   f: OWLDataFactory)
  : OWLLiteral
  = toOWLLiteral(l, f, Option.empty[OWLDatatype])

  def toOWLLiteral
  (l: tables.LiteralDateTime,
   f: OWLDataFactory,
   dt: Option[OWLDatatype])
  : OWLLiteral
  = f.getOWLLiteral(l.value.stripPrefix("dateTime="),
        dt.getOrElse(f.getOWLDatatype("http://www.w3.org/2001/XMLSchema#dateTime")))

  def toOWLLiteral
  (l: tables.LiteralValue,
   f: OWLDataFactory)
  : OWLLiteral
  = toOWLLiteral(l, f, Option.empty[OWLDatatype])

  def toOWLLiteral
  (l: tables.LiteralValue,
   f: OWLDataFactory,
   dt: Option[OWLDatatype])
  : OWLLiteral
  = l.literalType match {
    case tables.LiteralBooleanType =>
      f.getOWLLiteral(l.value == "true")

    case tables.LiteralDateTimeType =>
      f.getOWLLiteral(l.value.stripPrefix("dateTime="),
        dt.getOrElse(f.getOWLDatatype("http://www.w3.org/2001/XMLSchema#dateTime")))

    case tables.LiteralStringType =>
      f.getOWLLiteral(l.value,
        dt.getOrElse(f.getStringOWLDatatype))

    case tables.LiteralUUIDType =>
      f.getOWLLiteral(l.value,
        dt.getOrElse(f.getStringOWLDatatype))

    case tables.LiteralURIType =>
      f.getOWLLiteral(l.value,
        dt.getOrElse(f.getOWLDatatype("http://www.w3.org/2001/XMLSchema#anyURI")))

    case tables.LiteralRealType =>
      f.getOWLLiteral(l.value.stripPrefix("{").stripSuffix("}"),
        dt.getOrElse(f.getOWLDatatype("http://www.w3.org/2002/07/owl#real")))

    case tables.LiteralRationalType =>
      f.getOWLLiteral(l.value,
        dt.getOrElse(f.getOWLDatatype("http://www.w3.org/2002/07/owl#rational")))

    case tables.LiteralFloatType =>
      f.getOWLLiteral(l.value,
        dt.getOrElse(f.getFloatOWLDatatype))

    case tables.LiteralPositiveIntegerType =>
      f.getOWLLiteral(l.value,
        dt.getOrElse(f.getIntegerOWLDatatype))

    case tables.LiteralDecimalType =>
      f.getOWLLiteral(l.value,
        dt.getOrElse(f.getOWLDatatype("http://www.w3.org/2001/XMLSchema#decimal")))
  }

  def toOWLLiteral
  (l: tables.LiteralNumber,
   f: OWLDataFactory)
  : OWLLiteral
  = toOWLLiteral(l, f, Option.empty[OWLDatatype])

  def toOWLLiteral
  (l: tables.LiteralNumber,
   f: OWLDataFactory,
   dt: Option[OWLDatatype])
  : OWLLiteral
  = l.literalType match {
    case tables.LiteralRealType =>
      f.getOWLLiteral(l.value.stripPrefix("{").stripSuffix("}"),
        dt.getOrElse(f.getOWLDatatype("http://www.w3.org/2002/07/owl#real")))

    case tables.LiteralRationalType =>
      f.getOWLLiteral(l.value,
        dt.getOrElse(f.getOWLDatatype("http://www.w3.org/2002/07/owl#rational")))

    case tables.LiteralFloatType =>
      f.getOWLLiteral(l.value,
        dt.getOrElse(f.getFloatOWLDatatype))

    case tables.LiteralPositiveIntegerType =>
      f.getOWLLiteral(l.value,
        dt.getOrElse(f.getIntegerOWLDatatype))

    case tables.LiteralDecimalType =>
      f.getOWLLiteral(l.value,
        dt.getOrElse(f.getOWLDatatype("http://www.w3.org/2001/XMLSchema#decimal")))
  }

  def fromOWLLiteral
  (l: OWLLiteral,
   f: OWLDataFactory)
  : Try[tables.LiteralValue]
  = {
    val dt = l.getDatatype.getIRI.getIRIString
    val lit = l.getLiteral
    if (l.isBoolean)
      Success(tables.LiteralValue(tables.LiteralBooleanType, if (l.parseBoolean()) "true" else "false"))
    else dt match {
      case "http://www.w3.org/2001/XMLSchema#dateTime" =>
        Success(tables.LiteralValue(tables.LiteralDateTimeType, s"dateTime=$lit"))
      case "http://www.w3.org/2001/XMLSchema#anyURI" =>
        Success(tables.LiteralValue(tables.LiteralURIType, lit))
      case "http://www.w3.org/2002/07/owl#real" =>
        Success(tables.LiteralValue(tables.LiteralRealType, s"{$lit}"))
      case "http://www.w3.org/2002/07/owl#rational" =>
        Success(tables.LiteralValue(tables.LiteralRationalType, lit))
      case "http://www.w3.org/2001/XMLSchema#float" =>
        Success(tables.LiteralValue(tables.LiteralFloatType, lit))
      case "http://www.w3.org/2001/XMLSchema#integer" =>
        Success(tables.LiteralValue(tables.LiteralPositiveIntegerType, lit))
      case "http://www.w3.org/2001/XMLSchema#decimal" =>
        Success(tables.LiteralValue(tables.LiteralDecimalType, lit))
      case "http://www.w3.org/2001/XMLSchema#string" =>
        if (lit.startsWith("uuid"))
          Success(tables.LiteralValue(tables.LiteralUUIDType, lit))
        else if (lit.startsWith("\"") && lit.endsWith("\""))
          Success(tables.LiteralValue(tables.LiteralStringType, lit))
        else
          Success(tables.LiteralValue(tables.LiteralStringType, lit))
      case _ =>
          Failure(new java.lang.IllegalArgumentException(
            s"fromOWLLiteral: unrecognized datatype: $dt and value '$lit'"
          ))
    }
  }

}
