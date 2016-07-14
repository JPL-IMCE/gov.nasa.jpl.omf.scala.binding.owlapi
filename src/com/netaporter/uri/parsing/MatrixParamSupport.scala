package com.netaporter.uri.parsing

import org.parboiled2._
import com.netaporter.uri.PathPart
import com.netaporter.uri.Parameters._
import scala.Predef.String
import scala.{Any,Boolean}
import scala.collection.immutable._

trait MatrixParamSupport {
  this: Parser with UriParser =>

  def _plainPathPart: Rule1[String] = rule {
    capture(zeroOrMore(!anyOf(";/?#") ~ ANY))
  }

  def _matrixParam: Rule1[Param] = rule {
    capture(zeroOrMore(!anyOf(";/=?#") ~ ANY)) ~ "=" ~ capture(zeroOrMore(!anyOf(";/=?#") ~ ANY)) ~> extractTuple
  }

  override def _pathSegment: Rule1[PathPart] = rule {
    _plainPathPart ~ zeroOrMore(";") ~
    zeroOrMore(_matrixParam).separatedBy(oneOrMore(";")) ~
    zeroOrMore(";") ~> extractPathPartWithMatrixParams
  }

  val extractPathPartWithMatrixParams = (pathPart: String, matrixParams: ParamSeq) => {
    val decodedPathPart = pathDecoder.decode(pathPart)
    val decodedMatrixParams = matrixParams.map(pathDecoder.decodeTuple)
    PathPart(decodedPathPart, decodedMatrixParams.toVector)
  }
}
