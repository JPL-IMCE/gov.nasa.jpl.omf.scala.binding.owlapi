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
package com.netaporter.uri.parsing

import com.netaporter.uri.config.UriConfig
import scala.util.Failure
import org.parboiled2._
import com.netaporter.uri.{StringPathPart, QueryString, PathPart}
import com.netaporter.uri.decoding.UriDecoder
import com.netaporter.uri.Parameters._
import org.parboiled2.ParseError
import scala.util.Success
import scala.util.Failure
import scala.collection.immutable._
import scala.{Int,Option,None,Some}
import scala.Predef.{augmentString,ArrowAssoc,String}

trait UriParser {

  def pathDecoder: UriDecoder
  def queryDecoder: UriDecoder
  def fragmentDecoder: UriDecoder

  def _pathSegment: Rule1[PathPart]

  val extractInt = (num: String) =>
    num.toInt

  val extractUserInfo = (user: String, pass: Option[Option[String]]) =>
    UserInfo(pathDecoder.decode(user), pass.map(_.fold("")(pathDecoder.decode)))

  val extractAuthority = (userInfo: Option[UserInfo], host: String, port: Option[String]) =>
    Authority(userInfo.map(_.user), userInfo.flatMap(_.pass), host, port.map(_.toInt))

  val extractFragment = (x: String) =>
    fragmentDecoder.decode(x)

  val extractQueryString = (tuples: ParamSeq) =>
    QueryString(tuples.toVector.map(queryDecoder.decodeTuple))

  val extractPathPart = (pathPart: String) => {
    val decodedPathPart = pathDecoder.decode(pathPart)
    StringPathPart(decodedPathPart)
  }

  val extractPathParts = (pp: Seq[PathPart]) =>
    pp.toVector

  val extractTuple = (k: String, v: String) =>
    k -> Some(v)

  val extractTok = (k: String) => (k -> None):(String,Option[String])

  /**
   * Used to made parsing easier to follow
   */
  case class Authority(user: Option[String], password: Option[String], host: String, port: Option[Int])
  case class UserInfo(user: String, pass: Option[String])
}

object UriParser {
  def parse(s: String, config: UriConfig) = {
    val parser =
      if(config.matrixParams) new DefaultUriParser(s, config) with MatrixParamSupport
      else                    new DefaultUriParser(s, config)

    parser._uri.run() match {
      case Success(uri) =>
        uri

      case Failure(pe@ParseError(position, _, formatTraces)) =>
        throw new java.net.URISyntaxException(s, "Invalid URI could not be parsed. " + formatTraces, position.index)

      case Failure(e) =>
        throw e
    }
  }

  def parseQuery(s: String, config: UriConfig) = {
    val withQuestionMark = if(s.head == '?') s else "?" + s
    val parser = new DefaultUriParser(withQuestionMark, config)

    parser._queryString.run() match {
      case Success(queryString) =>
        queryString

      case Failure(pe@ParseError(position, _, formatTraces)) =>
        throw new java.net.URISyntaxException(s, "Invalid URI could not be parsed. " + formatTraces, position.index)

      case Failure(e) =>
        throw e
    }
  }
}