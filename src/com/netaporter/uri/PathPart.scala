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
package com.netaporter.uri

import com.netaporter.uri.config.UriConfig
import Parameters._
import scala.{Any,AnyVal}
import scala.collection.immutable._
import scala.Predef.String

/**
 * Date: 28/08/2013
 * Time: 21:21
 */
trait PathPart extends Any {

  type Self <: PathPart

  /**
   * The non-parameter part of this pathPart
   *
   * @return
   */
  def part: String

  /**
   * Adds a matrix parameter to the end of this path part
   *
   * @param kv
   */
  def addParam(kv: Param): PathPart

  def params: ParamSeq

  def partToString(c: UriConfig): String

  def map(f: String=>String): Self
}

case class StringPathPart(part: String) extends AnyVal with PathPart {

  type Self = StringPathPart

  def params = Vector.empty

  def addParam(kv: Param) =
    MatrixParams(part, Vector(kv))

  def partToString(c: UriConfig) =
    c.pathEncoder.encode(part, c.charset)

  def map(f: String=>String) =
    StringPathPart(f(part))
}

case class MatrixParams(part: String, params: ParamSeq) extends PathPart with Parameters {

  type Self = MatrixParams

  def separator = ";"

  def withParams(paramsIn: ParamSeq) =
    MatrixParams(part, paramsIn)

  def partToString(c: UriConfig) =
    c.pathEncoder.encode(part, c.charset) + ";" + paramsToString(c.pathEncoder, c.charset)

  def addParam(kv: Param) =
    copy(params = params :+ kv)

  def map(f: String=>String) =
    MatrixParams(f(part), params)
}

object PathPart {
  def apply(path: String, matrixParams: ParamSeq = Seq.empty) =
    if(matrixParams.isEmpty) new StringPathPart(path) else MatrixParams(path, matrixParams)
}