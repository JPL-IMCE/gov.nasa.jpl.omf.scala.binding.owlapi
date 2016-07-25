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
package com.netaporter.uri.encoding

import PercentEncoder._
import scala.{Char}
import scala.collection.immutable._
import scala.Predef.{augmentString}

case class PercentEncoder(charsToEncode: Set[Char] = DEFAULT_CHARS_TO_ENCODE) extends UriEncoder {

  def shouldEncode(ch: Char) = {
    !ascii(ch) || charsToEncode.contains(ch)
  }

  def encodeChar(ch: Char) = "%" + toHex(ch)
  def toHex(ch: Char) = "%04x".format(ch.toInt).substring(2).toUpperCase

  /**
   * Determines if this character is in the ASCII range (excluding control characters)
   */
  def ascii(ch: Char) = ch > 31 && ch < 127

  def --(chars: Char*) = new PercentEncoder(charsToEncode -- chars)
  def ++(chars: Char*) = new PercentEncoder(charsToEncode ++ chars)
}

object PercentEncoder {

  val USER_INFO_CHARS_TO_ENCODE = Set (
    ' ', '%', '<', '>', '[', ']', '#', '%', '{', '}', '^', '`', '|', '?', '@', ':', '/'
  )

  val PATH_CHARS_TO_ENCODE = Set (
    ' ', '%', '<', '>', '[', ']', '#', '%', '{', '}', '^', '`', '|', '?'
  )

  val QUERY_CHARS_TO_ENCODE = Set (
    ' ', '%', '<', '>', '[', ']', '#', '%', '{', '}', '^', '`', '|', '&', '\\', '+', '='
  )

  val FRAGMENT_CHARS_TO_ENCODE = Set('#')


  val GEN_DELIMS = Set(':', '/', '?',  '#', '[', ']', '@')
  val SUB_DELIMS  = Set('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=')
  val RESERVED = GEN_DELIMS ++ SUB_DELIMS

  val EXCLUDED = Set('"') // RFC 2396 section 2.4.3

  /**
   * Probably more than you need to percent encode. Wherever possible try to use a tighter Set of characters
   * to encode depending on your use case
   */
  val DEFAULT_CHARS_TO_ENCODE = RESERVED ++ PATH_CHARS_TO_ENCODE ++ QUERY_CHARS_TO_ENCODE ++ EXCLUDED
}