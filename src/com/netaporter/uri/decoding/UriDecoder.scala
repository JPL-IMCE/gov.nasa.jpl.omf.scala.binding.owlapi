package com.netaporter.uri.decoding

import com.netaporter.uri.Parameters
import Parameters._
import scala.Predef.{ArrowAssoc,String}
/**
 * Date: 28/08/2013
 * Time: 21:01
 */
trait UriDecoder {
  def decode(u: String): String

  def decodeTuple(kv: Param) =
    decode(kv._1) -> kv._2.map(decode(_))
}
