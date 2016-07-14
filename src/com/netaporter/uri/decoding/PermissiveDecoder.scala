package com.netaporter.uri.decoding

import scala.Predef.String

class PermissiveDecoder(child: UriDecoder) extends UriDecoder {
  def decode(s: String) = {
    try {
      child.decode(s)
    } catch {
      case _: java.lang.Throwable => s
    }
  }
}

object PermissivePercentDecoder extends PermissiveDecoder(PercentDecoder)