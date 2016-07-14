package com.netaporter.uri

import com.netaporter.uri.encoding.{ChainedUriEncoder, UriEncoder}
import com.netaporter.uri.config.UriConfig
import scala.collection.immutable._
import scala.Predef.String
import scala.Any

/**
 * Date: 23/08/2013
 * Time: 09:10
 */
package object dsl {

  import scala.language.implicitConversions

  implicit def uriToUriOps(uri: Uri) = new UriDsl(uri)

  implicit def encoderToChainedEncoder(enc: UriEncoder) = ChainedUriEncoder(enc :: Nil)

  implicit def stringToUri(s: String)(implicit c: UriConfig = UriConfig.default) = Uri.parse(s)(c)
  implicit def stringToUriDsl(s: String)(implicit c: UriConfig = UriConfig.default) = new UriDsl(stringToUri(s)(c))

  implicit def queryParamToUriDsl(kv: (String, Any))(implicit c: UriConfig = UriConfig.default) = new UriDsl(Uri.empty.addParam(kv._1, kv._2))

  implicit def uriToString(uri: Uri)(implicit c: UriConfig = UriConfig.default): String = uri.toString(c)
}
