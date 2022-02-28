/*
 * Copyright 2022 Tomas Zeman <tomas@functionals.cz>
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
 */
package fxb

import Pickler._
import fxb.Response.{Result, Stats}
import upickle.implicits.key

case class Response(
  success: String,
  message: Option[String] = None,
  stats: Option[Stats] = None,
  results: Seq[Result] = Seq.empty)

object Response {
  implicit val rw: ReadWriter[Response] = macroRW

  case class Stats(failed: Int, created: Int, deleted: Int, skipped: Int,
    updated: Int)
  object Stats {
    implicit val rw: ReadWriter[Stats] = macroRW
  }

  case class Err(
    `for`: String = "",
    code: String = "",
    path: String = "",
    value: String = "",
    message: String = "")

  object Err {
    implicit val rw: ReadWriter[Err] = macroRW
  }

  case class Result(
    @key("request-id") requestId: Option[Int] = None,
    errors: Seq[Err] = Seq.empty,
    id: Option[Int] = None,
    ref: Option[String] = None
  )
  object Result {
    implicit val rw: ReadWriter[Result] = macroRW
  }

}

case class ResponseW(winstrom: Response)

object ResponseW {
  implicit val rw: ReadWriter[ResponseW] = macroRW
}