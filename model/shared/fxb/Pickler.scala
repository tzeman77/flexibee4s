/*
 * Copyright 2021-2024 Tomas Zeman <tomas@functionals.cz>
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

import upickle.AttributeTagged
import upickle.core.Util

import scala.util.Try

object Pickler extends AttributeTagged {

  override implicit def OptionWriter[T](implicit
    evidence$1: Pickler.Writer[T]): Pickler.Writer[Option[T]] =
    implicitly[Writer[T]].comap[Option[T]] {
      case None => null.asInstanceOf[T]
      case Some(v) => v
    }

  override implicit def OptionReader[T](implicit
    evidence$1: Pickler.Reader[T]): Pickler.Reader[Option[T]] =
    implicitly[Reader[T]].mapNulls{
      case null => None
      case v => Some(v)
    }

  override implicit val IntReader: Pickler.Reader[Int] = new SimpleReader[Int] {
    override def expectedMsg = "expected number"
    override def visitInt32(d: Int, index: Int): Int = d
    override def visitInt64(d: Long, index: Int): Int = d.toInt
    override def visitUInt64(d: Long, index: Int): Int = d.toInt
    override def visitFloat64(d: Double, index: Int): Int = d.toInt
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int,
      expIndex: Int, index: Int): Int = {
      Util.parseIntegralNum(s, decIndex, expIndex, index).toInt
    }

    override def visitString(s: CharSequence, index: Int): Int =
      Try{s.toString.toInt}.getOrElse(Int.MinValue)
  }

}
