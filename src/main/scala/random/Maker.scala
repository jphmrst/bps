// Copyright (C) 2021, 2022 John Maraist.
// All rights reserved.
//
// See the LICENSE.txt and README-forbus-dekleer.txt files distributed
// with this work for a paragraph stating scope of permission and
// disclaimer of warranty, and for additional information regarding
// copyright ownership.  The above copyright notice and that paragraph
// must be included in any separate copy of this file.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.truthmaintenancesystems.random
import scala.util.Random
import scala.collection.mutable.ListBuffer
import org.maraist.truthmaintenancesystems.utils.Printing.*
import org.maraist.truthmaintenancesystems.assumptionbased.Blurb

class Maker(val random: Random) {
  def this(seed: Int) = this(new Random(seed))
  def this(seed: Long) = this(new Random(seed))
  def this() = this(new Random())
  given Random = random

  def intSet(size: Int, range: IntSampler): ListBuffer[Int] = {
    val buf = new ListBuffer[Int]
    while buf.size < size do buf += range.get
    buf
  }

  def intSetExcept(size: Int, range: IntSampler, disallow: Int):
      ListBuffer[Int] = {
    val buf = new ListBuffer[Int]
    while buf.size < size do {
      val add = range.get
      if add != disallow then buf += add
    }
    buf
  }
}

trait IntSampler {
  def get(using random: Random): Int
}

class IntRange(lower: Int, upper: Int) extends IntSampler {
  private val nextArg = upper - lower + 1
  def get(using random: Random): Int =
    if lower < upper then random.nextInt(nextArg) + lower else lower
}
