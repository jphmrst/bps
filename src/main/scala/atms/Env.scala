// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University
// and Johan de Kleer, the Xerox Corporation.
// Copyright (C) 2021 John Maraist.
// All rights reserved.
//
// See the LICENSE.txt and README-forbus-dekleer.txt files distributed
// with this work for a paragraph stating scope of permission
// and disclaimer of warranty, and for additional
// information regarding copyright ownership.    The above copyright notice and that
// paragraph must be included in any separate copy of this file.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.


// Translated from KDF/JdK version 61 of 7/21/92.

package org.maraist.tms.atms
import scala.collection.mutable.ListBuffer

type EnvList[D] = ListBuffer[Option[Env[D]]]

class Env[D](
  val assumptions: List[TMSnode[D]],
  var index: Int = 0,
  var count: Int = 0
) {

  def this(atms: ATMS[D], assumptions: List[TMSnode[D]]) = this(assumptions)

  /** Number of assumptions. */

  var nodes: ListBuffer[TMSnode[D]] = ListBuffer.empty

  var isNogood: Boolean = false

  /** Call this if becomes nogood. */
  var rules = {}

  override def toString(): String = s"E-$index"

  def envOrder(that: Env[?]): Boolean = index < that.index

  def isSubset(that: Env[D]): Boolean = ???

  def +(that: Env[D]): Env[D] = ???
}
