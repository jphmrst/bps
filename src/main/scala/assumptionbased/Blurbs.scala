// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University
// and Johan de Kleer, the Xerox Corporation.
// Copyright (C) 2021 John Maraist.
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

package org.maraist.truthmaintenancesystems.assumptionbased
import scala.util.control.NonLocalReturns.*
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}

// Assumption-based truth maintenance system, translated from F/dK
// version 61 of 7/21/92.

object Blurb {

  def option[A](aa: (A) => String)(o: Option[A]): String =
    o.map(aa).getOrElse("[none]")

  def listBuf[A](aa: (A) => String, sep: String)(lb: ListBuffer[A]): String =
    lb.map(aa).mkString(sep)

  def list[A](aa: (A) => String, sep: String)(b: List[A]): String =
    b.map(aa).mkString(sep)

  def env[D, I](e: Env[D, I]): String = e.envString

  def envLB[D, I]: (ListBuffer[Env[D, I]]) => String = listBuf(env, "; ")

  def justification[D, I](j: Justification[D, I]): String = j match {
    case s: Symbol => s.toString
    case j: Just[D, I] => j.blurb
  }

  def bareNode[D, I](n: Node[D, I]): String = n.datum.toString

  def node[D, I](n: Node[D, I]): String =
    s"${bareNode(n)} (${if n.isContradictory then "" else "not "}contradictory)"

  def nodeOption[D, I] = option(bareNode[D, I])

  def nodeLB[D, I] = listBuf(bareNode[D, I], ",")
}
