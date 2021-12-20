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

/**
  * Newer formatting functions for creating debugging output.
  */
object Blurb {

  /**
    * Wrapper for formatting functions for a type `A` which extend the
    * behavior to `Option[A]`s.
    */
  def option[A](aa: (A) => String)(o: Option[A]): String =
    o.map(aa).getOrElse("[none]")

  /**
    * Wrapper for formatting functions for a type `A` which extend the
    * behavior to `ListBuffer[A]`s.
    */
  def listBuf[A](aa: (A) => String, sep: String = "(empty)")
    (lb: ListBuffer[A]):
      String =
    "{" + lb.map(aa).mkString(sep) + "}"

  /**
    * Wrapper for formatting functions for a type `A` which extend the
    * behavior to `List[A]`s.
    */
  def list[A](aa: (A) => String, sep: String = "(empty)")
    (b: List[A]):
      String =
    "{" + b.map(aa).mkString(sep) + "}"

  /**
    * Formatting functions for [[Env]] arguments.
    */
  def env[D, I, R](e: Env[D, I, R]): String = e.envString

  /**
    * Formatting functions for `ListBuffer[Env]` arguments.
    */
  def envLB[D, I, R]: (ListBuffer[Env[D, I, R]]) => String = listBuf(env, "; ")

  /**
    * Formatting functions for [[Justification]] arguments.
    */
  def justification[D, I, R](j: Justification[D, I, R]): String = j match {
    case NodeAssumed(n) => s"Node \"${n.datum.toString}\" assumed"
    case MakeContradiction() => "Stipulated as contradictory"
    case j: Just[D, I, R] => j.blurb
  }

  /**
    * Formatting functions for [[Node]] arguments.
    */
  def bareNode[D, I, R](n: Node[D, I, R]): String = n.datum.toString

  /**
    * Formatting functions for [[Node]] arguments which include a note
    * of whether the node is contradictory.
    */
  def node[D, I, R](n: Node[D, I, R]): String =
    s"${bareNode(n)} (${if n.isContradictory then "" else "not "}contradictory)"

  /**
    * Formatting functions for `Option[Node]` arguments.
    */
  def nodeOption[D, I, R] = option(bareNode[D, I, R])

  /**
    * Formatting functions for `ListBuffer[Node]` arguments.
    */
  def nodeLB[D, I, R] = listBuf(bareNode[D, I, R], ",")
}
