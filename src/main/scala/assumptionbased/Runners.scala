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
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}

def traceInterpretations1: Unit = {
  val atms = new ATMS[Symbol, String, Nothing]("atms-test0")
  val a = atms.createNode("A", isAssumption = true)
  val c = atms.createNode("C", isAssumption = true)
  val e = atms.createNode("E", isAssumption = true)
  val h = atms.createNode("H")
  val j1 = atms.justifyNode("R1", h, ListBuffer(c, e))
  val g = atms.createNode("G")
  val j2 = atms.justifyNode("R2", g, ListBuffer(a, c))
  val x = atms.createNode("X", isContradictory = true)
  val j3 = atms.justifyNode("R3", x, ListBuffer(g))
  val b = atms.createNode("B", isAssumption = true)
  val j4 = atms.justifyNode("R4", h, ListBuffer(b, c))

  atms.debugging = true
  atms.debugAtms
  val i1_hORg = atms.interpretations(List(List(h, g)))

  // val i1_hANDg = atms.interpretations(List(List(h), List(g)))
}
