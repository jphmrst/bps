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

package org.maraist.truthmaintenancesystems.justificationbased.tests
import scala.language.adhocExtensions
import scala.collection.mutable.ListBuffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.maraist.truthmaintenancesystems.justificationbased.*

trait JTMScoreEx3 extends JTMSexample[Symbol, String] {
  val na = j.createNode(Symbol("A"), assumptionP = true)
  val nc = j.createNode(Symbol("C"), assumptionP = true)
  val ne = j.createNode(Symbol("E"), assumptionP = true)
  val ng = j.createNode(Symbol("g"))
  val nh = j.createNode(Symbol("h"))

  val contradiction =
    j.createNode(Symbol("CONTRADICTION"), contradictionP = true)

  j.justifyNode("R1", nh, ListBuffer(nc, ne))
  j.justifyNode("R2", ng, ListBuffer(na, nc))
  j.justifyNode("R3", contradiction, ListBuffer(ng))

  def beliefsString: String = s"a:${na.believed} c:${nc.believed} e:${ne.believed} g:${ng.believed} h:${nh.believed}"

  def contradictoryString: String = s"a:${na.isContradictory} c:${nc.isContradictory} e:${ne.isContradictory} g:${ng.isContradictory} h:${nh.isContradictory}"

  // (defun ex3 ()
  //   (setq *jtms* (create-jtms "Multiple support example")
  //      assumption-a (tms-create-node *jtms* 'A :assumptionp T)
  //      assumption-c (tms-create-node *jtms* 'C :assumptionp T)
  //      assumption-e (tms-create-node *jtms* 'E :assumptionp T)
  //      node-h (tms-create-node *jtms* 'h))
  //   (enable-assumption assumption-a)
  //   (enable-assumption assumption-c)
  //   (enable-assumption assumption-e)
  //   (justify-node 'R1 node-h (list assumption-c assumption-e))
  //   (setq node-g (tms-create-node *jtms* 'g))
  //   (justify-node 'R2 node-g (list assumption-a assumption-c))
  //   (setq contradiction (tms-create-node *jtms*
  //                                     'CONTRADICTION :contradictoryp T))
  //   (justify-node 'R3 contradiction (list node-g)))
}

class JTMScoreEx3Test extends AnyFlatSpec with Matchers with JTMScoreEx3
    with JTMSexample[Symbol, String]("Multiple support example") {
  "JTMS ex3" `should` "all pass" in {
    na.enableAssumption
    nc.enableAssumption
    ne.enableAssumption
  }
}
