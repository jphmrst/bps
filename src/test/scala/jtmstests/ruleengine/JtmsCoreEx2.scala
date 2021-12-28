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

package org.maraist.truthmaintenancesystems.justificationbased.ruleengine.tests
import scala.language.adhocExtensions
import scala.collection.mutable.ListBuffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.maraist.truthmaintenancesystems.justificationbased.ruleengine.*

class JTMScoreEx2 extends AnyFlatSpec with Matchers with JTMScoreEx1
    with JTMSexample[Symbol, String, Unit]("JTMS+JTRE simple example") {
  val contra = j.createNode(Symbol("Loser"), contradictionP = true)

  override def beliefsString: String =
    s"${super.beliefsString} contra:${contra.believed}"

  override def contradictoryString: String =
    s"${super.beliefsString} contra:${contra.isContradictory}"

  "JTMS+JTRE ex2" `should` "all pass" in {
    na.enableAssumption
    nb.enableAssumption
    nc.enableAssumption
    nd.enableAssumption

    // showAll(s"Before contra justify")
    j.justifyNode("j5", contra, ListBuffer(ne, nf))
    // showAll(s"After contra justify")

    // (Defun ex2 () ;; uses Ex1 to test the contradiction stuff.
    //   (setq contra (tms-create-node *jtms* 'Loser :contradictoryp T))
    //   (justify-node 'j5 contra (list ne nf)))
  }
}
