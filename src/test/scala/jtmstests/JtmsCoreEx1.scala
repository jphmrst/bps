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

trait JTMScoreEx1 extends JTMSexample[Symbol, String] {
  val na = j.createNode(Symbol("a"), assumptionP = true)
  val nb = j.createNode(Symbol("b"), assumptionP = true)
  val nc = j.createNode(Symbol("c"), assumptionP = true)
  val nd = j.createNode(Symbol("d"), assumptionP = true)
  val ne = j.createNode(Symbol("e"), assumptionP = true)
  val nf = j.createNode(Symbol("f"), assumptionP = true)
  val ng = j.createNode(Symbol("g"), assumptionP = true)

  j.justifyNode("j1", nf, ListBuffer(na, nb))
  j.justifyNode("j2", ne, ListBuffer(nb, nc))
  j.justifyNode("j3", ng, ListBuffer(na, ne))
  j.justifyNode("j4", ng, ListBuffer(nd, ne))

  def beliefsString: String = s"a:${na.believed} b:${nb.believed} c:${nc.believed} d:${nd.believed} e:${ne.believed} f:${nf.believed} g:${ng.believed}"

  def contradictoryString: String = s"a:${na.isContradictory} b:${nb.isContradictory} c:${nc.isContradictory} d:${nd.isContradictory} e:${ne.isContradictory} f:${nf.isContradictory} g:${ng.isContradictory}"

  def printNodeAssumptions: Unit = {
    println("----------")
    println(s"$na assumptions ${na.assumptionsOfNode}")
    println(s"$nb assumptions ${nb.assumptionsOfNode}")
    println(s"$nc assumptions ${nc.assumptionsOfNode}")
    println(s"$nd assumptions ${nd.assumptionsOfNode}")
    println(s"$ne assumptions ${ne.assumptionsOfNode}")
    println(s"$nf assumptions ${nf.assumptionsOfNode}")
    println(s"$ng assumptions ${ng.assumptionsOfNode}")
  }

  def printNodeSupport: Unit = {
    println("----------")
    println(s"$na supports ${na.supportingJustificationForNode}")
    println(s"$nb supports ${nb.supportingJustificationForNode}")
    println(s"$nc supports ${nc.supportingJustificationForNode}")
    println(s"$nd supports ${nd.supportingJustificationForNode}")
    println(s"$ne supports ${ne.supportingJustificationForNode}")
    println(s"$nf supports ${nf.supportingJustificationForNode}")
    println(s"$ng supports ${ng.supportingJustificationForNode}")
  }

  // (defun ex1 ()
  //   (setq *jtms* (create-jtms "Simple Example" :debugging T)
  //      na (tms-create-node *jtms* 'a :assumptionp T)
  //      nb (tms-create-node *jtms* 'b :assumptionp T)
  //      nc (tms-create-node *jtms* 'c :assumptionp T)
  //      nd (tms-create-node *jtms* 'd :assumptionp T)
  //      ne (tms-create-node *jtms* 'e :assumptionp T)
  //      nf (tms-create-node *jtms* 'f :assumptionp T)
  //      ng (tms-create-node *jtms* 'g :assumptionp T))
  //   (justify-node 'j1 nf (list na nb))
  //   (justify-node 'j2 ne (list nb nc))
  //   (justify-node 'j3 ng (list na ne))
  //   (justify-node 'j4 ng (list nd ne))
  //   (enable-assumption na)
  //   (enable-assumption nb)
  //   (enable-assumption nc)
  //   (enable-assumption nd))
}

class JTMScoreEx1Test extends AnyFlatSpec with Matchers
    with JTMScoreEx1 with JTMSexample[Symbol, String]("Simple example") {
  "JTMS ex1" `should` "all pass" in {

    na.enableAssumption
    // showBeliefs(s"A enabled :: ")
    na.believed `should` be (true)
    nb.believed `should` be (false)
    nc.believed `should` be (false)
    nd.believed `should` be (false)
    ne.believed `should` be (false)
    nf.believed `should` be (false)
    ng.believed `should` be (false)
    // printNodeAssumptions
    na.assumptionsOfNode.length `should` be (1)
    na.assumptionsOfNode(0) `should` be (na)
    nb.assumptionsOfNode.isEmpty `should` be (true)
    nc.assumptionsOfNode.isEmpty `should` be (true)
    nd.assumptionsOfNode.isEmpty `should` be (true)
    ne.assumptionsOfNode.isEmpty `should` be (true)
    nf.assumptionsOfNode.isEmpty `should` be (true)
    ng.assumptionsOfNode.isEmpty `should` be (true)
    // printNodeSupport
    na.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    nb.supportingJustificationForNode.isEmpty `should` be (true)
    nc.supportingJustificationForNode.isEmpty `should` be (true)
    nd.supportingJustificationForNode.isEmpty `should` be (true)
    ne.supportingJustificationForNode.isEmpty `should` be (true)
    nf.supportingJustificationForNode.isEmpty `should` be (true)
    ng.supportingJustificationForNode.isEmpty `should` be (true)

    nb.enableAssumption
    // showBeliefs(s"B enabled :: ")
    na.believed `should` be (true)
    nb.believed `should` be (true)
    nc.believed `should` be (false)
    nd.believed `should` be (false)
    ne.believed `should` be (false)
    nf.believed `should` be (true)
    ng.believed `should` be (false)
    // printNodeAssumptions
    na.assumptionsOfNode.length `should` be (1)
    na.assumptionsOfNode(0) `should` be (na)
    nb.assumptionsOfNode.length `should` be (1)
    nb.assumptionsOfNode(0) `should` be (nb)
    nc.assumptionsOfNode.isEmpty `should` be (true)
    nd.assumptionsOfNode.isEmpty `should` be (true)
    ne.assumptionsOfNode.isEmpty `should` be (true)
    nf.assumptionsOfNode.length `should` be (2)
    nf.assumptionsOfNode.contains(na) `should` be (true)
    nf.assumptionsOfNode.contains(nb) `should` be (true)
    ng.assumptionsOfNode.isEmpty `should` be (true)
    // printNodeSupport
    na.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    nb.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    nc.supportingJustificationForNode.isEmpty `should` be (true)
    nd.supportingJustificationForNode.isEmpty `should` be (true)
    ne.supportingJustificationForNode.isEmpty `should` be (true)
    nf.supportingJustificationForNode.map(_ == j.justs(0)).getOrElse(false) `should` be (true)
    ng.supportingJustificationForNode.isEmpty `should` be (true)

    nc.enableAssumption
    // showBeliefs(s"C enabled :: ")
    na.believed `should` be (true)
    nb.believed `should` be (true)
    nc.believed `should` be (true)
    nd.believed `should` be (false)
    ne.believed `should` be (true)
    nf.believed `should` be (true)
    ng.believed `should` be (true)
    // printNodeAssumptions
    na.assumptionsOfNode.length `should` be (1)
    na.assumptionsOfNode(0) `should` be (na)
    nb.assumptionsOfNode.length `should` be (1)
    nb.assumptionsOfNode(0) `should` be (nb)
    nc.assumptionsOfNode.length `should` be (1)
    nc.assumptionsOfNode(0) `should` be (nc)
    nd.assumptionsOfNode.isEmpty `should` be (true)
    ne.assumptionsOfNode.length `should` be (2)
    ne.assumptionsOfNode.contains(nb) `should` be (true)
    ne.assumptionsOfNode.contains(nc) `should` be (true)
    nf.assumptionsOfNode.length `should` be (2)
    nf.assumptionsOfNode.contains(na) `should` be (true)
    nf.assumptionsOfNode.contains(nb) `should` be (true)
    ng.assumptionsOfNode.length `should` be (3)
    ng.assumptionsOfNode.contains(na) `should` be (true)
    ng.assumptionsOfNode.contains(nb) `should` be (true)
    ng.assumptionsOfNode.contains(nc) `should` be (true)
    // printNodeSupport
    na.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    nb.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    nc.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    nd.supportingJustificationForNode.isEmpty `should` be (true)
    ne.supportingJustificationForNode.map(_ == j.justs(1)).getOrElse(false) `should` be (true)
    nf.supportingJustificationForNode.map(_ == j.justs(0)).getOrElse(false) `should` be (true)
    ng.supportingJustificationForNode.map(_ == j.justs(2)).getOrElse(false) `should` be (true)

    nd.enableAssumption
    // showBeliefs("D enabled :: ")
    // j.debugJTMS
    na.believed `should` be (true)
    nb.believed `should` be (true)
    nc.believed `should` be (true)
    nd.believed `should` be (true)
    ne.believed `should` be (true)
    nf.believed `should` be (true)
    ng.believed `should` be (true)
    // printNodeAssumptions
    na.assumptionsOfNode.length `should` be (1)
    na.assumptionsOfNode(0) `should` be (na)
    nb.assumptionsOfNode.length `should` be (1)
    nb.assumptionsOfNode(0) `should` be (nb)
    nc.assumptionsOfNode.length `should` be (1)
    nc.assumptionsOfNode(0) `should` be (nc)
    nd.assumptionsOfNode.length `should` be (1)
    nd.assumptionsOfNode(0) `should` be (nd)
    ne.assumptionsOfNode.length `should` be (2)
    ne.assumptionsOfNode.contains(nb) `should` be (true)
    ne.assumptionsOfNode.contains(nc) `should` be (true)
    nf.assumptionsOfNode.length `should` be (2)
    nf.assumptionsOfNode.contains(na) `should` be (true)
    nf.assumptionsOfNode.contains(nb) `should` be (true)
    ng.assumptionsOfNode.length `should` be (3)
    ng.assumptionsOfNode.contains(na) `should` be (true)
    ng.assumptionsOfNode.contains(nb) `should` be (true)
    ng.assumptionsOfNode.contains(nc) `should` be (true)
    // printNodeSupport
    na.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    nb.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    nc.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    nd.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    ne.supportingJustificationForNode.map(_ == j.justs(1)).getOrElse(false) `should` be (true)
    nf.supportingJustificationForNode.map(_ == j.justs(0)).getOrElse(false) `should` be (true)
    ng.supportingJustificationForNode.map(_ == j.justs(2)).getOrElse(false) `should` be (true)

    na.retractAssumption
    // showBeliefs("A retracted :: ")
    // j.debugJTMS
    na.believed `should` be (false)
    nb.believed `should` be (true)
    nc.believed `should` be (true)
    nd.believed `should` be (true)
    ne.believed `should` be (true)
    nf.believed `should` be (false)
    ng.believed `should` be (true)
    // printNodeAssumptions
    na.assumptionsOfNode.isEmpty `should` be (true)
    nb.assumptionsOfNode.length `should` be (1)
    nb.assumptionsOfNode(0) `should` be (nb)
    nc.assumptionsOfNode.length `should` be (1)
    nc.assumptionsOfNode(0) `should` be (nc)
    nd.assumptionsOfNode.length `should` be (1)
    nd.assumptionsOfNode(0) `should` be (nd)
    ne.assumptionsOfNode.length `should` be (2)
    ne.assumptionsOfNode.contains(nb) `should` be (true)
    ne.assumptionsOfNode.contains(nc) `should` be (true)
    nf.assumptionsOfNode.isEmpty `should` be (true)
    ng.assumptionsOfNode.length `should` be (3)
    ng.assumptionsOfNode.contains(nb) `should` be (true)
    ng.assumptionsOfNode.contains(nc) `should` be (true)
    ng.assumptionsOfNode.contains(nd) `should` be (true)
    // printNodeSupport
    na.supportingJustificationForNode.isEmpty  `should` be (true)
    nb.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    nc.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    nd.supportingJustificationForNode.map(_ == EnabledAssumption).getOrElse(false) `should` be (true)
    ne.supportingJustificationForNode.map(_ == j.justs(1)).getOrElse(false) `should` be (true)
    nf.supportingJustificationForNode.isEmpty  `should` be (true)
    ng.supportingJustificationForNode.map(_ == j.justs(3)).getOrElse(false) `should` be (true)
  }
}
