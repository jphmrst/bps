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

class JTMScore extends AnyFlatSpec with Matchers {

  "JTMS ex1" `should` "all pass" in {
    val j = new JTMS[Symbol, String]("Simple Example", debugging = true)
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
    na.enableAssumption
    nb.enableAssumption
    nc.enableAssumption
    nd.enableAssumption
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

  "JTMS ex2" `should` "all pass" in {
    val j = new JTMS[Symbol, String]("Simple Example", debugging = true)
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
    na.enableAssumption
    nb.enableAssumption
    nc.enableAssumption
    nd.enableAssumption

    val contra = j.createNode(Symbol("Loser"), contradictionP = true)
    j.justifyNode("j5", contra, ListBuffer(ne, nf))
    // (Defun ex2 () ;; uses Ex1 to test the contradiction stuff.
    //   (setq contra (tms-create-node *jtms* 'Loser :contradictoryp T))
    //   (justify-node 'j5 contra (list ne nf)))
  }

  "JTMS ex3" `should` "all pass" in {
    val j = new JTMS[Symbol, String](
      "Multiple support example", debugging = true)
    val na = j.createNode(Symbol("A"), assumptionP = true)
    val nc = j.createNode(Symbol("C"), assumptionP = true)
    val ne = j.createNode(Symbol("E"), assumptionP = true)
    val nh = j.createNode(Symbol("h"))
    na.enableAssumption
    nc.enableAssumption
    ne.enableAssumption
    j.justifyNode("R1", nh, ListBuffer(nc, ne))
    val ng = j.createNode(Symbol("g"))
    j.justifyNode("R2", ng, ListBuffer(na, nc))
    val contradiction =
      j.createNode(Symbol("CONTRADICTION"), contradictionP = true)
    j.justifyNode("R3", contradiction, ListBuffer(ng))
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

  "Smoke" `should` "test" in { }
}

// (defun get-node (datum jtms)
//   (dolist (node (jtms-nodes jtms))
//     (if (equal datum (tms-node-datum node)) (return node))))

// (defun get-justification (num jtms)
//   (dolist (just (jtms-justs jtms))
//     (if (= num (just-index just)) (return just))))

// (proclaim '(special na nb nc nd ne nf ng contra *jtms*))


