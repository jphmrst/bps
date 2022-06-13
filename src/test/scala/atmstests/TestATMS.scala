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

package org.maraist.truthmaintenancesystems.assumptionbased.tests
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.maraist.truthmaintenancesystems.assumptionbased.{ATMS, Node}
import org.maraist.truthmaintenancesystems.assumptionbased.Blurb.*

class TestATMS extends AnyFlatSpec with Matchers {

  // Extension of F&dK Sec. 12.5
  "FdK simple example" `should` "all pass" in {
    val atms = new ATMS[Symbol, String, Nothing]("atms-test0")

    val a = atms.createNode("A", isAssumption = true)
    a.label.length `should` be (1)
    a.label(0).assumptions.length `should` be (1)
    a.label(0).assumptions(0) `should` be (a)
    atms.lookupEnv(List(a)).map(a.label.contains(_)).getOrElse(false)
      `should` be (true)

    val c = atms.createNode("C", isAssumption = true)
    c.label.length `should` be (1)
    c.label(0).assumptions.length `should` be (1)
    c.label(0).assumptions(0) `should` be (c)
    atms.lookupEnv(List(c)).map(c.label.contains(_)).getOrElse(false)
      `should` be (true)

    val e = atms.createNode("E", isAssumption = true)
    e.label.length `should` be (1)
    e.label(0).assumptions.length `should` be (1)
    e.label(0).assumptions(0) `should` be (e)
    atms.lookupEnv(List(e)).map(e.label.contains(_)).getOrElse(false)
      `should` be (true)

    val h = atms.createNode("H")
    h.label.length `should` be (0)
    atms.debugAtms

    val j1 = atms.justifyNode("R1", h, List(c, e))
    atms.debugAtms
    h.label.length `should` be (1)
    atms.lookupEnv(List(c, e)).map(h.label.contains(_)).getOrElse(false)
      `should` be (true)

    val g = atms.createNode("G")
    g.label.isEmpty `should` be (true)

    val j2 = atms.justifyNode("R2", g, List(a, c))
    g.label.isEmpty `should` be (false)
    g.label.length `should` be (1)
    atms.lookupEnv(List(a, c)).map(g.label.contains(_)).getOrElse(false)
      `should` be (true)

    val x = atms.createNode("X", isContradictory = true)
    val j3 = atms.justifyNode("R3", x, List(g))
    atms.debugAtms
    g.label.isEmpty `should` be (true)
    g.label.length `should` be (0)
    atms.lookupEnv(List(g)).map(g.label.contains(_)).getOrElse(false)
      `should` be (false)
    atms.lookupEnv(List(a, c)).map(_.isNogood).getOrElse(false)
      `should` be (true)

    val b = atms.createNode("B", isAssumption = true)
    b.label.length `should` be (1)
    atms.lookupEnv(List(b)).map(b.label.contains(_)).getOrElse(false)
    `should` be (true)

    val j4 = atms.justifyNode("R4", h, List(b, c))
    atms.debugAtms
    h.label.length `should` be (2)
    atms.lookupEnv(List(c, e)).map(h.label.contains(_)).getOrElse(false)
      `should` be (true)
    atms.lookupEnv(List(c, b)).map(h.label.contains(_)).getOrElse(false)
      `should` be (true)

    val i1_hORg = atms.interpretations(List(List(h, g)))
    atms.dbg(envL(i1_hORg)) // Lisp gives {B,C} and {C,E}
    i1_hORg.length `should` be (2)
    atms.lookupEnv(List(c, e)).map(i1_hORg.contains(_)).getOrElse(false)
      `should` be (true)
    atms.lookupEnv(List(c, b)).map(i1_hORg.contains(_)).getOrElse(false)
      `should` be (true)

    val i1_hANDb = atms.interpretations(List(List(b), List(h)))
    atms.dbg(envL(i1_hANDb))
    i1_hANDb.length `should` be (1)
    atms.lookupEnv(List(c, b)).map(i1_hANDb.contains(_)).getOrElse(false)
      `should` be (true)

    val i1_hANDg = atms.interpretations(List(List(h), List(g)))
    atms.dbg(envL(i1_hANDg)) // Lisp gives NIL
    i1_hANDg.isEmpty `should` be (true)

    atms.dbg("Testing ATMS.isTrueNode")
    a.isTrueNode `should` be (false)
    c.isTrueNode `should` be (false)
    e.isTrueNode `should` be (false)
    g.isTrueNode `should` be (false)
    h.isTrueNode `should` be (false)
    b.isTrueNode `should` be (false)
    x.isTrueNode `should` be (false)
    val j5 = atms.justifyNode("R5", b, List())
    b.isTrueNode `should` be (true)
    c.isTrueNode `should` be (false)
    h.isTrueNode `should` be (false)
    val j6 = atms.justifyNode("R6", c, List())
    b.isTrueNode `should` be (true)
    c.isTrueNode `should` be (true)
    h.isTrueNode `should` be (true)
    atms.debugAtms
  }

  "FdK Test 1" `should` "all pass" in {
    val atms = new ATMS[Symbol, String, Nothing]("atms-test1", debugging = false)
    val a = atms.createNode("A")
    val b = atms.createNode("B")
    val c = atms.createNode("C")
    val d = atms.createNode("D")
    val e = atms.createNode("E")
    val f = atms.createNode("F")
    atms.debugAtms

    atms.assumeNode(a)
    atms.debugAtms

    atms.assumeNode(b)
    atms.debugAtms

    atms.justifyNode("J1", d, List(a, b))
    atms.debugAtms

    atms.justifyNode("J2", e, List(b, c))
    atms.debugAtms

    atms.assumeNode(c)
    atms.debugAtms

    atms.justifyNode("J3", f, List(d, e))
    atms.debugAtms

    atms.nogoodNodes("X1", List(d, e))
    atms.debugAtms
  }

}

/****

;;;; Test code
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(proclaim '(special a b c d e f))

(defun atms-test2 ()
  (justify-node 'simpler d (list a)))

(defun atms-test3 ()
  (nogood-nodes 'atms-test3 (list a b)))

;;; an example from de Kleer's paper by Gitchang

(proclaim '(special a b c x=1 y=x x=z y=1 z=1))

(defun step-1 ()
  (setq *atms* (create-atms "Step-1")
        a (tms-create-node *atms* "A")
        b (tms-create-node *atms* "B")
        c (tms-create-node *atms* "C")
        x=1 (tms-create-node *atms* "x=1")
        y=x (tms-create-node *atms* "y=x")
        x=z (tms-create-node *atms* "x=z")
        y=1 (tms-create-node *atms* "y=1")
        z=1 (tms-create-node *atms* "z=1") )
  (assume-node a)
  (assume-node b)
  (assume-node c)
  (justify-node 'j1 x=1 (list a))
  (justify-node 'j2 y=x (list b))
  (justify-node 'j3 x=z (list c))
  (why-nodes *atms*)
  (print-envs *atms*)

  (format t "~2%Now register nogood{A,B}")
  (nogood-nodes 'NOGOOD (list a b))
  (why-nodes *atms*)
  (print-envs *atms*)

  (format t "~2%x=1, y=x => y=1")
  (justify-node 'j4 y=1 (list x=1 y=x))
  (why-nodes *atms*)
  (print-envs *atms*)

  (format t "~2%We have a premise z=1")
  (justify-node 'Premise z=1 nil)
  (why-nodes *atms*)
  (print-envs *atms*)

  (format t "~2%z=1, x=z => x=1")
  (justify-node 'j5 x=1 (list z=1 x=z))
  (why-nodes *atms*)
  (print-envs *atms*) )


#| The result of the above program.

The Contradiction is out.
A is in, under (E-2)
B is in, under (E-3)
C is in, under (E-4)
x=1 is in, under (E-2)
y=x is in, under (E-3)
x=z is in, under (E-4)
y=1 is out.
z=1 is out.
E-1: {}
E-2: {A}
E-3: {B}
E-4: {C}

Now register nogood{A,B}
The Contradiction is out.
A is in, under (E-2)
B is in, under (E-3)
C is in, under (E-4)
x=1 is in, under (E-2)
y=x is in, under (E-3)
x=z is in, under (E-4)
y=1 is out.
z=1 is out.
E-1: {}
E-2: {A}
E-3: {B}
E-4: {C}
E-5:* {A, B}

x=1, y=x => y=1
The Contradiction is out.
A is in, under (E-2)
B is in, under (E-3)
C is in, under (E-4)
x=1 is in, under (E-2)
y=x is in, under (E-3)
x=z is in, under (E-4)
y=1 is out.
z=1 is out.
E-1: {}
E-2: {A}
E-3: {B}
E-4: {C}
E-5:* {A, B}

We have a premise z=1
The Contradiction is out.
A is in, under (E-2)
B is in, under (E-3)
C is in, under (E-4)
x=1 is in, under (E-2)
y=x is in, under (E-3)
x=z is in, under (E-4)
y=1 is out.
z=1 is in, under (E-1)
E-1: {}
E-2: {A}
E-3: {B}
E-4: {C}
E-5:* {A, B}

z=1, x=z => x=1
The Contradiction is out.
A is in, under (E-2)
B is in, under (E-3)
C is in, under (E-4)
x=1 is in, under (E-2 E-4)
y=x is in, under (E-3)
x=z is in, under (E-4)
y=1 is in, under (E-6)
z=1 is in, under (E-1)
E-1: {}
E-2: {A}
E-3: {B}
E-4: {C}
E-5:* {A, B}
E-6: {B, C}

|#

***/
