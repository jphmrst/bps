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

type Justification[D, I] = Just[D, I] | Symbol

val justifyNodeAssumed: Symbol = Symbol("assume-node")
val justifyMakeContradiction: Symbol = Symbol("make-contradiction")

/**
  *
  *
  *
  * **Arguments and value members translated from**:
  * <pre>
  // ; From atms.lisp
  // (defstruct (just (:PRINT-FUNCTION print-just))
  //            (index 0)
  //            (informant nil)
  //            (consequence nil)
  //            (antecedents nil))
</pre>
  * @param index
  * @param informant
  * @param consequence
  * @param antecedents
  */
class Just[D, I](
  val index: Int,
  val informant: I,
  val consequence: Node[D, I],
  val antecedents: ListBuffer[Node[D, I]]
) {


  /**
    *
    */
  def blurb: String = s"[${informant.toString}.$index] ${consequence.datum.toString} <= ${antecedents.map(_.datum.toString).mkString(", ")}"

  /**
    *
    */
  override def toString: String = s"<${informant.toString} $index>"

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
(defun print-just (just stream ignore)
  (declare (ignore ignore))
  (format stream "<~A ~D>" (just-informant just)
          (just-index just)))
</pre>
    */
  def printJust: Unit = println(toString)

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun print-justification (j &optional (stream t))
  (format stream "~%  ~A, " (just-informant j))
  (dolist (a (just-antecedents j))
    (why-node a stream "     ")))
</pre>
    */
  def printJustification: Unit = {
    println(s"  $informant $index")
    for (a <- antecedents) do a.whyNode
  }
}
