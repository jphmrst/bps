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
  * All possible justifications for a belief which the TMS might
  * record.
  */
type Justification[D, I, R] =
  Just[D, I, R] | NodeAssumed[D, I, R] | MakeContradiction[D, I, R]

/**
  * Justifications for a belief which the TMS might report as part of
  * an explanation for why a node might be believed.
  */
type Explanation[D, I, R] =
  Just[D, I, R] | NodeAssumed[D, I, R]

/**
  * Common supertype for belief justifications stipulated from outside
  * of the ATMS.
  */
sealed trait Stipulated[D, I, R]

/**
  * Stipulation that a node should be assumed believable by the ATMS.
  *
  * @param node
  */
case class NodeAssumed[D, I, R](node: Node[D, I, R]) extends Stipulated[D, I, R]

/**
  * Stipulation that an environment or a list of nodes should be taken
  * as contradictory by the ATMS.
  */
case class MakeContradiction[D, I, R]() extends Stipulated[D, I, R]

/**
  * Representation of the inference of one node from belief in all of
  * a set other nodes.
  *
  * **Arguments and value members translated from**:
  * <pre>
; From atms.lisp
(defstruct (just (:PRINT-FUNCTION print-just))
           (index 0)
           (informant nil)
           (consequence nil)
           (antecedents nil))
</pre>
  *
  * @param index Internal assigned index for this structure.
  * @param informant Value associated with this inference rule by the
  * external system.
  * @param consequence Node whose belief is supported by this rule.
  * @param antecedents Nodes whose belief is required for this rule
  * to apply.
  *
  * @groupname construction Construction methods
  * @groupdesc construction API methods for building and changing
  * an ATMS from an external system.
  * @groupprio construction 1
  *
  * @groupname query Query methods
  * @groupdesc query API methods for querying the ATMS and its beliefs
  * from an external system.
  * @groupprio query 2
  *
  * @groupname diagnostic Diagnostic and debugging methods
  * @groupdesc diagnostic Reporting the current JTMS state as text.
  * @groupprio diagnostic 3
  *
  * @groupname internal Internal methods
  * @groupdesc internal Implementation methods; not generally for use
  * from outside this package.
  * @groupprio internal 10
  */
class Just[D, I, R](
  val index: Int,
  val informant: I,
  val consequence: Node[D, I, R],
  val antecedents: List[Node[D, I, R]]
) {


  /**
    * Returns the single-line representation of this rule.
    *
    * @group diagnostic
    */
  def blurb: String = s"[${informant.toString}.$index] ${consequence.datum.toString} <= ${antecedents.map(_.datum.toString).mkString(", ")}"

  /**
    * Returns a short tag for this rule.
    * @group diagnostic
    */
  override def toString: String = s"<${informant.toString} $index>"

  /**
    * Prints a short tag for this rule.
    *
    * **Translated from**:
    * <pre>
(defun print-just (just stream ignore)
  (declare (ignore ignore))
  (format stream "<~A ~D>" (just-informant just)
          (just-index just)))
</pre>
    *
    * @group diagnostic
    */
  def printJust: Unit = println(toString)

  /**
    * Prints verbose details about this rule, including why each
    * antecedent might be believed.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun print-justification (j &optional (stream t))
  (format stream "~%  ~A, " (just-informant j))
  (dolist (a (just-antecedents j))
    (why-node a stream "     ")))
</pre>
    *
    * @group diagnostic
    */
  def printJustification: Unit = {
    println(s"  $informant $index")
    for (a <- antecedents) do a.whyNode
  }
}
