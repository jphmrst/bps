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

package org.maraist.truthmaintenancesystems.justificationbased
import scala.collection.mutable.{ListBuffer, HashSet, HashMap}

type Justification[D, I] = Just[D, I] | Symbol

class Just[D, I](
  val index: Int,
  val informant: I,
  val consequence: Node[D, I],
  val antecedents: ListBuffer[Node[D, I]]
) {
  // (defstruct (just (:PRINT-FUNCTION print-just))
  //   (index 0)
  //   informant
  //   consequence
  //   antecedents)

  override def toString: String = s"<Just $index>"
  def printJust: Unit = println(this.toString)
  // (defun print-just (just stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "#<Just ~D>" (just-index just)))

  def checkJustification: Boolean =
    consequence.isOutNode && isJustificationSatisfied
  // (defun check-justification (just)
  //   (and (out-node? (just-consequence just))
  //        (justification-satisfied? just)))

  def isJustificationSatisfied: Boolean = antecedents.forall(_.isInNode)
  // (defun justification-satisfied? (just)
  //   (every #'in-node? (just-antecedents just)))

  def getBlurb: String =
    s"($index) $informant ${consequence.datum} <= ${antecedents.map(_.datum).mkString(", ")}"
  def detailJust: Unit = println(getBlurb)

} // class Just
