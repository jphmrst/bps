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
import scala.collection.mutable.{HashSet, HashMap}

/**
  * Forms of data which be signal support for a node.
  *
  * @tparam D Type of data associated with each node of a [[JTMS]].
  *
  * @tparam I Type of informants in the external system.
  *
  * @tparam R Type of rules which may be associated with each node of
  * a [[JTMS]].
  */
type Justification[D, I, R] =
  Just[D, I, R] | EnabledAssumption | UserStipulation

/**
  * Representation of node belief support when the node is believed as
  * an assumption by the external system.
  */
class EnabledAssumption private ()
/**
  * Singleton representation of node belief support when the node is
  * believed as an assumption by the external system.
  */
object EnabledAssumption extends EnabledAssumption {
  /**
    * Universal match to the singleton instance of this evidence
    * class.
    */
  def unapply(e: EnabledAssumption): Some[Unit] = Some(())
}

/**
  * Representation of node belief support following the stipulation of
  * an external system.
  */
class UserStipulation private ()
/**
  * Singleton representation of node belief support when the node is
  * believed as an assumption by the external system.
  */
object UserStipulation extends UserStipulation {
  /**
    * Universal match to the singleton instance of this evidence
    * class.
    */
  def unapply(e: UserStipulation): Some[Unit] = Some(())
}

/** Representation of a justification allowing belief in some [[Node]]
  * of the [[JTMS]] given the belief in other nodes.
  *
  * @param index Internal unique identifier, different from the other
  * `Just` records of this `JTMS`.
  * @param informant The informant assigned by the external system to
  * this justification.
  * @param consequence The node supported by this justification.
  * @param antecedents The nodes required for this justification to
  * apply.
  *
  * @tparam D Type of data associated with each [[Node]] of this
  * [[JTMS]].
  * @tparam I Type of informants in the external system.
  * @tparam R Type of rules which may be associated with each [[Node]]
  * of this [[JTMS]].
  */
class Just[D, I, R](
  val index: Int,
  val informant: I,
  val consequence: Node[D, I, R],
  val antecedents: List[Node[D, I, R]]
) {
  // (defstruct (just (:PRINT-FUNCTION print-just))
  //   (index 0)
  //   informant
  //   consequence
  //   antecedents)

  /**
    * Return the short tag for this justification.
    */
  override def toString: String = s"<Just $index>"

  /**
    * Output a brief tag for this justification.
    */
  def printJust: Unit = println(this.toString)
  // (defun print-just (just stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "#<Just ~D>" (just-index just)))

  /** Detect the case when this justification thinks it is satisfied,
    * but the [[JTMS]] has tagged its `consequence` as `out`.
    */
  def checkJustification: Boolean =
    consequence.isOutNode && isJustificationSatisfied
  // (defun check-justification (just)
  //   (and (out-node? (just-consequence just))
  //        (justification-satisfied? just)))

  /**
    * Returns `true` when all of the antecedents of this justification
    * are believed by the [[JTMS]].
    */
  def isJustificationSatisfied: Boolean = antecedents.forall(_.isInNode)
  // (defun justification-satisfied? (just)
  //   (every #'in-node? (just-antecedents just)))

  /**
    * Return a string detailing this justification.
    */
  def getBlurb: String =
    s"($index) $informant ${consequence.datum} <= ${antecedents.map(_.datum).mkString(", ")}"

  /**
    * Print a string detailing this justification.
    */
  def detailJust: Unit = println(getBlurb)

} // class Just
