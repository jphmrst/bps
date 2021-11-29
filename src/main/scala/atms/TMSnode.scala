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

package org.maraist.tms.atms
import scala.util.control.NonLocalReturns.*
import scala.collection.mutable.HashSet

// Definitions

/**
  *
  *
  * @param atms
  * @param datum Pointer to IE data structures.
  * @param isContradictory Flag marking it as contradictory.
  * @param isAssumption Flag marking it as n assumption.
  */
class TMSnode[D](
  val atms: ATMS[D],
  val datum: D | Contra,
  val isContradictory: Boolean = false,
  var isAssumption: Boolean = false
) {
  /** Unique name. */
  val index = atms.nextNodeIndex

  /** Minimal envs believed under */
  val label: HashSet[Env[D]] = new HashSet[Env[D]]

  atms.nodes += this

  if isContradictory then atms.contradictions += this
  if isAssumption then {
    atms.assumptions += this
    label += new Env(atms, List(this))
  }

  /** Providers of support */
  var justs = {}
  /** Provides support for. */
  var consequences = {}
  /** Run when label non-empty. */
  var rules = {}

  override def toString(): String =
    if isAssumption then s"A-$index" else s"#<NODE: ${atms.nodeString}>"

  def nodeString: String = atms.nodeString(this)

  def assumptionOrder(that: TMSnode[D]): Boolean = index > that.index

  def isInNode: Boolean = !label.isEmpty

  def isInNode(givenEnv: Env[D]): Boolean = label.exists(_.isSubset(givenEnv))

  def isOutNode(givenEnv: Env[D]): Boolean = !isInNode(givenEnv)

  def inConsistentWith(givenEnv: Env[D]): Boolean =
    label.exists((env) => !(env + givenEnv).isNogood)


  def updateLabel(newEnvs: List[Env[D]]): List[Env[D]] = {
    ???
  /*
(defun update-label (node new-envs &aux envs)
  (setq envs (tms-node-label node))
  (do ((new-envs new-envs (cdr new-envs)))
      ((null new-envs))
    (do ((nenvs envs (cdr nenvs)))
        ((null nenvs) (push (car new-envs) envs))
      (cond ((null (car nenvs)))
            ((null (car new-envs)))
            ((case (compare-env (car new-envs) (car nenvs))
               ((:EQ :S21) (rplaca new-envs nil))
               (:S12 (setf (env-nodes (car nenvs))
                           (delete node (env-nodes (car nenvs))
                                   :COUNT 1))
                     (rplaca nenvs nil)))))))
  (setq new-envs (delete nil new-envs :TEST #'eq))
  (dolist (new-env new-envs) (push node (env-nodes new-env)))
  (setf (tms-node-label node) (delete nil envs :TEST #'eq))
  new-envs)
  */
  }
}
