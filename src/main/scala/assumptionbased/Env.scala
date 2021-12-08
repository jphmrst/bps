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


// Translated from KDF/JdK version 61 of 7/21/92.

package org.maraist.tms.atms
import scala.collection.mutable.ListBuffer

type EnvList[D] = ListBuffer[Option[Env[D]]]

class Env[D](
  val assumptions: List[TMSnode[D]],
  var index: Int = 0,
  var count: Int = 0
) {
  // (defstruct (env (:PREDICATE env?)
  //            (:PRINT-FUNCTION print-env-structure))
  //       (index 0)
  //       (count 0)                            ; Number of assumptions.
  //       (assumptions nil)
  //       (nodes nil)
  //       (nogood? nil)
  //       (rules nil))

  def this(atms: ATMS[D], assumptions: List[TMSnode[D]]) = this(assumptions)

  /** Number of assumptions. */

  var nodes: ListBuffer[TMSnode[D]] = ListBuffer.empty

  var isNogood: Boolean = false

  /** Call this if becomes nogood. */
  var rules = {}

  override def toString(): String = s"E-$index"
  // (defun print-env-structure (env stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "E-~D" (env-index env)))

  def envOrder(that: Env[?]): Boolean = index < that.index

  def isSubset(that: Env[D]): Boolean = ???

  def +(that: Env[D]): Env[D] = ???

  // Adding this for TMSnode.isTrueNode
  def isEmpty: Boolean = assumptions.isEmpty && nodes.isEmpty

  def isWeave(nodes: List[TMSnode[D]]): Boolean = ???
  // (defun weave? (env nodes &aux new-env)
  //   (cond ((null nodes) t)
  //         (t (dolist (e (tms-node-label (car nodes)))
  //              (setq new-env (union-env e env))
  //              (unless (env-nogood? new-env)
  //                (if (weave? new-env (cdr nodes))
  //                    (return T)))))))

  def isSupportingAntecedent(nodes: List[TMSnode[D]]): Boolean = ???
  // (defun supporting-antecedent? (nodes env)
  //   (dolist (node nodes t) (unless (isInNode node env) (return nil))))
  //

  def removeNode: Unit = ???
  // (defun remove-node (node &aux atms)
  //   (if (tms-node-consequences node)
  //       (error "Can't remove node with consequences"))
  //   (setq atms (tms-node-atms node))
  //   (setf (atms-nodes atms)
  //         (delete node (atms-nodes atms) :test #'eq :count 1))
  //   (dolist (just (tms-node-justs node))
  //     (dolist (ant (just-antecedents just))
  //       (setf (tms-node-consequences ant)
  //             (delete just (tms-node-consequences ant)
  //                     :test #'eq :count 1))))
  //   (dolist (env (tms-node-label node))
  //     (setf (env-nodes env)
  //           (delete node (env-nodes env) :test #'eq :count 1))))

  def union(that: Env[D]): Env[D] = ???
  // (defun union-env (e1 e2)
  //   (when (> (env-count e1)
  //            (env-count e2))
  //     (psetq e1 e2 e2 e1))
  //   (dolist (assume (env-assumptions e1))
  //     (setq e2 (cons-env assume e2))
  //     (if (env-nogood? e2) (return nil)))
  //   e2)

  def isSubsetEnv(e2: Env[D]): Boolean = ???
  // (defun subset-env? (e1 e2)
  //   (cond ((eq e1 e2) t)
  //         ((> (env-count e1)
  //             (env-count e2)) nil)
  //         ((subsetp (env-assumptions e1)
  //                   (env-assumptions e2)))))

  def compareEnv(e2: Env[D]) = ???
  // (defun compare-env (e1 e2)
  //   (cond ((eq e1 e2) :EQ)
  //         ((< (env-count e1) (env-count e2))
  //          (if (subsetp (env-assumptions e1)
  //                       (env-assumptions e2))
  //              :S12))
  //         ((subsetp (env-assumptions e2) (env-assumptions e1))
  //          :S21)))

  def printEnv: Unit = ???
  // (defun print-env (e &optional (stream t))
  //   (format stream "~%~A:~A"
  //           e (if (env-nogood? e)
  //                 "* " " "))
  //   (env-string e stream))

  def envString: Unit = ???
  // (defun env-string (e &optional stream
  //                      &aux assumptions strings printer)
  //   (setq assumptions (env-assumptions e))
  //   (when assumptions
  //     (setq printer (atms-nodeString (tms-node-atms (car assumptions)))))
  //   (dolist (a assumptions) (push (funcall printer a) strings))
  //   (format stream "{~{~A~^,~}}" (sort strings #'string-lessp)))
}
