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

class Node[D, I](
  val atms: ATMS[D, I],
  val datum: D | String,
  var isAssumption: Boolean = false,
  var isContradictory: Boolean = false
) {

  val index: Int = atms.incrNodeCounter

  /** The minimal envs under which this node is believed. */
  val label: ListBuffer[Env[D, I]] =
    ListBuffer(atms.createEnv(ListBuffer(this)))

  /** What this node provides support for. */
  val consequences: ListBuffer[Just[D, I]] = ListBuffer.empty

  /** Providers of support. */
  val justs: ListBuffer[Just[D, I]] = ListBuffer.empty

  /** Run when label non-empty. */
  val rules: ListBuffer[Rule[I]] = ListBuffer.empty

  // ; From atms.lisp
  // (defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
  //   (index 0)                                             ;; Unique name.
  //   (datum nil)                   ; Pointer to IE data structures.
  //   (label nil)                   ; minimal envs believed under
  //   (justs nil)                   ; providers of support
  //   (consequences nil)            ; provides support for.
  //   (contradictory? nil)          ; flag marking it as contradictory.
  //   (assumption? nil)             ; flag marking it as n assumption.
  //   (rules nil)                   ; run when label non-empty.
  //   (atms nil))
  //
  // (defun print-tms-node (node stream ignore)
  //   (declare (ignore ignore))
  //   (if (tms-node-assumption? node)
  //       (format stream "A-~D" (tms-node-index node))
  //       (format stream "#<NODE: ~A>" (node-string node))))

  def nodeString: String = atms.nodeString(this)
  // ; From atms.lisp
  // (defun node-string (node)
  //   (funcall (atms-node-string (tms-node-atms node)) node))

  def defaultNodeString: String = datum.toString
  // ; From atms.lisp
  // (defun default-node-string (n) (format nil "~A" (tms-node-datum n)))

  def assumptionOrder(a2: Node[D, I]): Boolean = index < a2.index
  // ; From atms.lisp
  // (defun assumption-order (a1 a2)
  //   (< (tms-node-index a1) (tms-node-index a2)))

  def isTrueNode: Boolean = {
    ???
  }
  // ; From atms.lisp
  // (defun true-node? (node)
  //   (eq (car (tms-node-label node))
  //       (atms-empty-env (tms-node-atms node))))

  def isInNode: Boolean = {
    ???
  }
  // ; From atms.lisp
  // (defun in-node? (n &optional env)
  //   (if env
  //       (some #'(lambda (le) (subset-env? le env))
  //             (tms-node-label n))
  //       (not (null (tms-node-label n)))))

  def isOutNode: Boolean = {
    ???
  }
  // ; From atms.lisp
  // (defun out-node? (n env) (not (in-node? n env)))

  def isNodeConsistentWith(env: Env[D, I]): Boolean = {
    ???
  }
  // ; From atms.lisp
  // (defun node-consistent-with? (n env)
  //   (some #'(lambda (le) (not (env-nogood? (union-env le env))))
  //         (tms-node-label n)))

  /**
    *
    *
    * @param newEnvs A list of environments to be mutated by this call.
    * @return
    */
  def updateLabel(newEnvs: ListBuffer[Env[D, I]]): ListBuffer[Env[D, I]] = {
    ???
  }
  // ; From atms.lisp
  // (defun update-label (node new-envs &aux envs)
  //   (setq envs (tms-node-label node))
  //   (do ((new-envs new-envs (cdr new-envs)))
  //       ((null new-envs))
  //     (do ((nenvs envs (cdr nenvs)))
  //         ((null nenvs) (push (car new-envs) envs))
  //       (cond ((null (car nenvs)))
  //             ((null (car new-envs)))
  //             ((case (compare-env (car new-envs) (car nenvs))
  //                ((:EQ :S21) (rplaca new-envs nil))
  //                (:S12 (setf (env-nodes (car nenvs))
  //                            (delete node (env-nodes (car nenvs))
  //                                    :COUNT 1))
  //                      (rplaca nenvs nil)))))))
  //   (setq new-envs (delete nil new-envs :TEST #'eq))
  //   (dolist (new-env new-envs) (push node (env-nodes new-env)))
  //   (setf (tms-node-label node) (delete nil envs :TEST #'eq))
  //   new-envs)

  def findOrMakeEnv(assumptions: ListBuffer[Node[D, I]]): Env[D, I] = {
    ???
  }
  // ; From atms.lisp
  // (defun find-or-make-env (assumptions atms)
  //   (unless assumptions
  //     (return-from find-or-make-env (atms-empty-env atms)))
  //   ;; Presumes the list of assumptions is ordered properly
  //   (or (lookup-env assumptions)
  //       (create-env atms assumptions)))

  // ; From atms.lisp
  // ;;; Generating explanations
  // ;;; This returns a list of justifications which form a DAG for the
  // ;;; derivation. This is quite complicated because this is really a
  // ;;; simple consequent JTMS.

  def explainNode(env: Env[D, I]): Env[D, I] = {
    ???
  }
  // ; From atms.lisp
  // (defun explain-node (node env) (explain-node-1 env node nil nil))

  def explainNode1(
    env: Env[D, I],
    node: Node[D, I],
    queuedNodes: List[Node[D, I]],
    explanation: Just[D, I]):
      Env[D, I] = {
    ???
  }
  // ; From atms.lisp
  // (defun explain-node-1 (env node queued-nodes explanation)
  //   (cond
  //     ((member node queued-nodes) nil)
  //     ((and (tms-node-assumption? node)
  //           (member node (env-assumptions env)))
  //      (cons (cons 'ASSUME node) explanation))
  //     ((dolist (just explanation)
  //        (if (if (listp just)
  //                (eq (cdr just) node)
  //                (eq (just-consequence just) node))
  //            (return explanation))))
  //     (t (setq queued-nodes (cons node queued-nodes))
  //        (dolist (just (tms-node-justs node))
  //          (unless (dolist (a (just-antecedents just))
  //                    (unless (in-node? a env) (return t)))
  //            (let ((new-explanation explanation))
  //              (dolist (a (just-antecedents just)
  //                         (return-from explain-node-1
  //                           (cons just new-explanation)))
  //                (setq new-explanation
  //                      (explain-node-1 env a queued-nodes new-explanation))
  //                (unless new-explanation (return nil)))))))))

  // ;;; Printing

  def whyNode: Unit = {
    ???
  }
  // ; From atms.lisp
  // (defun why-node (node &optional (stream t) (prefix ""))
  //   (format stream "~%<~A~A,{" prefix (tms-node-datum node))
  //   (dolist (e (tms-node-label node))
  //     (env-string e stream))
  //   (format stream "}>"))

  def nodeJustifications: Unit = {
    ???
  }
  // ; From atms.lisp
  // (defun node-justifications (node &optional (stream t))
  //   (format t "~% For ~A:" (node-string node))
  //   (dolist (j (tms-node-justs node))
  //     (print-justification j stream)))

  // ; From adata.lisp --- not translating; expand in place at call sites
  //
  // (defun view-node (node)
  //   (datum-lisp-form (tms-node-datum node)))
  //
  // (defun stringify-node (node)
  //   (format nil "~A" (view-node node)))
}
