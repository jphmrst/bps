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
import scala.collection.mutable.{HashSet, ListBuffer}

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
  // (defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
  //   (index 0)                                        ;; Unique name.
  //   (datum nil)                   ; Pointer to IE data structures.
  //   (label nil)                   ; minimal envs believed under
  //   (justs nil)                   ; providers of support
  //   (consequences nil)            ; provides support for.
  //   (contradictory? nil)          ; flag marking it as contradictory.
  //   (assumption? nil)             ; flag marking it as n assumption.
  //   (rules nil)                   ; run when label non-empty.
  //   (atms nil))

  // (defun tms-create-node (atms datum &key assumptionp contradictoryp
  //                                    &aux node)
  //   (setq node (make-tms-node :INDEX (incf (atms-node-counter atms))
  //                             :DATUM datum
  //                             :ASSUMPTION? assumptionp
  //                             :CONTRADICTORY? contradictoryp
  //                             :ATMS atms))
  //   (push node (atms-nodes atms))
  //   (if contradictoryp (push node (atms-contradictions atms)))
  //   (when assumptionp
  //     (push node (atms-assumptions atms))
  //     (push (create-env atms (list node)) (tms-node-label node)))
  //   node)

  /** Unique name. */
  val index = atms.nextNodeIndex

  /** Minimal envs believed under */
  val label: ListBuffer[Env[D]] = ListBuffer.empty[Env[D]]

  atms.nodes += this

  if isContradictory then atms.contradictions += this
  if isAssumption then {
    atms.assumptions += this
    label += new Env(atms, List(this))
  }

  /** Providers of support */
  var justs = {}
  /** Provides support for. */
  var consequences: List[TMSnode[D]] = List.empty
  /** Run when label non-empty. */
  var rules: List[D] = List.empty

  override def toString(): String =
    if isAssumption then s"A-$index" else s"#<NODE: ${atms.nodeString}>"
  // (defun print-tms-node (node stream ignore)
  //   (declare (ignore ignore))
  //   (if (tms-node-assumption? node)
  //       (format stream "A-~D" (tms-node-index node))
  //       (format stream "#<NODE: ~A>" (node-string node))))

  def nodeString: String = atms.nodeString(this)

  def assumptionOrder(that: TMSnode[D]): Boolean = index > that.index

  def isTrueNode: Boolean = label.head.isEmpty
  // (defun true-node? (node)
  //   (eq (car (tms-node-label node))
  //       (atms-empty-env (tms-node-atms node))))
  //

  def isInNode: Boolean = !label.isEmpty

  def isInNode(givenEnv: Env[D]): Boolean = label.exists(_.isSubset(givenEnv))
  // (defun in-node? (n &optional env)
  //   (if env
  //       (some #'(lambda (le) (subset-env? le env))
  //        (tms-node-label n))
  //       (not (null (tms-node-label n)))))

  def isOutNode(givenEnv: Env[D]): Boolean = !isInNode(givenEnv)
  // (defun out-node? (n env) (not (in-node? n env)))

  def inConsistentWith(givenEnv: Env[D]): Boolean =
    label.exists((env) => !(env + givenEnv).isNogood)
  // (defun node-consistent-with? (n env)
  //   (some #'(lambda (le) (not (env-nogood? (union-env le env))))
  //    (tms-node-label n)))

  def updateLabel(newEnvs: EnvList[D]): Unit = {
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

  def isWeave(nodes: List[TMSnode[D]]): Boolean = ???
  // (defun weave? (env nodes &aux new-env)
  //   (cond ((null nodes) t)
  //         (t (dolist (e (tms-node-label (car nodes)))
  //              (setq new-env (union-env e env))
  //              (unless (env-nogood? new-env)
  //                (if (weave? new-env (cdr nodes))
  //                    (return T)))))))

  def consEnv(env: Env[D]): Env[D] = ???
  // (defun cons-env (assumption env &aux nassumes)
  //   (setq nassumes (ordered-insert assumption
  //                                  (env-assumptions env)
  //                                  #'assumption-order))
  //   (or (lookup-env nassumes)
  //       (create-env (tms-node-atms assumption) nassumes)))

  // ;;; Generating explanations
  // ;;; This returns a list of justifications which form a DAG for the
  // ;;; derivation. This is quite complicated because this is really a
  // ;;; simple consequent JTMS.

  def explainNode(env: Env[D]): Unit = explainNode1(env, Nil, Nil)
  // (defun explain-node (node env) (explain-node-1 env node nil nil))

  def explainNode1(
    env: Env[D], queuedNodes: List[TMSnode[D]], explanation: List[Any]):
      Unit = ???
  // (defun explain-node-1 (env node queued-nodes explanation)
  //   (cond ((member node queued-nodes) nil)
  //         ((and (tms-node-isAssumption node)
  //               (member node (env-assumptions env)))
  //          (cons (cons 'ASSUME node) explanation))
  //         ((dolist (just explanation)
  //            (if (if (listp just)
  //                    (eq (cdr just) node) (eq (just-consequence just) node))
  //                (return explanation))))
  //         (t (setq queued-nodes (cons node queued-nodes))
  //            (dolist (just (tms-node-justs node))
  //              (unless (dolist (a (just-antecedents just))
  //                        (unless (isInNode a env) (return t)))
  //               (let ((new-explanation explanation))
  //                 (dolist (a (just-antecedents just)
  //                            (return-from explain-node-1 (cons just new-explanation)))
  //                   (setq new-explanation
  //                         (explain-node-1 env a queued-nodes new-explanation))
  //                   (unless new-explanation (return nil)))))))))

  // ;;; Printing

  def whyNode: Unit = ???
  // (defun why-node (node &optional (stream t) (prefix ""))
  //   (format stream "~%<~A~A,{" prefix (tms-node-datum node))
  //   (dolist (e (tms-node-label node))
  //     (env-string e stream))
  //   (format stream "}>"))

  def nodeJustification: Unit = ???
  // (defun node-justifications (node &optional (stream t))
  //   (format t "~% For ~A:" (nodeString node))
  //   (dolist (j (tms-node-justs node))
  //     (print-justification j stream)))
}
