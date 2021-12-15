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
    ListBuffer(atms.createEnv(List(this)))

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

  def isTrueNode: Boolean = {
    ???
  }
  // ; From atms.lisp
  // (defun true-node? (node)
  //   (eq (car (tms-node-label node))
  //       (atms-empty-env (tms-node-atms node))))

  def isInNode: Boolean = !label.isEmpty

  def isInNodeUnder(env: Env[D, I]): Boolean = label.exists(_.isSubsetEnv(env))
  // ; From atms.lisp
  // (defun in-node? (n &optional env)
  //   (if env
  //       (some #'(lambda (le) (subset-env? le env))
  //             (tms-node-label n))
  //       (not (null (tms-node-label n)))))

  def isOutNode(env: Env[D, I]): Boolean = !isInNodeUnder(env)
  // ; From atms.lisp
  // (defun out-node? (n env) (not (in-node? n env)))

  def isNodeConsistentWith(env: Env[D, I]): Boolean =
    label.exists((le) => !le.unionEnv(env).isNogood)
  // ; From atms.lisp
  // (defun node-consistent-with? (n env)
  //   (some #'(lambda (le) (not (env-nogood? (union-env le env))))
  //         (tms-node-label n)))

  /**
    * TODO Fill in method purpose.
    *
    * Note that the original list returned its argument, since
    * destructive updates to Lisp's lists might result in a change to
    * the first `cons` cell.  Since Scala [[ListBuffer]]s are wrapped,
    * this change to the overall list reference is not possible, and
    * so this method returns `Unit`.
    *
    * @param newEnvs A list of environments to be mutated by this call.
    * @return
    */
  def updateLabel(newEnvs: ListBuffer[Env[D, I]]): Unit = {
    var toRemoveFromNewEnvs = HashSet.empty[Env[D, I]]
    for (newEnv <- newEnvs) do {
      var addThisNewEnvToLabel = true
      var toRemoveFromLabel = HashSet.empty[Env[D, I]]
      for (labelEnv <- label) do {
        newEnv.compareEnv(labelEnv) match {
          case EnvCompare.Disjoint => { }
          case EnvCompare.EQ  => {
            toRemoveFromNewEnvs += newEnv
            addThisNewEnvToLabel = false
          }
          case EnvCompare.S21 => {
            toRemoveFromNewEnvs += newEnv
            addThisNewEnvToLabel = false
          }
          case EnvCompare.S12 => {
            toRemoveFromLabel += labelEnv
          }
        }
      }
      label --= toRemoveFromLabel
      if addThisNewEnvToLabel then label += newEnv
    }
    newEnvs --= toRemoveFromNewEnvs
    for (newEnv <- newEnvs) do newEnv.nodes += this
  }
  // ; From atms.lisp --- comments by JM
  // (defun update-label (node new-envs &aux envs)
  //   (setq envs (tms-node-label node))
  //
  //   ;; Outer loop: traverse the new-envs list.  In the loop,
  //   ;; new-envs points to the newxt CONS cell of the list.
  //   (do ((new-envs new-envs (cdr new-envs)))
  //       ((null new-envs))
  //
  //     ;; Inner loop: traverse the labels of this node.
  //     ;; - nenvs and envs both alias to cons cells in the label list.
  //     (do ((nenvs envs (cdr nenvs)))
  //         ;; The exit condition and (ignored, but side-effecting)
  //         ;; result value.
  //         ((null nenvs) (push (car new-envs) envs))
  //
  //       (cond
  //
  //         ;; Do nothing if either of the cons-cell traversers have
  //         ;; NIL at at their CAR.  Could arise from the RPLACAs, I
  //         ;; guess.
  //         ((null (car nenvs)))
  //         ((null (car new-envs)))
  //
  //         ;; The real case: compare the environments at the start
  //         ;; of each list traverser.
  //         ((case (compare-env (car new-envs) (car nenvs))
  //
  //            ;; If the same, or if the label's environment is a
  //            ;; subset, then null out the CAR of the scanner on the
  //            ;; NEW-ENVS.
  //            ((:EQ :S21) (rplaca new-envs nil))
  //
  //            ;; (1) Remove this node from the list of nodes in the Env
  //            ;; at the CAR of the scanner on the label's environments,
  //            ;; and (2) null out that CAR from the label's environments,
  //            (:S12 (setf (env-nodes (car nenvs))
  //                        (delete node (env-nodes (car nenvs))
  //                                :COUNT 1))
  //                  (rplaca nenvs nil)))))
  //
  //      ) ;; End of the inner DO-loop.
  //        ;; Here the end condition of that loop adds the current
  //        ;; new-env CAR to the node label, unless the former has
  //        ;; been nulled out.
  //    )
  //
  //   ;; After the loop: tidy the Env passed in as an argument by
  //   ;; removing all NILs, and push this Node onto the node-list
  //   ;; of each Env which remains.
  //   (setq new-envs (delete nil new-envs :TEST #'eq))
  //   (dolist (new-env new-envs) (push node (env-nodes new-env)))
  //
  //   ;; Tidy and update the LABEL list of environments.
  //   (setf (tms-node-label node) (delete nil envs :TEST #'eq))
  //
  //   ;; Return the updated version of the list we were passed in
  //   ;; (since side-effecting calls may have changed the top CONS
  //   ;; cell).
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

  def differsFrom(that: Option[Node[D, I]]): Boolean = that match {
    case None => true
    case Some(n) => this != n
  }

  // ;;; Printing

  def whyNode(prefix: String = "", firstPrefix: String = ""): Unit = {
    println(s"$firstPrefix$datum")
    label.length match {
      case 0 => println(s"${prefix}Empty label")
      case 1 => println(s"${prefix}Label environment: ${label(0).envString}")
      case n => {
        println(s"${prefix}Label environments ($n)")
        var e = 0
        label.map((env) => {
          e = e + 1
          println(s"$prefix$e. ${env.envString}")
        })
      }
    }
  }
  // ; From atms.lisp
  // (defun why-node (node &optional (stream t) (prefix ""))
  //   (format stream "~%<~A~A,{" prefix (tms-node-datum node))
  //   (dolist (e (tms-node-label node))
  //     (env-string e stream))
  //   (format stream "}>"))

  def debugNode: Unit = {
    println(s"- $datum")
    label.length match {
      case 0 => println("  Empty label")
      case 1 => println(s"  Label environment: ${label(0).envString}")
      case n => {
        println(s"  Label environments ($n)")
        var e = 0
        label.map((env) => {
          e = e + 1
          println(s"  $e. ${env.envString}")
        })
      }
    }
    consequences.length match {
      case 0 => println("  Antecedent to no justifications")
      case n =>
        println("  Antecedent to " +
          consequences.map(_.informant.toString).mkString(", "))
    }
  }

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
