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

class EnvTable[D, I] extends HashMap[Int, ListBuffer[Env[D, I]]] {

  def printEnvTable: Unit = {
    ???
  }
  // ; From ainter.lisp
  // (defun print-env-table (table stream)
  //   (dolist (bucket table)
  //     (dolist (env (cdr bucket))
  //       (print-env env stream))))

  def insertInTable(env: Env[D, I]): Unit = {
    val count = env.count
    this.get(count) match {
      case None => this(count) = ListBuffer(env)
      case Some(entry) => entry += env
    }
  }
  // ; From ainter.lisp
  // (defun insert-in-table (table env &aux count entry)
  //   (setq count (env-count env)
  //         entry (assoc count table :TEST #'=))
  //   (cond (entry (setf (cdr entry) (cons env (cdr entry))) table)
  //         (t (ordered-insert
  //              (list count env) table
  //              #'(lambda (entry1 entry2)
  //                  (< (car entry1) (car entry2)))))))
}

enum EnvCompare {
  case S12 extends EnvCompare
  case S21 extends EnvCompare
  case EQ extends EnvCompare
}

class Env[D, I](
  val index: Int,
  val assumptions: ListBuffer[Node[D, I]]
) {

  /** Number of assumptions. */
  val count = assumptions.length

  /** If this node is nogood, stores the evidence. */
  var nogoodEvidence: Option[Justification[D, I] | Env[D, I]] = None
  inline def isNogood = !nogoodEvidence.isEmpty

  val nodes: ListBuffer[Node[D, I]] = ListBuffer.empty

  /** Call this if becomes nogood */
  val rules: ListBuffer[Rule[I]] = ListBuffer.empty

  // ; From atms.lisp
  // (defstruct (env (:PREDICATE env?)
  //                 (:PRINT-FUNCTION print-env-structure))
  //            (index 0)
  //            (count 0)                    ; Number of assumptions.
  //            (assumptions nil)
  //            (nodes nil)
  //            (nogood? nil)
  //            (rules nil))                 ; Call this if becomes nogood.
  //
  // (defun print-env-structure (env stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "E-~D" (env-index env)))

  def isWeave(nodes: Iterable[Node[D, I]]): Boolean = {
    ???
  }
  // ; From atms.lisp
  // (defun weave? (env nodes &aux new-env)
  //   (cond ((null nodes) t)
  //         (t (dolist (e (tms-node-label (car nodes)))
  //              (setq new-env (union-env e env))
  //              (unless (env-nogood? new-env)
  //                (if (weave? new-env (cdr nodes))
  //                    (return T)))))))

  def envOrder(e2: Env[D, I]): Boolean = {
    ???
  }
  // ; From ainter.lisp
  // (defun env-order (e1 e2)
  //   (< (env-index e1) (env-index e2)))

  def unionEnv(e2: Env[D, I]): Env[D, I] = {
    ???
  }
  // ; From ainter.lisp
  // (defun union-env (e1 e2)
  //   (when (> (env-count e1)
  //            (env-count e2))
  //     (psetq e1 e2 e2 e1))
  //   (dolist (assume (env-assumptions e1))
  //     (setq e2 (cons-env assume e2))
  //     (if (env-nogood? e2) (return nil)))
  //   e2)

  def consEnv(assumption: Node[D, I]): Env[D, I] = {
    ???
  }
  // ; From ainter.lisp
  // (defun cons-env (assumption env &aux nassumes)
  //   (setq nassumes (ordered-insert assumption
  //                                  (env-assumptions env)
  //                                  #'assumption-order))
  //   (or (lookup-env nassumes)
  //       (create-env (tms-node-atms assumption) nassumes)))

  def isSubsetEnv(e2: Env[D, I]): Boolean = {
    ???
  }
  // ; From ainter.lisp
  // (defun subset-env? (e1 e2)
  //   (cond ((eq e1 e2) t)
  //         ((> (env-count e1)
  //             (env-count e2)) nil)
  //         ((subsetp (env-assumptions e1)
  //                   (env-assumptions e2)))))

  def compareEnv(e2: Env[D, I]): EnvCompare = {
    ???
  }
  // ; From ainter.lisp
  // (defun compare-env (e1 e2)
  //   (cond ((eq e1 e2) :EQ)
  //         ((< (env-count e1) (env-count e2))
  //          (if (subsetp (env-assumptions e1)
  //                       (env-assumptions e2))
  //              :S12))
  //         ((subsetp (env-assumptions e2) (env-assumptions e1))
  //          :S21)))

  def isSupportingAntecedent(nodes: Iterable[Node[D, I]]): Boolean = {
    ???
  }
  // ; From ainter.lisp
  // (defun supporting-antecedent? (nodes env)
  //   (dolist (node nodes t) (unless (in-node? node env) (return nil))))

  def printEnv: Unit = {
    ???
  }
  // ; From ainter.lisp
  // (defun print-env (e &optional (stream t))
  //   (format stream "~%~A:~A"
  //           e (if (env-nogood? e)
  //                 "* " " "))
  //   (env-string e stream))

  def envString: String = {
    ???
  }
  // ; From ainter.lisp
  // (defun env-string (e &optional stream
  //                      &aux assumptions strings printer)
  //   (setq assumptions (env-assumptions e))
  //   (when assumptions
  //     (setq printer (atms-node-string (tms-node-atms (car assumptions)))))
  //   (dolist (a assumptions) (push (funcall printer a) strings))
  //   (format stream "{~{~A~^,~}}" (sort strings #'string-lessp)))
}

// ; From ainter.lisp
// (defun print-table (msg table)
//   (format t msg)
//   (dolist (entry table)
//     (format t "~%   Length ~D, ~D" (car entry)
//             (length (cdr entry)))))
