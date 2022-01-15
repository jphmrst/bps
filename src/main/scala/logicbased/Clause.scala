// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University
// and Johan de Kleer, the Xerox Corporation.
// Copyright (C) 2022 John Maraist.
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

package org.maraist.truthmaintenancesystems.logicbased
import scala.util.control.NonLocalReturns.*
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}
import java.io.PrintStream

/**
  *
  * **Arguments and `val` members translated from**:
  * <pre>
(defstruct (clause (:PRINT-FUNCTION print-clause))
  (index 0)       ; Unique namer
  (informant nil)
  (literals nil)  ; a list of (<node> . <truth>)
  (pvs 0)         ; Number of terms which potentially violate it.
  (length 0)      ; Number of literals.
  (sats 0)        ; Number of terms which satisfy it.
  (status nil))   ; :SUBSUMED | :QUEUED | :DIRTY | :NOT-INDEXED | nil
</pre>
  *
  * @groupname interface Interface methods
  * @groupdesc interface Top-level methods for control of the LTMS
  * from an external system.
  * @groupprio interface 1
  *
  * @groupname diagnostic Diagnostic and debugging methods
  * @groupdesc diagnostic Reporting the current LTMS state as text.
  * @groupprio diagnostic 2
  *
  * @groupname internal Internal methods
  * @groupdesc internal Implementation methods; not generally for use
  * from outside this package.
  * @groupprio internal 10
  */
class Clause[D, I, R](
  val index: Int
) {

  /**
    *
    *
    * **Translated from**:
    * <pre>
(defun print-clause (clause stream ignore)
;; From ltms.lisp
   (declare (ignore ignore))
   (format stream "#<Clause ~D>" (clause-index clause)))
</pre>
    *
    */
  def printClause(stream: PrintStream = Console.out): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defmacro satisfied-clause? (clause) `(> (clause-sats ,clause) 0))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defmacro violated-clause? (clause) `(= (clause-pvs ,clause) 0))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun simplify-clause (literals)
  (setq literals (sort-clause literals))
  (do ((tail literals next)
       (next (cdr literals) (cdr next)))
      ((null next) literals)
    (cond ((not (eq (caar tail) (caar next))))
          ((not (eq (cdar tail) (cdar next)))
           (return-from simplify-clause :TRUE))
          (t (rplacd tail (cdr next))))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun sort-clause (literals)
  (sort (copy-list literals) ;; Avoids shared structure bugs.
     #'< :KEY #'(lambda (n) (tms-node-index (car n)))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun find-unknown-pair (clause)
  (dolist (term-pair (clause-literals clause))
    (if (unknown-node? (car term-pair)) (return term-pair))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun clause-consequent (clause)
  (dolist (term-pair (clause-literals clause))
    (when (eq (tms-node-label (car term-pair)) (cdr term-pair))
      (return (if (eq clause (tms-node-support (car term-pair)))
                  (car term-pair))))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun assumptions-of-clause (in-clause &aux)
  (do ((clause-queue (list in-clause)
                     (nconc (cdr clause-queue) new-clauses))
       (mark (list nil))
       (node nil)
       (new-clauses nil nil)
       (assumptions nil))
      ((null clause-queue) assumptions)
    (dolist (term-pair (clause-literals (car clause-queue)))
      (setq node (car term-pair))
      (unless (eq (tms-node-mark node) mark)
        (unless (eq (tms-node-label node) (cdr term-pair))
          (cond ((eq :ENABLED-ASSUMPTION (tms-node-support node))
                 (push node assumptions))
                ((null (tms-node-support node)) (ltms-error "Node is unknown" node))
                (t (push (tms-node-support node) new-clauses))))
        (setf (tms-node-mark node) mark)))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun clause-antecedents (clause &aux result)
  (dolist (pair (clause-literals clause) result)
    (unless (eq (tms-node-support (car pair)) clause)
      (push (car pair) result))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun pretty-print-clause (clause)
  (format T "(:OR")
  (dolist (literal (clause-literals clause))
    (format T " ~:[(:NOT ~A)~;~A~]"
            (eq :TRUE (cdr literal)) (node-string (car literal))))
  (format T ")"))
</pre>
    *
    */

} // class Clause
