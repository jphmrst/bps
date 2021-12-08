// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University
// and Johan de Kleer, the Xerox Corporation.
// Copyright (C) 2021 John Maraist.
// All rights reserved.

// See the LICENSE.txt and README-forbus-dekleer.txt files distributed
// with this work for a paragraph stating scope of permission and
// disclaimer of warranty, and for additional information regarding
// copyright ownership.  The above copyright notice and that paragraph
// must be included in any separate copy of this file.

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

class Rule[D] {
  // ; From ainter.lisp
  // (defstruct (rule (:PRINT-FUNCTION (lambda (r st ignore)
  //                                  (declare (ignore ignore))
  //                                  (format st "<Rule ~D>"
  //                                          (rule-counter r)))))
  //   counter      ; Unique ID for easy lookup
  //   atre         ; The ATRE it is part of
  //   dbclass        ; Dbclass of associated pattern
  //   matcher      ; Procedure that performs the match
  //   body         ; Procedure that does the rules' work
  //   in-nodes     ; Must have a jointly non-empty label
  //   imp-nodes)   ; Must be implied by the focus
}

class RuleEngine[F, I]() {

  // ; From ainter.lisp
  // (defstruct (atre (:PREDICATE atre?)
  //               (:PRINT-FUNCTION print-atre))
  //   title                   ; Pretty name
  //   atms                    ; Pointer to its ATMS
  //   (dbclasses nil)           ; List of dbclasses
  //   (dbclass-table nil)       ; Quick index into dbclasses
  //   (datum-counter 0)       ; Unique ID for asserts
  //   (rules nil)             ; Index for rules
  //   (rule-counter 0)        ; Unique ID for rules
  //   (debugging nil)         ; Show basic operations
  //   (queue nil)             ; General queue
  //   (rules-run 0)           ; Statistics
  //   (in-rules nil)          ; in-rules to be executed
  //   (focus nil)             ; State of the search, if any.
  //   (contradiction-rules nil) ; As in Focus paper (AAAI-88)
  //   (imp-rules nil))   ; Ibid.
  //
  // (defun create-atre (title &key debugging)
  //  (let ((j (make-atre
  //         :TITLE title
  //         :ATMS (create-atms (list :ATMS-OF title)
  //                            :NODE-STRING 'stringify-node)
  //         :DBCLASS-TABLE (make-hash-table :TEST #'eq)
  //         :DEBUGGING debugging))
  //        (false nil))
  //    (in-atre j)
  //    (change-atms (atre-atms j)
  //              :ENQUEUE-PROCEDURE
  //              #'(lambda (pair) (enqueue pair j)))
  //    ;; Create a default contradiction
  //    (setq false (make-datum :COUNTER (incf (atre-datum-counter j))
  //                         :ATRE j :LISP-FORM 'FALSE
  //                         :DBCLASS (get-dbclass 'FALSE)))
  //    (setf (datum-tms-node false) (atms-contra-node (atre-atms j)))
  //    (setf (tms-node-datum (datum-tms-node false)) false)
  //    (push false (dbclass-facts (datum-dbclass false)))
  //    j))
  //
  // (defun change-atre (atre &key (debugging nil debugging?))
  //   (if debugging? (setf (atre-debugging atre) debugging)))

  // (defun print-atre (j st ignore) (declare (ignore ignore))
  //   (format st "<ATRE: ~A>" (atre-title j)))

  // (defmacro debugging-atre (msg &rest args)
  //   `(when (atre-debugging *atre*) (format t ,msg  ,@args)))

  // ; From ainter.lisp
  // (defun run (&optional (atre *ATRE*)) ;; Toplevel driver function
  //     (format T "~%>>")
  //     (do ((form (read-form) (read-form)))
  //         ((member form '(quit stop exit abort)) nil)
  //         (format t "~%~A" (eval form))
  //         (run-rules atre)
  //         (format t "~%>>")))

  // ; From ainter.lisp
  // (defun run-forms (forms &optional (atre *ATRE*))
  //   (dolist (form forms) (eval form) (run-rules atre)))

  // ; From ainter.lisp
  // (defun show (&optional (atre *ATRE*) (stream *standard-output*))
  //   (format stream "For ATRE ~A:~% Focus = ~A."
  //        (atre-title atre)
  //        (if (env? (atre-focus atre)) (atre-focus atre)
  //          "empty"))
  //   (show-data atre stream) (show-rules atre stream))

  // ; From ainter.lisp
  // (defun solutions (atre choice-sets)
  //   (interpretations
  //    (atre-atms atre)
  //    (mapcar #'(lambda (choice-set)
  //             (mapcar #'(lambda (f) (get-tms-node f atre))
  //                     choice-set))
  //         choice-sets)))

  // ;;;; Implied-by rules

  // ;; The rule expansion code sets up the necessary tests for
  // ;; seeing if the antecedent nodes are implied by the current
  // ;; focus when the rule is on the queue.  Here we just
  // ;; re-queue the implied-by rules which were not in the scope
  // ;; of the previous focus for re-examination.

  // ; From ainter.lisp
  // (defun change-focus (env &optional (atre *atre*))
  //   (unless (atre? atre) ;; Users do slip, sometimes
  //     (error "Must change the focus of some ATRE, not ~A." atre))
  //   (when (and (env? env)
  //           (not (env-nogood? env)))
  //     (setf (atre-focus atre) env) ;; change focus
  //     (setf (atre-queue atre) ;; re-queue implied-by rules
  //        (nconc (atre-queue atre) (atre-imp-rules atre)))
  //     (setf (atre-imp-rules atre) nil)
  //     env)) ;; return new focus to indicate switch

  // ; From ainter.lisp
  // (defun focus-okay? (atre)
  //   (and (atre-focus atre)
  //        (not (env-nogood? (atre-focus atre)))))

  // ; From ainter.lisp
  // (defmacro with-focus (focus atre &rest forms)
  //   `(let ((old-focus (atre-focus ,atre)))
  //      (unwind-protect (progn (change-focus ,focus ,atre)
  //                          ,@ forms)
  //        (change-focus old-focus ,atre))))

  // ;; Interface to contradiction rules in ATMS

  // ; From ainter.lisp
  // (defun contradiction-rule (env proc atre)
  //   (cond ((env-nogood? env)
  //       (enqueue (list proc (list env) nil) atre))
  //      (t (push (list proc (list env) nil) (env-rules env)))))
}

  // ; From ainter.lisp
// (defmacro With-ATRE (atre &rest forms)
//    ;; Executes <forms> within <atre>
//   `(let ((*ATRE* ,atre)) ,@ forms))

  // ; From ainter.lisp
// (defun In-ATRE (atre) (setq *ATRE* atre))

// ;;;; Running ATRE

  // ; From ainter.lisp
// (defun read-form () (read))
