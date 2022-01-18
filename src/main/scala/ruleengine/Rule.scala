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

package org.maraist.truthmaintenancesystems.ruleengine
import scala.util.control.NonLocalReturns.*
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}

// Tiny rule engine, translated from F/dK version 61 of 7/21/92.

/**
  *
  * **Arguments and `val` members translated from**:
  * <pre>
; From rules.lisp
(proclaim '(special *TRE* *ENV*))
(defvar *ENV* nil)              ; Environment for rules

(defstruct (rule (:PRINT-FUNCTION (lambda (r st ignore)
                                    (format st "<Rule ~D>"
                                            (rule-counter r)))))
     counter            ;Integer to provide unique "name"
     Dbclass            ;Dbclass it is linked to.
     trigger            ;pattern it runs on.
     body               ;code to be evaluated in local environment.
     environment)       ;binding envirionment.

;;;; Interface for rules
</pre>
  *
  * @param title Name of this TMS, for output.
  *
  * @constructor The `title` argument is required; others are optional.
  *
  * @groupname construction Construction methods
  * @groupdesc construction API methods for building and changing
  * an ATMS from an external system.
  * @groupprio construction 1
  *
  * @groupname query Query methods
  * @groupdesc query API methods for querying the TRE and its beliefs
  * from an external system.  Note that most query-style methods are
  * on [[Node]]s.
  * @groupprio query 2
  *
  * @groupname diagnostic Diagnostic and debugging methods
  * @groupdesc diagnostic Reporting the current TRE state as text.
  * @groupprio diagnostic 3
  *
  * @groupname internal Internal methods
  * @groupdesc internal Implementation methods; not generally for use
  * from outside this package.
  * @groupprio internal 10
  */
class Rule(
  val counter: Int
) {

/*

(defun show-rules (&optional (stream *standard-output*) &aux counter)
  (setq counter 0)
  (maphash #'(lambda (key dbclass)
               (dolist (rule (dbclass-rules dbclass))
                       (incf counter)
                       (format stream "~%  ")
                       (print-rule rule stream)))
           (tre-dbclass-table *TRE*))
  counter)

(defun print-rule (rule &optional (stream *standard-output*))
  (format stream "Rule ~A: ~A; ~A"       ;don't show body, too big
          (rule-counter rule)
          ;; Plug in the variables, to show how much has been done.
          (sublis (rule-environment rule)
                  (rule-trigger rule))
          (rule-environment rule)))

;;;; Building and installing rules

;; Sugar for the user (or other programs!)
(defmacro rule (trigger &rest body) `(add-rule ',trigger ',body))

(defun try-rules (fact tre)
  ;; This is called by the database system when it adds something.
  (dolist (rule (get-candidate-rules fact tre))
    (try-rule-on rule fact tre)))

(defun get-candidate-rules (fact tre)
  (dbclass-rules (get-dbclass fact tre)))

(defun try-rule-on (rule fact tre &aux bindings)
  ;; If the trigger matches, queue it up.
  (setq bindings (unify fact (rule-trigger rule)
                        (rule-environment rule)))
  (unless (eq bindings :FAIL)
    (enqueue (cons (rule-body rule) bindings) tre)))

;;;; Executing rules

(defun run-rules (tre) ;; Called externally
    (do ((rule-pair (dequeue tre) (dequeue tre))
         (counter 0 (1+ counter)))
        ((null rule-pair)
         (debugging-tre  "~%    ~A rules run."  counter))
        (run-rule rule-pair tre)))

;; Ideally, all rules triggered will be executed, and the
;; results will be independent of the order of execution.
;; Thus a simple LIFO queue suffices.

(defun enqueue (new tre) (push new (tre-queue tre)))
(defun dequeue (tre) (pop (tre-queue tre)))

(defun run-rule (pair tre)
  ;; Here pair is (<body> . <bindings>).  The LET makes
  ;; the bindings available to nested rules.
  (let ((*ENV* (cdr pair))
        (*TRE* tre))
    (incf (tre-rules-run tre))
    ;; Now we build a form that creates the right environment.
    ;; We will see better ways to do this later.
    (eval `(let ,(mapcar #'(lambda (binding)
                             `(,(car binding)
                               ',(sublis (cdr pair)
                                         (cdr binding))))
                         (cdr pair))
             ,@ (car pair)))))
*/

}
