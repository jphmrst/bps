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

package org.maraist.tms.jtms
import scala.collection.mutable.{ListBuffer, HashSet, HashMap}

trait Rule[I] {
  val id: Int
  val dbClass: DbClass[I]
  type V
  def matcher(m: Matchable): Option[V]
  def body(jtms: JTMS[I], jtre: JTRE[I], values: V): Unit

  // (defstruct (rule (:PRINT-FUNCTION jtre-rule-printer))
  //   id           ; Unique ID for easy lookup
  //   jtre         ; The JTRE it is part of
  //   dbclass      ; Dbclass of associated pattern
  //   matcher      ; Procedure that performs the match.
  //   body)        ; Procedure that does the work.

  override def toString: String = s"<Rule $id>"
  def jtreRulePrinter: Unit = print(toString)
  def printRule: Unit = print(toString)
  // (defun jtre-rule-printer (r st ignore)
  //   (declare (ignore ignore))
  //   (format st "<Rule ~D>" (rule-id r)))
  //
  // (defun print-rule (rule &optional (stream *standard-output*))
  //   (format stream "~% ~A: ~A, ~A" rule
  //      (rule-matcher rule) (rule-body rule)))
}

object Rule {

  def buildRule[I](trigger: Any, body: Any): Rule[I] = ???
  // (defun build-rule (trigger body &aux match-procedure body-procedure)
  //   (multiple-value-bind (pattern condition var test)
  //                   (parse-rule-trigger trigger)
  //    (setq match-procedure
  //     (generate-match-procedure pattern var test condition))
  //    (setq body-procedure
  //     (generate-body-procedure pattern condition var body))
  //    (push match-procedure *rule-procedures*)
  //    (push body-procedure *rule-procedures*)
  //    `(insert-rule
  //      (get-dbclass ,(get-trigger-dbclass pattern))
  //      ;return form to index rule
  //      (function ;the match function for rule
  //        ,(if *bound-vars*
  //        `(lambda (p)
  //           (,(cadr match-procedure) p ,@ *bound-vars*))
  //      (cadr match-procedure)))
  //      (function ;;the body function for rule
  //        ,(if (or *bound-vars*
  //            (not (eq condition :INTERN)))
  //        (let ((tv (nreverse
  //                    (pattern-free-variables trigger))))
  //          (unless (eq condition :INTERN)
  //                  (push 'TRIGGER-NODE tv))
  //          `(lambda ,tv
  //             (,(cadr body-procedure) ,@ tv
  //              ;(fn-name parameters)
  //              ,@ (scratchout tv *bound-vars*))))
  //          (cadr body-procedure))))))
}

  // (defvar *file-counter* 0)
  // (defvar *file-prefix* "")

  // (defmacro Rule-File (prefix)
  //   `(eval-when (compile load eval)
  //      (setq *file-counter* 0)
  //      (setq *file-prefix* ,prefix)))

  // ;;;; Building and installing rules

  // (defmacro rule (triggers &rest body) (do-rule triggers body))

  // (defun do-rule (triggers body)
  //   (let ((*rule-procedures* nil)
  //    (*bound-vars* nil)
  //    (index-form nil))
  //     (setq index-form
  //      (build-rule (car triggers)
  //                  (subst 'internal-rule
  //                         'rule
  //                         (make-nested-rule
  //                          (cdr triggers) body))))
  //   `(progn ,@ *rule-procedures* ,index-form)))

  // (defmacro internal-rule (triggers &rest body)
  //   `(add-internal-rule ,(car triggers)
  //      ,(make-nested-rule (cdr triggers) body)))

  // (defun make-nested-rule (triggers body)
  //   (cond ((null triggers) body)
  //    (t `((add-internal-rule ,(car triggers)
  //           ,(make-nested-rule (cdr triggers) body))))))

  // (defmacro add-internal-rule (trigger body)
  //   (build-rule trigger body))

  // ;;;; Details of rule-building

  // (defun parse-rule-trigger (trigger)
  //   (values (cadr trigger)
  //      (cond ((member (car trigger) '(:INTERN :IN :OUT))
  //             (car trigger))
  //            (t (error
  //                "~% Unknown belief condition ~A in trigger ~A."
  //                      (car trigger) trigger)))
  //      (cadr (member :VAR (cddr trigger)))
  //      (cadr (member :TEST (cddr trigger)))))

  // (defun get-trigger-dbclass (trigger)
  //   (cond ((variable? trigger)
  //     (if (member trigger *bound-vars*)  trigger
  //         (error "~%Trigger dbclass is unbound -- ~A."
  //                trigger)))
  //    ((atom trigger)  (list 'QUOTE trigger))
  //    (t (get-trigger-dbclass (car trigger)))))

  // ;;;; Generating the body function

  // (defmacro with-pushed-variable-bindings (new-bindings
  //                                      &rest body)
  //   `(let ((*bound-vars* (append ,new-bindings
  //                           (scratchout ,new-bindings
  //                                       *bound-vars*))))
  //      ,@ body))

  // (defun generate-body-procedure (pattern condition var body
  //                                    &aux newly-bound env fname)
  //   (setq newly-bound (pattern-free-variables pattern))
  //   (if var (push var newly-bound))
  //   (setq body (with-pushed-variable-bindings
  //           newly-bound (fully-expand-body body)))
  //   (setq env (append newly-bound
  //                (scratchout newly-bound *bound-vars*)))
  //   (unless (eq condition :INTERN) (push 'trigger-node env))
  //   (setq fname (generate-rule-procedure-name pattern))
  //   `(defun ,fname ,env
  //      ,@ (cond ((eq condition :INTERN) body) ;; Just do it
  //          (t ;; Must check and see if the node's belief state
  //             ;; matches the rule's requirements
  //           `((cond ((,(case condition
  //                            (:IN 'in-node?)(:OUT 'out-node?)
  //                            (t (error "~A bad condition -- GBF"
  //                                             condition)))
  //                     TRIGGER-NODE) ,@ body)
  //                   (t (push (list ',fname ,@ env)
  //                            ,(ecase condition
  //                                   (:IN `(tms-node-in-rules TRIGGER-NODE))
  //                                   (:OUT `(tms-node-out-rules TRIGGER-NODE)
  //                                         ))))))))))

  // (defun generate-match-procedure (pattern var test condition)
  //   (multiple-value-bind (tests binding-specs)
  //    (generate-match-body
  //     pattern (pattern-free-variables pattern) test)
  //    `(defun ,(generate-rule-procedure-name pattern)
  //       (P ,@ *bound-vars*)
  //        ;;first arg, P, is the pattern
  //        (if (and ,@ tests)
  //       (values T (list ,@ (if var '(P))
  //                       ,@ (reverse binding-specs))
  //               ,(unless (eq condition :INTERN) t))))))

  // (defun scratchout (l1 l2)  ;non-destructive and order-preserving
  //   (dolist (el1 l1 l2) (setq l2 (remove el1 l2))))

  // (defun generate-rule-procedure-name (pattern)
  //   (intern (format nil "~A-~A-~A" *file-prefix* pattern (incf *file-counter*))))

  // ;;;; Recursive macroexpansion

  // (defvar *macros-to-expand*
  //   '(rule rlet rassert! rretract!
  //     internal-rule add-internal-rule with-pushed-variable-bindings
  //     without-contradiction-check with-contradiction-check
  //     with-contradiction-handler with-JTRE))

  // (defun fully-expand-body (body)
  //   (cond ((null body) nil)
  //    ((not (listp body)) body)
  //    ((symbolp (car body))
  //     (cond ((member (car body) *macros-to-expand*)
  //            (fully-expand-body (macroexpand body)))
  //           (t (cons (car body)
  //                    (fully-expand-body (cdr body))))))
  //    (t (cons (fully-expand-body (car body))
  //             (fully-expand-body (cdr body))))))

  // ;;;; Running rules

  // (defun try-rules (datum)
  //   (dolist (rule (dbclass-rules (datum-dbclass datum)))
  //     (try-rule-on rule datum)))

  // (defun try-rule-on (rule datum)
  //   (let ((*JTRE* (dbclass-jtre (datum-dbclass datum))))
  //     (multiple-value-bind (okay? bindings node?)
  //      (funcall (rule-matcher rule)
  //          (datum-lisp-form datum))
  //      (when okay?
  //       (when node?
  //             (push (datum-tms-node datum) bindings))
  //       (enqueue (cons (rule-body rule) bindings) *JTRE*)))))

  // (defun run-rules (&optional (*JTRE* *JTRE*))
  //   (do ((form (dequeue *JTRE*) (dequeue *JTRE*))
  //        (counter 0 (1+ counter)))
  //       ((null form)
  //        (debugging-jtre "~%    ~A rules run."  counter)
  //        (incf (jtre-rules-run *JTRE*) counter))
  //     (apply (car form) (cdr form))))

  // (defun rules-waiting? (jtre) (jtre-queue jtre))

  // (defun enqueue (new j) (push new (jtre-queue j)))

  // (defun dequeue (jtre) (pop (jtre-queue jtre)))

  // ;;;; Display routines

  // (defun test-rule-expansion ()
  //  (pprint (macroexpand
  //      '(rule ((:IN (implies ?p ?q) :VAR ?f1)
  //              (:IN ?p)) (rassert! ?q (:CE ?f1 ?p))))))
  //
