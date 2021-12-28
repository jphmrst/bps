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

type Fact = Unit

/**
  *
  *
  * **Parameters and value members translated from**:
  * <pre>
; From ainter.lisp
(defstruct (rule (:PRINT-FUNCTION (lambda (r st ignore)
                                 (declare (ignore ignore))
                                 (format st "<Rule ~D>"
                                         (rule-counter r)))))
  counter      ; Unique ID for easy lookup
  atre         ; The ATRE it is part of
  dbclass        ; Dbclass of associated pattern
  matcher      ; Procedure that performs the match
  body         ; Procedure that does the rules' work
  in-nodes     ; Must have a jointly non-empty label
  imp-nodes)   ; Must be implied by the focus
</pre>
  * @param ruleEngine
  * @param dbClass
  */
abstract class Rule[I](
  val ruleEngine: RuleEngine[I],
  val dbClass: DbClass[I]
) {
  type V
  def matcher(m: Fact): Option[V]
  def body(jtre: RuleEngine[I], values: V): Unit

  val counter: Int = ruleEngine.incrRuleCounter

  // val inNodes = ListBuffer.empty[Node[

}

/**
  *
  *
  * **Parameters and value members translated from**:
  * <pre>
; From ainter.lisp
(defstruct (atre (:PREDICATE atre?)
              (:PRINT-FUNCTION print-atre))
  title                   ; Pretty name
  atms                    ; Pointer to its ATMS
  (dbclasses nil)           ; List of dbclasses
  (dbclass-table nil)       ; Quick index into dbclasses
  (datum-counter 0)       ; Unique ID for asserts
  (rules nil)             ; Index for rules
  (rule-counter 0)        ; Unique ID for rules
  (debugging nil)         ; Show basic operations
  (queue nil)             ; General queue
  (rules-run 0)           ; Statistics
  (in-rules nil)          ; in-rules to be executed
  (focus nil)             ; State of the search, if any.
  (contradiction-rules nil) ; As in Focus paper (AAAI-88)
  (imp-rules nil))   ; Ibid.

(defun create-atre (title &key debugging)
 (let ((j (make-atre
        :TITLE title
        :ATMS (create-atms (list :ATMS-OF title)
                           :NODE-STRING 'stringify-node)
        :DBCLASS-TABLE (make-hash-table :TEST #'eq)
        :DEBUGGING debugging))
       (false nil))
   (in-atre j)
   (change-atms (atre-atms j)
                :ENQUEUE-PROCEDURE #'(lambda (pair) (enqueue pair j)))
   ;; Create a default contradiction
   (setq false (make-datum :COUNTER (incf (atre-datum-counter j))
                           :ATRE j :LISP-FORM 'FALSE
                           :DBCLASS (get-dbclass 'FALSE)))
   (setf (datum-tms-node false) (atms-contra-node (atre-atms j)))
   (setf (tms-node-datum (datum-tms-node false)) false)
   (push false (dbclass-facts (datum-dbclass false)))
   j))

(defun change-atre (atre &key (debugging nil debugging?))
  (if debugging? (setf (atre-debugging atre) debugging)))
</pre>
  *
  * @param title
  * @param debugging
  */
class RuleEngine[I](
  val title: String,
  var debugging: Boolean = false
) {
  val atms: ATMS[Datum[I], I] =
    new ATMS[Datum[I], I](
      title, (n: Node[Datum[I], I]) => n.datum.toString, debugging)

  // val dbClassTable: HashMap[] =

  /** Unique namer for datum instances. */
  var datumCounter: Int = 0
  /** Increment the datum counter and return its value. */
  def incrDatumCounter: Int = {
    val result = datumCounter
    datumCounter = datumCounter + 1
    result
  }

  var rules = ListBuffer.empty[Rule[I]]

  /** Unique namer for rules. */
  var ruleCounter: Int = 0
  /** Increment the rules counter and return its value. */
  def incrRuleCounter: Int = {
    val result = ruleCounter
    ruleCounter = ruleCounter + 1
    result
  }


  /**
    *
    *
    * **Translated from**:
    * <pre>
(defun print-atre (j st ignore) (declare (ignore ignore))
  (format st "<ATRE: ~A>" (atre-title j)))

(defmacro debugging-atre (msg &rest args)
  `(when (atre-debugging *atre*) (format t ,msg  ,@args)))

; From ainter.lisp
(defun run (&optional (atre *ATRE*)) ;; Toplevel driver function
    (format T "~%>>")
    (do ((form (read-form) (read-form)))
        ((member form '(quit stop exit abort)) nil)
        (format t "~%~A" (eval form))
        (run-rules atre)
        (format t "~%>>")))
</pre>
    */
  override def toString: String = s"RuleEngine $title"
  def printRuleEngine: Unit = println(toString)

  // ; From ainter.lisp
  // (defun run-forms (forms &optional (atre *ATRE*))
  //   (dolist (form forms) (eval form) (run-rules atre)))

  /**
    *
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun show (&optional (atre *ATRE*) (stream *standard-output*))
  (format stream "For ATRE ~A:~% Focus = ~A."
       (atre-title atre)
       (if (env? (atre-focus atre)) (atre-focus atre)
         "empty"))
  (show-data atre stream) (show-rules atre stream))
</pre>
    */
  def show: Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun solutions (atre choice-sets)
  (interpretations
   (atre-atms atre)
   (mapcar #'(lambda (choice-set)
            (mapcar #'(lambda (f) (get-tms-node f atre))
                    choice-set))
        choice-sets)))
</pre>
    */
  def solutions(
    choiceSets: ChoiceSets[Datum[I], I]):
      ListBuffer[Node[Datum[I], I]] = ???

  // ;;;; Implied-by rules

  // ;; The rule expansion code sets up the necessary tests for
  // ;; seeing if the antecedent nodes are implied by the current
  // ;; focus when the rule is on the queue.  Here we just
  // ;; re-queue the implied-by rules which were not in the scope
  // ;; of the previous focus for re-examination.

  /**
    *
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun change-focus (env &optional (atre *atre*))
  (unless (atre? atre) ;; Users do slip, sometimes
    (error "Must change the focus of some ATRE, not ~A." atre))
  (when (and (env? env)
          (not (env-nogood? env)))
    (setf (atre-focus atre) env) ;; change focus
    (setf (atre-queue atre) ;; re-queue implied-by rules
       (nconc (atre-queue atre) (atre-imp-rules atre)))
    (setf (atre-imp-rules atre) nil)
    env)) ;; return new focus to indicate switch
</pre>
    */
  def changeFocus(env: Env[Datum[I], I]): Env[Datum[I], I] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun focus-okay? (atre)
  (and (atre-focus atre)
       (not (env-nogood? (atre-focus atre)))))
</pre>
    */
  def isFocusOkay: Boolean = ???

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

  // ; From adata.lisp
  // (defun justifications (fact &optional (*atre* *atre*)
  //                          (stream *standard-output*))
  //   (node-justifications (get-tms-node fact *atre*) stream))

  // ; From adata.lisp
  // (defun the-e (num &optional (*atre* *atre*))
  //   (e (atre-atms *atre*) num))

  /**
    *
    *
    * **Translated from**:
    * <pre>
; From adata.lisp
(defun get-tms-node (fact &optional (*atre* *atre*))
  (datum-tms-node (referent fact t)))
</pre>
    */
  def getTmsNode(fact: Fact): Node[Datum[I], I] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
; From adata.lisp
(defun insert (fact &aux datum)
  (setq datum (referent1 fact))
  (cond (datum (values datum t))
     (t (setq datum (make-datum
                     :COUNTER
                     (incf (atre-datum-counter *atre*))
                     :ATRE *atre*
                     :LISP-FORM fact
                     :DBCLASS (get-dbclass fact)))
        (setf (datum-tms-node datum)
              (tms-create-node (atre-atms *atre*) datum))
        (push datum (dbclass-facts (datum-dbclass datum)))
        (try-rules datum)
        (values datum nil))))
</pre>
    */
  def insert(fact: Fact): (Datum[I], Boolean) = ???

  // ; From adata.lisp
  // (defun fetch (pattern &optional (*atre* *atre*)
  //                    &aux bindings unifiers)
  //   (dolist (candidate (get-candidates pattern) unifiers)
  //     (setq bindings (unify pattern (datum-lisp-form candidate)))
  //     (unless (eq bindings :FAIL)
  //       (push (sublis bindings pattern) unifiers))))

  // ; From adata.lisp
  // (defun true? (fact &optional (*atre* *atre*) &aux r)
  //   (when (setq r (referent fact nil))
  //      (true-node? (datum-tms-node r))))

  // ; From adata.lisp
  // (defun in? (fact env &optional (*atre* *atre*) &aux r)
  //   (when (setq r (referent fact nil))
  //      (in-node? (datum-tms-node r) env)))

  // ; From adata.lisp
  // (defun out? (fact env &optional (*atre* *atre*) &aux r)
  //   (when (setq r (referent fact nil))
  //      (out-node? (datum-tms-node r) env)))

  // ; From adata.lisp
  // (defun consistent-with? (fact env &optional (*atre* *atre*)
  //                            &aux r)
  //   (when (setq r (referent fact nil))
  //      (node-consistent-with? (datum-tms-node r) env)))

  // ; From adata.lisp
  // (defun why? (fact &optional (*atre* *atre*)
  //                (stream *standard-output*)
  //                &aux r)
  //   (when (setq r (referent fact nil))
  //      (why-node (datum-tms-node r) stream)))

  // ; From adata.lisp
  // (defun environment-of (facts &optional (*atre* *atre*)
  //                           &aux node env)
  //   (setq env (atms-empty-env (atre-atms *atre*)))
  //   (dolist (fact facts)
  //        (setq node (get-tms-node fact *atre*))
  //        (unless (tms-node-assumption? node)
  //   (error "Non-assumption in ENVIRONMENT-OF: ~A." fact))
  //        (setq env (cons-env node env))
  //        (when (env-nogood? env)
  //              (return-from ENVIRONMENT-OF
  //                           (values nil env))))
  //   env)

  // ; From adata.lisp
  // (defun get-datum (num &optional (*atre* *atre*))
  //   (maphash #'(lambda (key dbclass)
  //             (declare (ignore key))
  //             (dolist (datum (dbclass-facts dbclass))
  //               (when (= (datum-counter datum) num)
  //                 (return-from GET-DATUM datum))))
  //         (atre-dbclass-table *atre*)))

  // ; From adata.lisp
  // (defun get-just (num &optional (*atre* *atre*))
  //   (dolist (just (atms-justs (atre-atms *atre*)))
  //     (when (= (just-index just) num)
  //       (return-from GET-just just))))
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
