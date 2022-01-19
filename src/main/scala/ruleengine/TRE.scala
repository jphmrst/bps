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
import java.io.PrintStream

// Tiny rule engine, translated from F/dK version 61 of 7/21/92.

type Q = Any

/**
  *
  * **Arguments and `val` members translated from**:
  * <pre>
; From tinter.lisp
(defstruct (tre (:PRINT-FUNCTION tre-printer))
  title                     ; String for printing
  (dbclass-table nil)         ; symbols --> classes
  (debugging nil)           ; prints extra info if non-nil
  (queue nil)               ; LIFO
  (rule-counter 0)          ; Unique id for rules
  (rules-run 0))            ; Statistics

(defun create-tre (title &key debugging)
  (make-tre :TITLE title
            :DBCLASS-TABLE (make-hash-table :test #'eq)
            :DEBUGGING debugging))
</pre>
  *
  * **Untranslated Lisp code**:
  * <pre>
(defvar *TRE* nil "Name for default TRE")
(proclaim '(special *TRE*)) ;; Current TRE

(defmacro With-TRE (tre &rest forms)
  `(let ((*TRE* ,tre)) ,@ forms))

(defun in-TRE (tre) (setq *TRE* tre))

;; From tinter.lisp --- Here we just call tre.runRules in between
;; whatever other code.
(defun run (&optional (*TRE* *TRE*))
    (format T "~%>>")
    (do ((form (read) (read)))
        ((member form '(quit stop exit)) nil)
        (format t "~%~A" (eval form))
        (run-rules *tre*)  ;; Defined in RULES module
        (format t "~%>>")))

(defun run-forms (*TRE* forms) ;; Toplevel for programs
  (dolist (form forms)
          (eval form) (run-rules *TRE*)))
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
abstract class TRE[K, F](val title: String)(using factImpl: FactImpl[K, F]) {

  /** Prints extra info if non-nil.
    *
    * <pre>
;; From tinter.lisp
(defun debug-tre (tre debugging)
  (setf (tre-debugging tre) debugging))
</pre>
    */
  var debugging: Boolean = false

  /**
    *
    * <pre>
;; From tinter.lisp
(defun tre-printer (tre st ignore)
  (format st "<TRE: ~A>" (tre-title tre)))
</pre>
    */
  def trePrinter(st: PrintStream): Unit = st.print(s"<TRE: $title>")

  /** Map from symbols to classes. */
  val dbClassTable: HashMap[K, DbClass[K, F]] = new HashMap

  /**
    *
    * <pre>
;; From tinter.lisp
(defmacro debugging-tre (msg &rest args)
  `(when (tre-debugging *TRE*) (format t ,msg ,@ args)))
</pre>
    */
  inline def debuggingTre(msg: String): Unit = if debugging then println(msg)

  // ***** Drivers for programs and people ***************************

  /**
    *
    * <pre>
;; From tinter.lisp
(defun show (&optional (stream *standard-output*))
  ;; Pass on the request to both modules of default TRE
  (show-data stream)
  (show-rules stream))
</pre>
    */
  def show(stream: PrintStream = Console.out): Unit = {
    showData(stream)
    showRules(stream)
  }

  /**
    *
    * <pre>
;; From rules.lisp
(defun add-rule (trigger body &aux rule dbclass)
  ;; First build the struct
  (setq rule (make-rule :TRIGGER trigger
                        :BODY body
                        :COUNTER (incf (tre-rule-counter *TRE*))
                        :ENVIRONMENT *ENV*))
  ;; Now index it
  (setq dbclass (get-dbclass trigger *TRE*))
  (push rule (dbclass-rules dbclass))
  (setf (rule-dbclass rule) dbclass)
  (debugging-tre "~% TRE: New rule: ~A" (print-rule rule nil))
  ;; Go into the database and see what it might trigger on.
  (dolist (candidate (get-candidates trigger *TRE*))
    (try-rule-on rule candidate *TRE*)))
</pre>
    */
  def addRule(rule: Rule[K, F]): Unit = {
    val dbClass = indexToDbClass(rule.className)
    dbClass.rules += rule
    for (candidate <- dbClass.facts) do tryRuleOn(rule, candidate)
  }

  /**
    *
    * <pre>
;; From data.lisp
(defun assert! (fact &optional (*TRE* *TRE*))
  (when (insert fact *tre*)  ;; when it isn't already there
    (try-rules fact *tre*))) ;; run the rules on it.
</pre>
    */
  def assert(fact: F): Unit = if insert(fact) then tryRules(fact)

  /**
    *
    * <pre>
;; From data.lisp
(defun insert (fact tre &aux dbclass)
  (setq dbclass (get-dbclass fact tre)) ;Question: Why not use PUSHNEW here?
  (unless (member fact (dbclass-facts dbclass) :TEST #'equal)
          (debugging-tre "~% ~A: Inserting ~A into database." tre fact)
          (push fact (dbclass-facts dbclass))))
</pre>
    */
  def insert(fact: F): Boolean = {
    ???
  }

  /**
    *
    * <pre>
;; From data.lisp
(defun get-dbclass (fact tre &aux dbclass val)
  (cond ((listp fact) (get-dbclass (car fact) tre))
        ((variable? fact)
         ;; We might be in the environment of some rule, so must
         ;; check the variable's bindings.
         (cond ((boundp fact) (get-dbclass (symbol-value fact) tre))
               ((setq val (assoc fact *ENV*))
                (get-dbclass (cdr val) tre))
               (t (error "~%Dbclass unbound: ~A" fact))))
        ((symbolp fact)
         (cond ((setq dbclass (gethash fact (tre-dbclass-table tre))) dbclass)
               ;; Nothing found, so build it.
               (t (setq dbclass (make-dbclass :NAME fact :TRE tre
                                          :FACTS nil :RULES nil))
                  (setf (gethash fact (tre-dbclass-table tre)) dbclass)
                  dbclass)))
        (t (error "Bad dbclass type: ~A" fact))))
</pre>
    */
  def getDbClass(fact: F): DbClass[K, F] = {
    ???
  }
  def indexToDbClass(s: K): DbClass[K, F] = {
    ???
  }

  /**
    *
    * <pre>
;; From data.lisp
(defun fetch (pattern &optional (tre *TRE*) &aux bindings unifiers)
  ;; Returns the list of facts which unify with the pattern.
  (dolist (candidate (get-candidates pattern tre) unifiers)
    (setq bindings (unify pattern candidate))
    (unless (eq bindings :FAIL)
      (push (sublis bindings pattern) unifiers))))
</pre>
    */
  def fetch(pattern: F): List[F] = {
    ???
  }

  /**
    *
    * <pre>
;; From data.lisp
(defun show-data (&optional (stream *standard-output*) &aux counter)
  (setq counter 0)
  (maphash #'(lambda (key dbclass)
               (dolist (datum (dbclass-facts dbclass))
                       (incf counter)
                       (format stream "~%~A" datum)))
           (tre-dbclass-table *TRE*))
  counter)
</pre>
    */
  def showData(stream: PrintStream = Console.out): Int = {
    ???
  }

  /**
    *
    * <pre>
;; From data.lisp
(defun get-candidates (pattern tre) (dbclass-facts (get-dbclass pattern tre)))
</pre>
    */
  def getCandidates(pattern: F): List[F] = {
    ???
  }

  /**
    *
    * <pre>
; From rules.lisp
(defun try-rules (fact tre)
  ;; This is called by the database system when it adds something.
  (dolist (rule (get-candidate-rules fact tre))
    (try-rule-on rule fact tre)))
</pre>
    */
  def tryRules(fact: F): Unit = {
    ???
  }

  /**
    *
    * <pre>
; From rules.lisp
(defun get-candidate-rules (fact tre)
  (dbclass-rules (get-dbclass fact tre)))
</pre>
    */
  def getCandidateRules(fact: F): List[Rule[K, F]] = {
    ???
  }

  /**
    *
    * <pre>
; From rules.lisp
(defun try-rule-on (rule fact tre &aux bindings)
  ;; If the trigger matches, queue it up.
  (setq bindings (unify fact (rule-trigger rule)
                        (rule-environment rule)))
  (unless (eq bindings :FAIL)
    (enqueue (cons (rule-body rule) bindings) tre)))
</pre>
    */
  def tryRuleOn(rule: Rule[K, F], fact: F): Unit = {
    ???
  }

  /**
    *
    * <pre>
; From rules.lisp
(defun run-rules (tre) ;; Called externally
    (do ((rule-pair (dequeue tre) (dequeue tre))
         (counter 0 (1+ counter)))
        ((null rule-pair)
         (debugging-tre  "~%    ~A rules run."  counter))
        (run-rule rule-pair tre)))

;; Ideally, all rules triggered will be executed, and the
;; results will be independent of the order of execution.
;; Thus a simple LIFO queue suffices.
</pre>
    */
  def runRules: Unit = {
    ???
  }

  /**
    *
    * <pre>
; From rules.lisp
(defun enqueue (new tre) (push new (tre-queue tre)))
</pre>
    */
  def enqueue(n: Q): Unit = {
    ???
  }

  /**
    *
    * <pre>
; From rules.lisp
(defun dequeue (tre) (pop (tre-queue tre)))
</pre>
    */
  def dequeue: Q = {
    ???
  }

  /**
    *
    * <pre>
; From rules.lisp
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
</pre>
    */
  def runRule(bindings: Bindings[F], body: RuleBody): Unit = {
    ???
  }

  /**
    *
    * <pre>
; From rules.lisp
(defun show-rules (&optional (stream *standard-output*) &aux counter)
  (setq counter 0)
  (maphash #'(lambda (key dbclass)
               (dolist (rule (dbclass-rules dbclass))
                       (incf counter)
                       (format stream "~%  ")
                       (print-rule rule stream)))
           (tre-dbclass-table *TRE*))
  counter)
</pre>
    */
  def showRules(stream: PrintStream = System.out): Unit = {
    ???
  }

  /**
    *
    * <pre>
;; From rules.lisp
(defun add-rule (trigger body &aux rule dbclass)
  ;; First build the struct
  (setq rule (make-rule :TRIGGER trigger
                        :BODY body
                        :COUNTER (incf (tre-rule-counter *TRE*))
                        :ENVIRONMENT *ENV*))
  ;; Now index it
  (setq dbclass (get-dbclass trigger *TRE*))
  (push rule (dbclass-rules dbclass))
  (setf (rule-dbclass rule) dbclass)
  (debugging-tre "~% TRE: New rule: ~A" (print-rule rule nil))
  ;; Go into the database and see what it might trigger on.
  (dolist (candidate (get-candidates trigger *TRE*))
    (try-rule-on rule candidate *TRE*)))
</pre>
    */
  def addRule(trigger: F, body: RuleBody): Unit = {
    ???
  }

  /**
    *
    * <pre>
;; Sugar for the user (or other programs!)
; From rules.lisp
(defmacro rule (trigger &rest body) `(add-rule ',trigger ',body))
</pre>
    */
  inline def rule(trigger: F, body: RuleBody): Rule[K, F] = {
    ???
  }
}
