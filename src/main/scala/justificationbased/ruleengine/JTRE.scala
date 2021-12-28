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

package org.maraist.truthmaintenancesystems.justificationbased.ruleengine
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}
import org.maraist.truthmaintenancesystems.justificationbased.{
  JTMS, Node, Just, Justification, UserStipulation}

// type Fact = Symbol | List[Fact]
sealed trait Fact {
  def factToString: String
}

class SymbolFact(val symbol: Symbol) extends Fact {
  override def factToString: String = symbol.toString
}
object SymbolFact {
  /**
    * Universal match to the singleton instance of this evidence
    * class.
    */
  def unapply(f: SymbolFact): Some[Symbol] = Some(f.symbol)
}

class ListFact(val facts: List[Fact]) extends Fact {
  override def factToString: String = facts.map(_.factToString).mkString(", ")
}
object ListFact {
  /**
    * Universal match to the singleton instance of this evidence
    * class.
    */
  def unapply(l: ListFact): Some[List[Fact]] = Some(l.facts)
}

/**
  *
  *
  * @param title
  * @param debugging
  *
  * **Arguments and `val` members translated from**:
  * <pre>
(defstruct (jtre (:PRINT-FUNCTION jtre-printer))
  title                   ; Pretty name
  jtms                    ; Pointer to its JTMS
  (dbclass-table nil)     ; Table of dbclasses
  (datum-counter 0)       ; Unique ID for asserts
  (rule-counter 0)        ; Unique ID for rules
  (debugging nil)         ; If non-NIL, show basic operations
  (queue nil)             ; Rule queue
  (rules-run 0))          ; Statistic

(defun create-jtre (title &key debugging)
 (let ((j (make-jtre
      :TITLE title
      :JTMS (create-jtms (list :JTMS-OF title)
                         :NODE-STRING 'view-node)
      :DBCLASS-TABLE (make-hash-table :TEST #'eq)
      :DEBUGGING debugging)))
   (change-jtms (jtre-jtms j)
           :ENQUEUE-PROCEDURE
           #'(lambda (rule) (enqueue rule j)))
   j))

(defun change-jtre (jtre &key (debugging :NADA))
  (unless (eq debugging :NADA)
     (setf (jtre-debugging jtre) debugging)))
</pre>
  *
  * **Unused and not translated**:
  * <pre>
;; From jdata.lisp — does not seem to be used anywhere.
(defun run-forms (forms &optional (*JTRE* *JTRE*))
  (dolist (form forms) (eval form) (run-rules *JTRE*)))


Only called from some examples — leaving untranslated for now. [JM]
  //
;; From jdata.lisp
(defun fetch (pattern &optional (*JTRE* *JTRE*) &aux bindings unifiers)
  (dolist (candidate (get-candidates pattern) unifiers)
    (setq bindings (unify pattern (datum-lisp-form candidate)))
    (unless (eq bindings :FAIL)
      (push (sublis bindings pattern) unifiers))))

;;;; More display-intensive procedures

Does not seem to be used anywhere — leaving untranslated. [JM]
  //
;; From jdata.lisp
(defun wfs (fact &optional (*JTRE* *JTRE*))
  ;; Displays well-founded support for a fact
  (cond ((out? fact) (format t "~% ~A is OUT." fact))
   (t (do ((queue (list (get-tms-node fact))
                  (nconc (cdr queue) new-antes))
           (so-far (list (get-tms-node fact)))
           (new-antes nil nil))
          ((null queue) (format t "~%--------") fact)
        (why-node (car queue))
        (unless (or (out-node? (car queue))
                    (tms-node-assumption? (car queue)))
          ;; Go down the support
          (dolist (ante (just-antecedents
                         (tms-node-support (car queue))))
            (unless (member ante so-far)
              (push ante so-far)
              (push ante new-antes))))))))

Called from some examples only — leaving untranslated for now. [JM]
  //
(defun run-rules (&optional (*JTRE* *JTRE*))
  (do ((form (dequeue *JTRE*) (dequeue *JTRE*))
       (counter 0 (1+ counter)))
      ((null form)
       (debugging-jtre "~%    ~A rules run."  counter)
       (incf (jtre-rules-run *JTRE*) counter))
    (apply (car form) (cdr form))))

Not relevant in Scala translation: methods now called against a
particular object, not a special global variable.
  //
(defvar *JTRE* nil)
(defmacro With-Jtre (jtre &rest forms)
  `(let ((*JTRE* ,jtre)) ,@ forms))
(defun In-Jtre (jtre) (setq *JTRE* jtre))
</pre>
  */
class JTRE[I](val title: String, val debugging: Boolean = false) {

  /** Pointer to its JTMS. */
  val jtms: JTMS[Datum[I], I, Rule[I]] = new JTMS[Datum[I], I, Rule[I]](
    title,
    // nodeString = (n: Node[Datum[I], I, Rule[I]]) => n.datum.fact.factToString,
    enqueueProcedure = Some(this.enqueue))

  /** Table of `DbClass`es. */
  val dbClassTable: DbClassTable[I] = new HashMap[Any, Any]

  /** Unique ID generator for asserts. */
  var datumCounter: Int = 0
  def incfDatumCounter: Int = {
    datumCounter = 1 + datumCounter
    datumCounter
  }

  /** Unique ID generator for rules. */
  var ruleCounter: Int = 0
  def incfRuleCounter: Int = {
    ruleCounter = 1 + ruleCounter
    ruleCounter
  }

  /** Statistic. */
  var rulesRun: Int = 0

  /** Rule queue. */
  val queue: Queue[Rule[I]] = Queue.empty

  /**
    *
    *
    * @return
    *
    * **Translated from**:
    * <pre>
(defun jtre-printer (j st ignore)
  (format st "<JTRE: ~A>" (jtre-title j)))
</pre>
    */
  override def toString: String = s"<JTRE: $title>"
  /**
    *
    *
    * **Translated from**:
    * <pre>
(defun jtre-printer (j st ignore)
  (format st "<JTRE: ~A>" (jtre-title j)))
</pre>
    */
  def jtrePrinter: Unit = print(toString)

  //
  // ;;;; Making statements

  /**
    *
    *
    * @param fact
    * @param just
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun assert! (fact just &optional (*JTRE* *JTRE*) &aux datum node)
  (setq datum (referent fact t)
        node (datum-tms-node datum))
  (unless (listp just) (setq just (list just)))
  (debugging-jtre "~%    Asserting ~A via ~A." fact just)
  (justify-node (car just) node
                (mapcar #'(lambda (f) (datum-tms-node (referent f t)))
                        (cdr just)))
  datum)
</pre>
    */
  def assert(fact: Fact, just: Justification[Datum[I], I, Rule[I]]):
      Datum[I] = {
    val factList: List[Fact] = fact match {
      case ListFact(l) => l
      case SymbolFact(_) => List(fact)
    }
    val datum = referent(fact, true).get
    val node = datum.node
    dbgJtre(
      this,
      s"    Asserting (${factList.map(_.toString).mkString(", ")}) via $just"
    )

    // jtms.justifyNode(factList.head, node, factList.tail.map((f) => referent(f, true).get.node))
    ??? // TODO Come back to this --- we have a list here but
        // ListBuffer expeted, probably unnecessarily.
  }

  /**
    *
    *
    * @param fact
    * @param just
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun quiet-assert! (fact just &optional (*JTRE* *JTRE*))
  (without-contradiction-check (jtre-jtms *JTRE*) (assert! fact just)))
</pre>
    */
  def quietAssert(fact: Fact, just: Just[Datum[I], I, Rule[I]]): Datum[I] = {
    ???
  }

  /**
    *
    *
    * @param fact
    * @param reason
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun assume! (fact reason &optional (*JTRE* *JTRE*) &aux datum node)
  (setq datum (referent fact t)
        node (datum-tms-node datum))
  (cond
   ((not (datum-assumption? datum))
    (setf (datum-assumption? datum) reason)
    (debugging-jtre "~%    Assuming ~A via ~A." fact reason)
    (assume-node node))
   ((eq reason (datum-assumption? datum)))
   (t (error
       "Fact ~A assumed because of ~A assumed again because of ~A"
       (show-datum datum) (datum-assumption? datum) reason)))
  datum)
</pre>
    */
  def assume(fact: Fact, reason: Node[Datum[I], I, Rule[I]]): Datum[I] = {
    ???
  }

  /**
    *
    *
    * @param fact
    * @param just
    * @param quiet
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun retract! (fact &optional (just 'user) (quiet? nil)
                 (*JTRE* *JTRE*) &aux datum node)
  (setq datum (referent fact t)
        node (datum-tms-node datum))
  (cond
   ((not (tms-node-assumption? node))
    (unless quiet?
      (format t "~%~A isn't an assumption."
              (show-datum datum))))
   ((not (in-node? node))
    (unless quiet?
      (format T
        "~%The assumption ~A is not currently in."
        fact)))
   ((eq just (datum-assumption? datum))
    (debugging-jtre "~%    Retracting ~A via ~A."
                    fact just)
    (setf (datum-assumption? datum) nil)
    (retract-assumption node))
   ((not quiet?)
    (format t "~%~A not source of assumption for ~A"
            just fact)))
  node)
</pre>
    */
  def retract(
    fact: Fact,
    just: Justification[Datum[I], I, Rule[I]] = UserStipulation,
    quiet: Boolean = false):
      Node[Datum[I], I, Rule[I]] = {
    ???
  }

  /**
    *
    *
    * @param fact
    * @param just
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun uassert! (fact &optional (just 'user))
  (assert! fact just) ;; Do internal operation
  (run-rules *JTRE*))        ;; Run the rules
</pre>
    */
  def uAssert(
    fact: Fact,
    just: Justification[Datum[I], I, Rule[I]] = UserStipulation):
      Unit = {
    ???
  }

  /**
    *
    *
    * @param fact
    * @param reason
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun uassume! (fact reason) ;; Similar to UASSERT!
  (assume! fact reason *JTRE*)
  (run-rules *JTRE*))
</pre>
    */
  def uAssume(fact: Fact, reason: Node[Datum[I], I, Rule[I]]): Unit = {
    ???
  }

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun run (&optional (*JTRE* *JTRE*)) ;; Toplevel driver function
    (format T "~%>>")
    (do ((form (read) (read)))
        ((member form '(quit stop exit abort)) nil)
        (format t "~%~A" (eval form))
        (run-rules)
        (format t "~%>>")))
</pre>
    */
  def run: Unit = {
    ???
  }

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun show (&optional (*JTRE* *JTRE*) (stream *standard-output*))
  (show-data *JTRE* stream) (show-rules *JTRE* stream))
</pre>
    */
  def show: Unit = {
    ???
  }

  /**
    *
    *
    * @param fact
    * @param isVirtual
    * @return
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun referent (fact &optional (virtual? nil)
                 (*JTRE* *JTRE*))
  (if virtual? (insert fact) (referent1 fact)))
</pre>
    */
  def referent(fact: Fact, isVirtual: Boolean = false): Option[Datum[I]] = {
    ???
  }

  /**
    *
    *
    * @param fact
    * @return
    *
    * **Translated from**:
    * <pre>
(defun already-assumed? (fact  &aux r)
  (when (setq r (referent fact))
    (datum-assumption? r)))
</pre>
    */
  def isAlreadyAssumed(fact: Fact): Boolean = referent(fact) match {
    case None => false
    case Some(r) => r.isAssumption
  }

  /**
    *
    *
    * @param fact
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun contradiction (fact &optional (*JTRE* *JTRE*))
  (make-contradiction (datum-tms-node (referent fact t))))
</pre>
    */
  def contradiction(fact: Fact): Node[Datum[I], I, Rule[I]] = {
    ???
  }

  // Interface and display of data

  /**
    *
    *
    * @param fact
    * @return
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun in? (fact &optional (*JTRE* *JTRE*) &aux r)
  (when (setq r (referent fact))
   (in-node? (datum-tms-node r))))
</pre>
    */
  def isIn(fact: Fact): Boolean = {
    ???
  }

  /**
    *
    *
    * @param fact
    * @return
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun out? (fact &optional (*JTRE* *JTRE*) &aux r)
  (or (not (setq r (referent fact))) ; a non-existent fact is out
      (out-node? (datum-tms-node r))))
</pre>
    */
  def isOut(fact: Fact): Boolean = {
    ???
  }

  /**
    *
    *
    * @param fact
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun why? (fact &optional (*JTRE* *JTRE*) &aux r)
  (when (setq r (referent fact))
   (why-node (datum-tms-node r))))
</pre>
    */
  def why(fact: Fact): Node[Datum[I], I, Rule[I]] = {
    ???
  }

  /**
    *
    *
    * @param fact
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun assumptions-of (fact &optional (*JTRE* *JTRE*))
  (mapcar #'view-node
     (assumptions-of-node
      (datum-tms-node (referent fact *jtre* t)))))
</pre>
    */
  def assumptionsOf(fact: Fact): Node[Datum[I], I, Rule[I]] = {
    ???
  }

  /**
    *
    *
    * @param pr
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun say-datum-belief (pr &optional (*jtre* *jtre*)
                       (indent ""))
  (format t "~%~A~A: ~A" indent pr
     (if (in-node? (get-tms-node pr *jtre*))
         "IN" "OUT")))
</pre>
    */
  def sayDatumBelief(pr: Datum[I]): Unit = {
    ???
  }

  /**
    *
    *
    * @param fact
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun show-justifications (fact &optional (*jtre* *jtre*))
  (format t "~% ~A::" fact)
  (let* ((node (get-tms-node fact *jtre*))
    (justs (tms-node-justs node)))
    (unless justs
       (format t " No justifications.")
       (return-from show-justifications node))
    (dolist (j justs)
       (format t "~% ~A" (just-informant j))
       (cond ((just-antecedents j)
              (format t ", on:")
              (dolist (ante (just-antecedents j))
                      (say-datum-belief
                       (view-node ante) *jtre* "  "))
              (format t "."))
             (t (format t "."))))))
</pre>
    */
  def showJustifications(fact: Fact): Unit = {
    ???
  }

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun show-data (&optional (*JTRE* *JTRE*)
                       (stream *standard-output*))
  (format stream
     "~%~D facts total." (jtre-datum-counter *JTRE*))
  (map-dbclass
   #'(lambda (dbclass)
       (dolist (datum (dbclass-facts dbclass))
          (format stream "~%~A: ~A" (show-datum datum)
                  (if (in-node? (datum-tms-node datum))
                      "IN" "OUT"))))))
</pre>
    */
  def showData: Unit = {
    ???
  }

  // ;;;; Database system

  /**
    *
    *
    * @param fact
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun get-dbclass (fact &optional (*JTRE* *JTRE*)
                    &aux dbclass)
  (cond ((null fact) (error "~% NIL can't be a dbclass."))
   ((listp fact) (get-dbclass (car fact) *JTRE*))
   ((variable? fact)
    (cond ((boundp fact)
           (get-dbclass (symbol-value fact) *JTRE*))
          (t (error "~%Dbclass unbound: ~A" fact))))
   ((symbolp fact)
    (cond ((setq dbclass
                 (gethash fact
                          (jtre-dbclass-table *JTRE*)))
           dbclass)
          (t (setq dbclass
                   (make-dbclass :NAME fact :FACTS nil
                               :RULES nil :JTRE *JTRE*))
             (setf (gethash fact
                    (jtre-dbclass-table *JTRE*))
                   dbclass)
             dbclass)))
   (t (error "Bad dbclass type: ~A" fact))))
</pre>
    */
  def getDbClass(fact: Fact): DbClass[I] = {
    ???
  }

  /**
    *
    *
    * @param proc
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun map-dbclass (proc &optional (*JTRE* *JTRE*))
  (maphash #'(lambda (name dbclass) (declare (ignore name))
          (funcall proc dbclass))
      (jtre-dbclass-table *JTRE*)))
</pre>
    */
  def mapDbClass(proc: (DbClass[I]) => Unit): Unit = {
    ???
  }

  /**
    *
    *
    * @param fact
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun get-tms-node (fact &optional (*JTRE* *JTRE*))
  (datum-tms-node (referent fact t)))
</pre>
    */
  def getTmsNode(fact: Fact): Node[Datum[I], I, Rule[I]] = {
    ???
  }

  /**
    *
    *
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun get-datum (num &optional (*JTRE* *JTRE*))
  (map-dbclass
   #'(lambda (dbclass)
       (dolist (datum (dbclass-facts dbclass))
          (when (= (datum-id datum) num)
                (return-from GET-DATUM datum))))))
</pre>
    */
  def getDatum(num: Int): Datum[I] = {
    ???
  }

  /**
    *
    *
    *
    * **Translated from**:
    * <pre>
;; From jdata.lisp
(defun get-just (num &optional (*JTRE* *JTRE*))
  (dolist (just (jtms-justs (jtre-jtms *JTRE*)))
    (when (= (just-index just) num)
     (return-from GET-just just))))
</pre>
    */
  def getJust(num: Int): Just[Datum[I], I, Rule[I]] = {
    ???
  }

  /**
    *
    *
    *
    * **Translated from**:
    * <pre>
(defun show-rules (&optional (*JTRE* *JTRE*) (stream *standard-output*))
  (format t "~%There are ~D rules in ~A:"
     (jtre-rule-counter *JTRE*) (jtre-title *JTRE*))
  (format stream "~% ~A queued." (if (null (jtre-queue *JTRE*)) "None"
                              (length (jtre-queue *JTRE*))))
  (map-dbclass #'(lambda (dbclass)
            (dolist (rule (dbclass-rules dbclass))
                    (print-rule rule stream)))))
</pre>
    */
  def showRules: Unit = {
    ???
  }

  /**
    *
    *
    *
    * **Translated from**:
    * <pre>
(defun get-rule (num &optional (*JTRE* *JTRE*))
  (map-dbclass #'(lambda (dbclass)
            (dolist (rule (dbclass-rules dbclass))
                    (when (= (rule-id rule) num)
                          (return-from GET-RULE rule))))))
</pre>
    */
  def getRule(num: Int): Rule[I] = {
    ???
  }

  /**
    *
    *
    *
    * **Translated from**:
    * <pre>
(defun rules-waiting? (jtre) (jtre-queue jtre))
</pre>
    */
  def rulesWaiting: Boolean = !queue.isEmpty

  /**
    *
    *
    *
    * **Translated from**:
    * <pre>
(defun enqueue (new j) (push new (jtre-queue j)))
</pre>
    */
  def enqueue(rule: Rule[I]): Unit = queue.enqueue(rule)

  /**
    *
    *
    *
    * **Translated from**:
    * <pre>
(defun dequeue (jtre) (pop (jtre-queue jtre)))
</pre>
    */
  def dequeue: Rule[I] = queue.dequeue
}

  /**
    *
    *
    *
    * **Translated from**:
    * <pre>
(defmacro debugging-jtre (msg &rest args)
  `(when (jtre-debugging *JTRE*) (format t ,msg  ,@args)))
</pre>
    */
inline def dbgJtre[I](jtre: JTRE[I], msg: String) =
  if jtre.debugging then println(msg)
