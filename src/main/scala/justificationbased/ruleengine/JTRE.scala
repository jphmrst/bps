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
import scala.collection.mutable.{HashSet, HashMap, Queue}
import org.maraist.truthmaintenancesystems.justificationbased.{
  JTMS, Node, Just, Justification, UserStipulation, TmsError}

// type Fact = Symbol | List
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

Not relevant in Scala translation: methods now called against a
particular object, not a special global variable.
  //
(defvar *JTRE* nil)
(defmacro With-Jtre (jtre &rest forms)
  `(let ((*JTRE* ,jtre)) ,@ forms))
(defun In-Jtre (jtre) (setq *JTRE* jtre))
</pre>
  */
class JTRE(val title: String, val debugging: Boolean = false) {

  /** Pointer to its JTMS. */
  val jtms: JTMS[Datum, Fact, Rule] = new JTMS[Datum, Fact, Rule](
    title,
    // nodeString = (n: Node[Datum, Fact, Rule]) => n.datum.fact.factToString,
    enqueueProcedure = Some(this.enqueue))

  /** Table of `DbClass`es. */
  val dbClassTable: DbClassTable = new HashMap[Any, Any]

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
  val queue: Queue[Rule] = Queue.empty

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
  def assert(fact: Fact, just: Justification[Datum, Fact, Rule]):
      Datum = {
    val factList: List[Fact] = fact match {
      case ListFact(l) => l
      case SymbolFact(_) => List(fact)
    }
    val datum = getReferent(fact)
    val node = datum.node
    dbgJtre(
      this,
      s"    Asserting (${factList.map(_.toString).mkString(", ")}) via $just"
    )

    jtms.justifyNode(
      factList.head,
      node,
      factList.tail.map((f) => getReferent(f).node))
    datum
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
  def quietAssert(fact: Fact, just: Just[Datum, Fact, Rule]): Datum =
    jtms.withoutContradictionCheck(() => assert(fact, just))

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
  def assume(fact: Fact, reason: Fact): Datum = {
    val datum = getReferent(fact)
    val node = datum.node
    if datum.isAssumption.isEmpty
    then {
      datum.isAssumption = Some(reason)
      dbgJtre(this, s"Assuming $fact via $reason")
      node.assumeNode
    }
    else if datum.isAssumption.map(_ != reason).getOrElse(true)
    then throw new TmsError(node,
      s"Fact ${datum.showDatum} assumed because of ${datum.isAssumption} assumed again because of ${reason}")
    datum
  }

  // (defmacro rassert! (fact just)
  //   `(assert! ,(quotize fact) ,(quotize just)))

  // ;;;; Retraction

  // (defmacro rretract! (fact &optional (just 'USER))
  //   `(retract! ,(quotize fact) ,(quotize just)))

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
    just: Justification[Datum, Fact, Rule] = UserStipulation,
    quiet: Boolean = false):
      Node[Datum, Fact, Rule] = {
    val datum = getReferent(fact)
    val node = datum.node

    if !node.isAssumption
    then {
      if !quiet then println(s"${datum.showDatum} is not an assumption")
    }
    else if !node.isInNode
    then {
      if !quiet then println(s"The assumption ${fact} is not currently in.")
    }
    else if datum.isAssumption.map(_ == just).getOrElse(false)
    then {
      dbgJtre(this, s"    Retracting $fact via $just")
      datum.isAssumption = None
      node.retractAssumption
    }
    else if !quiet
    then println(s"${just} not source of assumption for ${fact}")

    node
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
    just: Justification[Datum, Fact, Rule] = UserStipulation):
      Unit = {
    assert(fact, just)
    runRules
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
  def uAssume(fact: Fact, reason: Node[Datum, Fact, Rule]): Unit = {
    ???
    //assume(fact, reason)
    //runRules
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
(defun run-rules (&optional (*JTRE* *JTRE*))
  (do ((form (dequeue *JTRE*) (dequeue *JTRE*))
       (counter 0 (1+ counter)))
      ((null form)
       (debugging-jtre "~%    ~A rules run."  counter)
       (incf (jtre-rules-run *JTRE*) counter))
    (apply (car form) (cdr form))))
</pre>
    */
  def runRules: Unit = {
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
  def checkReferent(fact: Fact): Option[Datum] =
    getDbClass(fact).referent1(fact)

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
  def getReferent(fact: Fact): Datum =
    getDbClass(fact).insert(fact) match { case (datum, _) => datum }

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
  def isAlreadyAssumed(fact: Fact): Boolean = checkReferent(fact) match {
    case None => false
    case Some(r) => !r.isAssumption.isEmpty
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
  def contradiction(fact: Fact): Node[Datum, Fact, Rule] = {
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
  def why(fact: Fact): Node[Datum, Fact, Rule] = {
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
  def assumptionsOf(fact: Fact): Node[Datum, Fact, Rule] = {
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
  def sayDatumBelief(pr: Datum): Unit = {
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
  def getDbClass(fact: Fact): DbClass = {
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
  def mapDbClass(proc: (DbClass) => Unit): Unit = {
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
  def getTmsNode(fact: Fact): Node[Datum, Fact, Rule] = {
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
  def getDatum(num: Int): Datum = {
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
  def getJust(num: Int): Just[Datum, Fact, Rule] = {
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
  def getRule(num: Int): Rule = {
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
  def enqueue(rule: Rule): Unit = queue.enqueue(rule)

  /**
    *
    *
    *
    * **Translated from**:
    * <pre>
(defun dequeue (jtre) (pop (jtre-queue jtre)))
</pre>
    */
  def dequeue: Rule = queue.dequeue
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
inline def dbgJtre(jtre: JTRE, msg: String) =
  if jtre.debugging then println(msg)
