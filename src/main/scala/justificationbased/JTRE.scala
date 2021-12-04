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

package org.maraist.truthmaintenancesystems.justificationbased
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}

type Fact = Matchable

class JTRE[I](val title: String, val debugging: Boolean = false) {

  /** Pointer to its JTMS. */
  val theJtms: JTMS[I] = new JTMS[I](title,
    nodeString = (n: Node[I]) => n.viewNode.toString,
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

  // (defstruct (jtre (:PRINT-FUNCTION jtre-printer))
  //   title                   ; Pretty name
  //   jtms                    ; Pointer to its JTMS
  //   (dbclass-table nil)     ; Table of dbclasses
  //   (datum-counter 0)       ; Unique ID for asserts
  //   (rule-counter 0)        ; Unique ID for rules
  //   (debugging nil)         ; If non-NIL, show basic operations
  //   (queue nil)             ; Rule queue
  //   (rules-run 0))          ; Statistic

  // (defun create-jtre (title &key debugging)
  //  (let ((j (make-jtre
  //       :TITLE title
  //       :JTMS (create-jtms (list :JTMS-OF title)
  //                          :NODE-STRING 'view-node)
  //       :DBCLASS-TABLE (make-hash-table :TEST #'eq)
  //       :DEBUGGING debugging)))
  //    (change-jtms (jtre-jtms j)
  //            :ENQUEUE-PROCEDURE
  //            #'(lambda (rule) (enqueue rule j)))
  //    j))

  override def toString: String = s"<JTRE: $title>"
  def jtrePrinter: Unit = print(toString)
  // (defun jtre-printer (j st ignore)
  //   (format st "<JTRE: ~A>" (jtre-title j)))

  // (defun change-jtre (jtre &key (debugging :NADA))
  //   (unless (eq debugging :NADA)
  //      (setf (jtre-debugging jtre) debugging)))

  //
  // ;;;; Making statements

  def assert(fact: Fact, just: Justification[I]): Datum[I] = {
    val datum = referent(fact, true).get
    val node = datum.node
    dbgJtre(this, s"    Asserting ${fact} via ${just}.")
    // theJtms.justifyNode(just, node,
    ??? // TODO Come back to this.
  }
  // ;; From jdata.lisp
  // (defun assert! (fact just &optional (*JTRE* *JTRE*) &aux datum node)
  //   (setq datum (referent fact t)
  //         node (datum-tms-node datum))
  //   (unless (listp just) (setq just (list just)))
  //   (debugging-jtre "~%    Asserting ~A via ~A." fact just)
  //   (justify-node (car just) node
  //            (mapcar #'(lambda (f) (datum-tms-node (referent f t)))
  //                    (cdr just)))
  //   datum)

  def quietAssert(fact: Fact, just: Just[I]): Datum[I] = {
    ???
  }
  // ;; From jdata.lisp
  // (defun quiet-assert! (fact just &optional (*JTRE* *JTRE*))
  //   (without-contradiction-check (jtre-jtms *JTRE*) (assert! fact just)))

  def assume(fact: Fact, reason: Node[I]): Datum[I] = {
    ???
  }
  // ;; From jdata.lisp
  // (defun assume! (fact reason &optional (*JTRE* *JTRE*) &aux datum node)
  //   (setq datum (referent fact t)
  //    node (datum-tms-node datum))
  //   (cond    ((not (datum-assumption? datum))
  //     (setf (datum-assumption? datum) reason)
  //     (debugging-jtre "~%    Assuming ~A via ~A." fact reason)
  //     (assume-node node))
  //    ((eq reason (datum-assumption? datum)))
  //    (t (error
  //        "Fact ~A assumed because of ~A assumed again because of ~A"
  //        (show-datum datum) (datum-assumption? datum) reason)))
  //   datum)

  def retract(
    fact: Fact,
    just: Justification[I] = Symbol("user"),
    quiet: Boolean = false):
      Node[I] = {
    ???
  }
  // ;; From jdata.lisp
  // (defun retract! (fact &optional (just 'user) (quiet? nil)
  //                  (*JTRE* *JTRE*) &aux datum node)
  //   (setq datum (referent fact t)
  //    node (datum-tms-node datum))
  //   (cond ((not (tms-node-assumption? node))
  //     (unless quiet?
  //       (format t "~%~A isn't an assumption."
  //               (show-datum datum))))
  //    ((not (in-node? node))
  //     (unless quiet?
  //       (format T
  //         "~%The assumption ~A is not currently in."
  //         fact)))
  //    ((eq just (datum-assumption? datum))
  //     (debugging-jtre "~%    Retracting ~A via ~A."
  //                     fact just)
  //     (setf (datum-assumption? datum) nil)
  //     (retract-assumption node))
  //    ((not quiet?)
  //     (format t "~%~A not source of assumption for ~A"
  //             just fact)))
  //   node)

  def uAssert(
    fact: Fact,
    just: Justification[I] = Symbol("user")):
      Unit = {
    ???
  }
  // ;; From jdata.lisp
  // (defun uassert! (fact &optional (just 'user))
  //   (assert! fact just) ;; Do internal operation
  //   (run-rules *JTRE*))        ;; Run the rules

  def uAssume(fact: Fact, reason: Node[I]): Unit = {
    ???
  }
  // ;; From jdata.lisp
  // (defun uassume! (fact reason) ;; Similar to UASSERT!
  //   (assume! fact reason *JTRE*)
  //   (run-rules *JTRE*))

  // Does not seem to be used anywhere — leaving untranslated. [JM]
  //
  // ;; From jdata.lisp
  // (defun run-forms (forms &optional (*JTRE* *JTRE*))
  //   (dolist (form forms) (eval form) (run-rules *JTRE*)))

  def run: Unit = {
    ???
  }
  // ;; From jdata.lisp
  // (defun run (&optional (*JTRE* *JTRE*)) ;; Toplevel driver function
  //     (format T "~%>>")
  //     (do ((form (read) (read)))
  //         ((member form '(quit stop exit abort)) nil)
  //         (format t "~%~A" (eval form))
  //         (run-rules)
  //         (format t "~%>>")))

  def show: Unit = {
    ???
  }
  // ;; From jdata.lisp
  // (defun show (&optional (*JTRE* *JTRE*) (stream *standard-output*))
  //   (show-data *JTRE* stream) (show-rules *JTRE* stream))

  def referent(fact: Fact): Option[Datum[I]] = referent(fact, false)
  def referent(fact: Fact, isVirtual: Boolean): Option[Datum[I]] = {
    ???
  }
  // ;; From jdata.lisp
  // (defun referent (fact &optional (virtual? nil)
  //                  (*JTRE* *JTRE*))
  //   (if virtual? (insert fact) (referent1 fact)))

  def isAlreadyAssumed(fact: Fact): Boolean = referent(fact) match {
    case None => false
    case Some(r) => r.isAssumption
  }
  // (defun already-assumed? (fact  &aux r)
  //   (when (setq r (referent fact))
  //     (datum-assumption? r)))

  def contradiction(fact: Fact): Node[I] = {
    ???
  }
  // ;; From jdata.lisp
  // (defun contradiction (fact &optional (*JTRE* *JTRE*))
  //   (make-contradiction (datum-tms-node (referent fact t))))
  // 
  // ;;;; Interface and display of data

  def isIn(fact: Fact): Boolean = {
    ???
  }
  // ;; From jdata.lisp
  // (defun in? (fact &optional (*JTRE* *JTRE*) &aux r)
  //   (when (setq r (referent fact))
  //    (in-node? (datum-tms-node r))))

  def isOut(fact: Fact): Boolean = {
    ???
  }
  // ;; From jdata.lisp
  // (defun out? (fact &optional (*JTRE* *JTRE*) &aux r)
  //   (or (not (setq r (referent fact))) ; a non-existent fact is out
  //       (out-node? (datum-tms-node r))))

  def why(fact: Fact): Node[I] = {
    ???
  }
  // ;; From jdata.lisp
  // (defun why? (fact &optional (*JTRE* *JTRE*) &aux r)
  //   (when (setq r (referent fact))
  //    (why-node (datum-tms-node r))))

  def assumptionsOf(fact: Fact): Node[I] = {
    ???
  }
  // ;; From jdata.lisp
  // (defun assumptions-of (fact &optional (*JTRE* *JTRE*))
  //   (mapcar #'view-node
  //      (assumptions-of-node
  //       (datum-tms-node (referent fact *jtre* t)))))

  // Only called from some examples — leaving untranslated for now. [JM]
  //
  // ;; From jdata.lisp
  // (defun fetch (pattern &optional (*JTRE* *JTRE*) &aux bindings unifiers)
  //   (dolist (candidate (get-candidates pattern) unifiers)
  //     (setq bindings (unify pattern (datum-lisp-form candidate)))
  //     (unless (eq bindings :FAIL)
  //       (push (sublis bindings pattern) unifiers))))

  // ;;;; More display-intensive procedures

  // Does not seem to be used anywhere — leaving untranslated. [JM]
  //
  // ;; From jdata.lisp
  // (defun wfs (fact &optional (*JTRE* *JTRE*))
  //   ;; Displays well-founded support for a fact
  //   (cond ((out? fact) (format t "~% ~A is OUT." fact))
  //    (t (do ((queue (list (get-tms-node fact))
  //                   (nconc (cdr queue) new-antes))
  //            (so-far (list (get-tms-node fact)))
  //            (new-antes nil nil))
  //           ((null queue) (format t "~%--------") fact)
  //         (why-node (car queue))
  //         (unless (or (out-node? (car queue))
  //                     (tms-node-assumption? (car queue)))
  //           ;; Go down the support
  //           (dolist (ante (just-antecedents
  //                          (tms-node-support (car queue))))
  //             (unless (member ante so-far)
  //               (push ante so-far)
  //               (push ante new-antes))))))))

  def sayDatumBelief(pr: Datum[I]): Unit = {
    ???
  }
  // ;; From jdata.lisp
  // (defun say-datum-belief (pr &optional (*jtre* *jtre*)
  //                        (indent ""))
  //   (format t "~%~A~A: ~A" indent pr
  //      (if (in-node? (get-tms-node pr *jtre*))
  //          "IN" "OUT")))

  def showJustifications(fact: Fact): Unit = {
    ???
  }
  // ;; From jdata.lisp
  // (defun show-justifications (fact &optional (*jtre* *jtre*))
  //   (format t "~% ~A::" fact)
  //   (let* ((node (get-tms-node fact *jtre*))
  //     (justs (tms-node-justs node)))
  //     (unless justs
  //        (format t " No justifications.")
  //        (return-from show-justifications node))
  //     (dolist (j justs)
  //        (format t "~% ~A" (just-informant j))
  //        (cond ((just-antecedents j)
  //               (format t ", on:")
  //               (dolist (ante (just-antecedents j))
  //                       (say-datum-belief
  //                        (view-node ante) *jtre* "  "))
  //               (format t "."))
  //              (t (format t "."))))))

  def showData: Unit = {
    ???
  }
  // ;; From jdata.lisp
  // (defun show-data (&optional (*JTRE* *JTRE*)
  //                        (stream *standard-output*))
  //   (format stream
  //      "~%~D facts total." (jtre-datum-counter *JTRE*))
  //   (map-dbclass
  //    #'(lambda (dbclass)
  //        (dolist (datum (dbclass-facts dbclass))
  //           (format stream "~%~A: ~A" (show-datum datum)
  //                   (if (in-node? (datum-tms-node datum))
  //                       "IN" "OUT"))))))

  // ;;;; Database system

  def getDbClass(fact: Fact): DbClass[I] = {
    ???
  }
  // ;; From jdata.lisp
  // (defun get-dbclass (fact &optional (*JTRE* *JTRE*)
  //                     &aux dbclass)
  //   (cond ((null fact) (error "~% NIL can't be a dbclass."))
  //    ((listp fact) (get-dbclass (car fact) *JTRE*))
  //    ((variable? fact)
  //     (cond ((boundp fact)
  //            (get-dbclass (symbol-value fact) *JTRE*))
  //           (t (error "~%Dbclass unbound: ~A" fact))))
  //    ((symbolp fact)
  //     (cond ((setq dbclass
  //                  (gethash fact
  //                           (jtre-dbclass-table *JTRE*)))
  //            dbclass)
  //           (t (setq dbclass
  //                    (make-dbclass :NAME fact :FACTS nil
  //                                :RULES nil :JTRE *JTRE*))
  //              (setf (gethash fact
  //                     (jtre-dbclass-table *JTRE*))
  //                    dbclass)
  //              dbclass)))
  //    (t (error "Bad dbclass type: ~A" fact))))

  def mapDbClass(proc: (DbClass[I]) => Unit): Unit = {
    ???
  }
  // ;; From jdata.lisp
  // (defun map-dbclass (proc &optional (*JTRE* *JTRE*))
  //   (maphash #'(lambda (name dbclass) (declare (ignore name))
  //           (funcall proc dbclass))
  //       (jtre-dbclass-table *JTRE*)))

  def getTmsNode(fact: Fact): Node[I] = {
    ???
  }
  // ;; From jdata.lisp
  // (defun get-tms-node (fact &optional (*JTRE* *JTRE*))
  //   (datum-tms-node (referent fact t)))

  def getDatum(num: Int): Datum[I] = {
    ???
  }
  // ;; From jdata.lisp
  // (defun get-datum (num &optional (*JTRE* *JTRE*))
  //   (map-dbclass
  //    #'(lambda (dbclass)
  //        (dolist (datum (dbclass-facts dbclass))
  //           (when (= (datum-id datum) num)
  //                 (return-from GET-DATUM datum))))))

  def getJust(num: Int): Just[I] = {
    ???
  }
  // ;; From jdata.lisp
  // (defun get-just (num &optional (*JTRE* *JTRE*))
  //   (dolist (just (jtms-justs (jtre-jtms *JTRE*)))
  //     (when (= (just-index just) num)
  //      (return-from GET-just just))))

  def showRules: Unit = {
    ???
  }
  // (defun show-rules (&optional (*JTRE* *JTRE*) (stream *standard-output*))
  //   (format t "~%There are ~D rules in ~A:"
  //      (jtre-rule-counter *JTRE*) (jtre-title *JTRE*))
  //   (format stream "~% ~A queued." (if (null (jtre-queue *JTRE*)) "None"
  //                               (length (jtre-queue *JTRE*))))
  //   (map-dbclass #'(lambda (dbclass)
  //             (dolist (rule (dbclass-rules dbclass))
  //                     (print-rule rule stream)))))

  def getRule(num: Int): Rule[I] = {
    ???
  }
  // (defun get-rule (num &optional (*JTRE* *JTRE*))
  //   (map-dbclass #'(lambda (dbclass)
  //             (dolist (rule (dbclass-rules dbclass))
  //                     (when (= (rule-id rule) num)
  //                           (return-from GET-RULE rule))))))

  // Called from some examples only — leaving untranslated for now. [JM]
  //
  // (defun run-rules (&optional (*JTRE* *JTRE*))
  //   (do ((form (dequeue *JTRE*) (dequeue *JTRE*))
  //        (counter 0 (1+ counter)))
  //       ((null form)
  //        (debugging-jtre "~%    ~A rules run."  counter)
  //        (incf (jtre-rules-run *JTRE*) counter))
  //     (apply (car form) (cdr form))))

  def rulesWaiting: Boolean = !queue.isEmpty
  // (defun rules-waiting? (jtre) (jtre-queue jtre))

  def enqueue(rule: Rule[I]): Unit = queue.enqueue(rule)
  // (defun enqueue (new j) (push new (jtre-queue j)))

  def dequeue: Rule[I] = queue.dequeue
  // (defun dequeue (jtre) (pop (jtre-queue jtre)))

  // Not relevant in Scala translation: methods now called against a
  // particular object, not a special global variable.
  //
  // (defvar *JTRE* nil)
  // (defmacro With-Jtre (jtre &rest forms)
  //   `(let ((*JTRE* ,jtre)) ,@ forms))
  // (defun In-Jtre (jtre) (setq *JTRE* jtre))
}

inline def dbgJtre[I](jtre: JTRE[I], msg: String) =
  if jtre.debugging then println(msg)
// (defmacro debugging-jtre (msg &rest args)
//   `(when (jtre-debugging *JTRE*) (format t ,msg  ,@args)))
