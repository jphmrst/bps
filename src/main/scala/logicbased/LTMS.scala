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

sealed trait Label
object UnknownLabel extends Label
object TrueLabel extends Label
object FalseLabel extends Label

object IsKnown {
  def unapply(l: Label): Option[Unit] =
    if l == TrueLabel || l == FalseLabel then Some(()) else None
}
object IsUnknown {
  def unapply(l: Label): Option[Unit] =
    if l != TrueLabel && l != FalseLabel then Some(()) else None
}

object IsTrue {
  def unapply(l: Label): Option[Unit] =
    if l == TrueLabel then Some(()) else None
}
object IsFalse {
  def unapply(l: Label): Option[Unit] =
    if l == FalseLabel then Some(()) else None
}

type Formula = Any

type ContradictionHandler[D, I, R] = Any

/** Standalone implementation of logic-based truth maintenance
  * systems.
  *
  * @param title Name of this TMS, for output.
  * @param nodeString Default formatter for TMS nodes.
  * @param debugging Debugging flag.
  * @param contradictionHandler External handler for detecting contradictions.
  * @param checkingContradictions For external systems.
  *
  * @tparam D Type of data associated with each [[Node]] of this LTMS.
  * @tparam I Type of informants in the external system.
  * @tparam R Type of rules which may be associated with each [[Node]]
  * of this LTMS.
  *
  * @constructor The `title` argument is required; others are optional.
  *
  * **Arguments and `val` members translated from**:
  * <pre>
(defstruct (ltms (:PRINT-FUNCTION print-ltms))
  (title nil)
  (node-counter 0)              ; unique namer for nodes.
  (clause-counter 0)            ; unique namer for justifications.
  (nodes nil)                   ; hash table for nodes.
  (clauses nil)                 ; list of all clauses.
  (debugging nil)               ; debugging flag
  (checking-contradictions t)
  (node-string nil)
  (contradiction-handlers nil)
  (pending-contradictions nil)
  (enqueue-procedure nil)
  (complete nil)                ; Is this a complete LTMS?
  (violated-clauses nil)
  (queue nil)                   ; Queue of clauses to resolve.
  (conses nil)                  ; Source of conses to reuse in inner loop.
  (delay-sat nil)               ; Don't resolve satisfied clauses.
  (cons-size 0))                ; Size of temporary structure.

(defun create-ltms (title &key (node-string 'default-node-string)
                    (debugging NIL)
                    (checking-contradictions T)
                    (contradiction-handler 'ask-user-handler)
                    (enqueue-procedure NIL)
                    (cache-datums? T)
                    (complete nil)
                    (delay-sat T)
                    &aux ltms)
   ;; The CACHE-DATUMS? flag is new in this version.  When used as
   ;; part of a larger system, the internal TMS cache tends to be redundant.
   ;; Creating an LTMS with this flag turned off avoids the storage overhead
   ;; of this redundancy, while still leaving a default mechanism in place
   ;; for experimentation and systems that choose to use it.
  (setq ltms
        (make-ltms :TITLE title
                   :NODES (if cache-datums? (make-hash-table :TEST #'equal))
                   :NODE-STRING node-string
                   :DEBUGGING debugging
                   :CHECKING-CONTRADICTIONS checking-contradictions
                   :ENQUEUE-PROCEDURE enqueue-procedure
                   :CONTRADICTION-HANDLERS (list contradiction-handler)
                   :DELAY-SAT delay-sat
                   :COMPLETE complete))
  ltms)

(defun change-ltms (ltms &key (contradiction-handler nil contra?)
                              node-string
                              enqueue-procedure
                              (debugging nil debugging?)
                              (checking-contradictions nil checking?)
                              (complete nil complete?)
                              (delay-sat nil delay-sat?))
  (if node-string (setf (ltms-node-string ltms) node-string))
  (if debugging? (setf (ltms-debugging ltms) debugging))
  (if checking? (setf (ltms-checking-contradictions ltms)
                      checking-contradictions))
  (if contra?
      (setf (ltms-contradiction-handlers ltms)
            (list contradiction-handler)))
  (if enqueue-procedure
      (setf (ltms-enqueue-procedure ltms) enqueue-procedure))
  (if complete? (setf (ltms-complete ltms) complete))
  (if delay-sat? (setf (ltms-delay-sat ltms) delay-sat)))
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
class LTMS[D, I, R](
  val title: String,
  val nodeString: (Node[D, I, R]) => String =
    (n: Node[D, I, R]) => s"${n.datum.toString()}",
  var debugging: Boolean = false,
  var checkingContradictions: Boolean = true,
  var enqueueProcedure: Option[(R) => Unit] = None,
  var contradictionHandler:
      Option[(LTMS[D, I, R], ListBuffer[Node[D, I, R]]) => Unit] = None
) {

  /** Unique namer for nodes.
    * @group internal
    */
  var nodeCounter: Int = 0

  /** Increment the node counter and return its value.
    * @group internal
    */
  def incrNodeCounter: Int = {
    val result = nodeCounter
    nodeCounter = nodeCounter + 1
    result
  }

  /** Unique namer for clauses.
    * @group internal
    */
  var clauseCounter: Int = 0

  /** Increment the clause counter and return its value.
    * @group internal
    */
  def incrClauseCounter: Int = {
    val result = clauseCounter
    clauseCounter = clauseCounter + 1
    result
  }

  /** List of all tms nodes.
    * @group internal
    */
  var nodes: ListBuffer[Node[D, I, R]] = ListBuffer.empty

  /** Main gateway for debugging messages.
    *
    * @param msg This debugging message.
    *
    * @group internal
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defmacro debugging-ltms (ltms msg &optional node &rest args)
  `(when (ltms-debugging ,ltms)
     (format *trace-output*
             ,msg (if ,node (node-string ,node)) ,@args)))
</pre>
    */
  inline def dbg(msg: String): Unit = if debugging then println(msg)

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun ltms-error (string &optional thing) (error string thing))
</pre>
    *
    */
  def ltmsError(string: String): Nothing = throw new LtmsError(string)

  /** Print the LTMS by name.
    *
    * @group interface
    *
    * **Translated from**:
    * <pre>
(defun print-ltms (ltms stream ignore)
  (declare (ignore ignore))
  (format stream "#<LTMS: ~A>" (ltms-title ltms)))
</pre>
    */
  def printLtms(): Unit = println(s"<LTMS: $title>")

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defmacro walk-clauses (ltms f)
  `(if (ltms-complete ,ltms)
       (walk-trie ,f (ltms-clauses ,ltms))
       (mapc ,f (ltms-clauses ,ltms))))
</pre>
    *
    */
  def walkClauses(f: Any): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun tms-create-node (ltms datum &key assumptionp)
  (if (and (ltms-nodes ltms) (gethash datum (ltms-nodes ltms)))
      (ltms-error "Two nodes with same datum:" datum))
  (let ((node (make-tms-node :INDEX (incf (ltms-node-counter ltms))
                             :DATUM datum
                             :ASSUMPTION? assumptionp
                             :LTMS ltms)))
    (setf (tms-node-true-literal node) (cons node :TRUE))
    (setf (tms-node-false-literal node) (cons node :FALSE))
    (if (ltms-nodes ltms) ;; Insert if locally caching
       (setf (gethash datum (ltms-nodes ltms)) node))
    (when (and (ltms-complete ltms)
               (> (ltms-node-counter ltms) (ltms-cons-size ltms)))
      (setf (ltms-conses ltms) nil)
      (incf (ltms-cons-size ltms) 50.)
      (dotimes (i (ltms-cons-size ltms))
        (push (cons nil nil) (ltms-conses ltms))))
    node))
</pre>
    *
    */
  def createNode(datum: D): Node[D, I, R] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun enable-assumption (node label)
  (cond ((not (tms-node-assumption? node))
         (ltms-error "Can't enable the non-assumption ~A" node))
        ((eq (tms-node-label node) label)
         (setf (tms-node-support node) :ENABLED-ASSUMPTION))
        ((eq (tms-node-label node) :UNKNOWN)
         (top-set-truth node label :ENABLED-ASSUMPTION))
        (t (ltms-error "Can't set an already set node" node))))
</pre>
    *
    */
  def enableAssumption(node: Node[D, I, R], label: Label): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun retract-assumption (node)
  (when (and (known-node? node)
             (eq (tms-node-support node) :ENABLED-ASSUMPTION))
    (find-alternative-support (tms-node-ltms node)
                              (propagate-unknownness node))))
</pre>
    *
    */
  def retractAssumption(node: Node[D, I, R]): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
;;; Adding formulas to the LTMS.
(defun add-formula (ltms formula &optional informant)
  (setq informant (list :IMPLIED-BY formula informant))
  (dolist (clause (normalize ltms formula))
    (unless (eq :TRUE (setq clause (simplify-clause clause)))
        (add-clause-internal clause informant T)))
  (check-for-contradictions ltms))
</pre>
    *
    */
  def addFormula(formula: Formula): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun find-node (ltms name)
  (cond ((typep name 'tms-node) name)
        ((if (ltms-nodes ltms) (gethash name (ltms-nodes ltms))))
        ((tms-create-node ltms name))))
</pre>
    *
    */
  def findNode(name: D | Node[D, I, R]): Node[D, I, R] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun generate-code (ltms run-tms informant &aux result bound datum)
  (maphash #'(lambda (ignore symbol)
               (when (or (tms-node-true-clauses symbol)
                         (tms-node-false-clauses symbol))
                 (setq datum (tms-node-datum symbol))
                 (when (listp datum)
                   (setf (tms-node-mark symbol) datum)
                   (setf (tms-node-datum symbol)
                         (make-symbol (format nil "~A" (cadr datum))))
                   (push symbol bound))))
           (ltms-nodes ltms))
  (walk-clauses ltms
                #'(lambda (clause &aux ps ns)
                    (dolist (lit (clause-literals clause))
                      (if (eq (cdr lit) :TRUE)
                          (push (tms-node-datum (car lit)) ps)
                          (push (tms-node-datum (car lit)) ns)))
                    (push `(add-clause `(,,@ps) `(,,@ns) ,informant)
                          result)))
  `(let ,(mapcar #'(lambda (s)
                     `(,(tms-node-datum s) (find-node ,run-tms ,(tms-node-mark s))))
                 bound)
     ,@result))
</pre>
    *
    */
  def generateCode(runTms: D | Node[D, I, R], informant: I): () => Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun add-clause (true-nodes false-nodes &optional informant)
  (add-clause-internal (nconc (mapcar #'tms-node-true-literal true-nodes)
                              (mapcar #'tms-node-false false-nodes))
                       informant
                       nil))
</pre>
    *
    */
  def addClause(
    trueNodes: ListBuffer[Node[D, I, R]],
    falseNodes: ListBuffer[Node[D, I, R]],
    informant: Option[I] = None
  ): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun add-clause-internal (literals informant internal &aux ltms)
  (setq ltms (tms-node-ltms
               (or (caar literals)
                   (ltms-error "Total contradiction: Null clause" informant))))
  (if (ltms-complete ltms)
      (full-add-clause ltms literals informant)
      (push (bcp-add-clause ltms literals informant)
            (ltms-clauses ltms)))
  (unless internal (check-for-contradictions ltms)))
</pre>
    *
    */
  def addClauseInternal(
    literals: List[Literal[D, I, R]], informant: Option[I], internal: Boolean):
      Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun bcp-add-clause (ltms literals informant &optional (index T)
                                               &aux cl label)
  (setq cl (make-clause :INDEX (incf (ltms-clause-counter ltms))
                        :LITERALS literals
                        :INFORMANT informant
                        :LENGTH (length literals)))
  (dolist (term literals)
    (if (eq :UNKNOWN (setq label (tms-node-label (car term))))
        (incf (clause-pvs cl)))
    (ecase (cdr term)
      (:TRUE
        (if index (insert-true-clause cl (car term)))
        (when (eq label :TRUE)
          (incf (clause-sats cl)) (incf (clause-pvs cl))))
      (:FALSE
       (if index (insert-false-clause cl (car term)))
       (when (eq label :FALSE)
         (incf (clause-sats cl)) (incf (clause-pvs cl))))))
  (if index (check-clauses ltms (list cl)))
  cl)
</pre>
    *
    */
  def bcpAddClause(
    literals: List[Literal[D, I, R]],
    informant: I, index: Boolean | Int = true):
      Clause[D, I, R] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun add-nogood (culprit sign assumptions &aux trues falses)
  (dolist (a assumptions (add-clause trues falses 'NOGOOD))
    (ecase (if (eq a culprit) sign (tms-node-label a))
      (:TRUE (push a falses))
      (:FALSE (push a trues)))))
</pre>
    *
    */
  def addNogood(
    culprit: Node[D, I, R],
    sign: Label,
    assumptions: ListBuffer[Node[D, I, R]]):
      Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(proclaim '(special *clauses-to-check*))

(defun check-clauses (ltms *clauses-to-check*)
  (debugging-ltms ltms "~% Beginning propagation...")
  (do nil ((null *clauses-to-check*))
    (check-clause ltms (pop *clauses-to-check*))))
</pre>
    *
    */
  def checkClauses: Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun check-clause (ltms clause &aux unknown-pair)
  (cond ((violated-clause? clause)
         (pushnew clause (ltms-violated-clauses ltms)))
        ((= (clause-pvs clause) 1)
         ;; Exactly one term of the clause remains that can
         ;; satisfy the clause, so deduce that term
         (setq unknown-pair (find-unknown-pair clause))
         (when unknown-pair ;must check, because it might have other
           (set-truth (car unknown-pair) ; support
                      (cdr unknown-pair) clause)))))
</pre>
    *
    */
  def checkClause(clause: Clause[D, I, R]): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun propagate-unknownness (in-node)
  (let (node old-value node2 unknown-queue ltms)
    (setq ltms (tms-node-ltms in-node))
    (do ((forget-queue (cons in-node nil) (nconc forget-queue new))
         (new nil nil))
        ((null forget-queue) unknown-queue)
      (setq forget-queue (prog1 (cdr forget-queue)
                                (rplacd forget-queue unknown-queue)
                                (setq unknown-queue forget-queue))
            node (car unknown-queue))
      (debugging-ltms ltms "~% Retracting ~A." node)
      (setq old-value (tms-node-label node))
      (setf (tms-node-label node) :UNKNOWN)
      (setf (tms-node-support node) nil)
      (dolist (clause (ecase old-value
                        (:TRUE (tms-node-false-clauses node))
                        (:FALSE (tms-node-true-clauses node))))
        (when (= (incf (clause-pvs clause)) 2)
          (when (setq node2 (clause-consequent clause))
            (push node2 new))))
      (if (ltms-complete ltms)
          (propagate-more-unknownness old-value node ltms)))))
</pre>
    *
    */
  def propagateUnknownness(inNode: Node[D, I, R]): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun find-alternative-support (ltms nodes)
  (dolist (node nodes)
    (when (unknown-node? node)
      (check-clauses ltms (tms-node-true-clauses node))
      (check-clauses ltms (tms-node-false-clauses node))))
  (if (eq T (ltms-complete ltms)) (ipia ltms)))
</pre>
    *
    */
  def findAlternativeSupport(nodes: List[Node[D, I, R]]): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun check-for-contradictions (ltms &aux violated-clauses)
  (setq violated-clauses
        (delete-if-not #'(lambda (c) (violated-clause? c))
                       (ltms-violated-clauses ltms)))
  (setf (ltms-violated-clauses ltms) violated-clauses) ;; Cache them.
  (if violated-clauses (contradiction-handler ltms violated-clauses)))
</pre>
    *
    */
  def checkForContradictions: Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun contradiction-handler (ltms violated-clauses)
  (cond ((not (ltms-checking-contradictions ltms))
         ;; Update cache of violated clauses
         (setf (ltms-pending-contradictions ltms)
               (delete-if-not #'(lambda (c) (violated-clause? c))
                  (ltms-pending-contradictions ltms)))
         (dolist (vc violated-clauses)
            (when (violated-clause? vc)
               (pushnew vc (ltms-pending-contradictions ltms)))))
        (t (dolist (handler (ltms-contradiction-handlers ltms))
             (if (funcall handler violated-clauses ltms) (return T))))))
</pre>
    *
    */
  def contradictonHandler(violatedClauses: List[Clause[D, I, R]]): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defmacro without-contradiction-check (ltms &body body)
  (contradiction-check ltms nil body))
</pre>
    *
    */
  inline def withoutContradictionCheck(body: () => Unit): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defmacro with-contradiction-check (ltms &body body)
  (contradiction-check ltms t body))
</pre>
    *
    */
  inline def withContradictionCheck(body: () => Unit): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun contradiction-check (ltms flag body)
  `(let* ((.ltms. ,ltms)
          (.old-value. (ltms-checking-contradictions .ltms.)))
     (unwind-protect
         (progn (setf (ltms-checking-contradictions .ltms.) ,flag)
                ,@body)
       (setf (ltms-checking-contradictions .ltms.) .old-value.))))
</pre>
    *
    */
  def contradictionCheck(flag: Boolean, body: () => Unit): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defmacro with-contradiction-handler (ltms handler &body body)
  `(let ((.ltms. ,ltms))
     (unwind-protect
         (progn (push ,handler (ltms-contradiction-handlers .ltms.))
                ,@ body)
       (pop (ltms-contradiction-handlers .ltms.)))))
</pre>
    *
    */
  inline def withContradictionHandler(
    handler: ContradictionHandler[D, I, R], body: () => Unit): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defmacro with-assumptions (assumption-values &body body)
  ;; Allows assumptions to be made safely, and retracted properly
  ;; even if non-local exits occur.
  `(unwind-protect (progn (dolist (av ,assumption-values)
                            (enable-assumption (car av) (cdr av)))
                         ,@ body)
     (dolist (av ,assumption-values) (retract-assumption (car av)))))
</pre>
    *
    */
  inline def withAssumptions(assumptionValues: List[(Node[D, I, R], Label)]):
      Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(proclaim '(special *contra-assumptions*))

(defun ask-user-handler (contradictions ltms)
  (declare (ignore ltms))
  (dolist (contradiction contradictions)
    (if (violated-clause? contradiction)
        (handle-one-contradiction contradiction))))
</pre>
    *
    */
  def askUserHandler(contradictions: List[Node[D, I, R]]): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun handle-one-contradiction (violated-clause)
   (let ((*contra-assumptions* (assumptions-of-clause violated-clause))
         (the-answer nil))
      (unless *contra-assumptions* (ltms-error "Global contradiction"
                                    violated-clause))
      (format t "~%Contradiction found:")
      (print-contra-list *contra-assumptions*)
      (format t "~%Call (TMS-ANSWER <number>) to retract assumption.")
      (setq the-answer
         (catch 'tms-contradiction-handler
            (cerror "Continue LTRE processing (after retracting an assumption)"
               "LTMS contradiction break")))
      (if the-answer
         (retract-assumption (nth (1- the-answer)
                                *contra-assumptions*)))))
</pre>
    *
    */
  def handleOneContradiction(violatedClauses: List[Clause[D, I, R]]): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun print-contra-list (nodes)
  (do ((counter 1 (1+ counter))
       (nn nodes (cdr nn)))
      ((null nn))
    (format t "~%~A ~A" counter
            (node-string (car nn)))))
</pre>
    *
    */
  def printContraList(nodes: List[Node[D, I, R]]) = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun tms-answer (num)
  (if (integerp num)
      (if (> num 0)
          (if (not (> num (length *contra-assumptions*)))
              (throw 'tms-contradiction-handler num)
              (format t "~%Ignoring answer, too big."))
          (format t "~%Ignoring answer, too small"))
      (format t "~%Ignoring answer, must be an integer.")))
</pre>
    *
    */
  def answer(num: Int) = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun avoid-all (contradictions ignore &aux culprits culprit sign)
  (dolist (contradiction contradictions)
    (when (violated-clause? contradiction)
      (unless (setq culprits (assumptions-of-clause contradiction))
        (ltms-error "Total contradiction" contradiction))
      (setq culprit (car culprits)
            sign (tms-node-label culprit))
      (retract-assumption culprit)
      (add-nogood culprit sign culprits)
      t)))
</pre>
    *
    */
  def avoidAll(contradictions: List[Clause[D, I, R]]): Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun why-nodes (ltms)
  (maphash #'(lambda (ignore n) (why-node n)) (ltms-nodes ltms)))
</pre>
    *
    */
  def whyNodes: Unit = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun pretty-print-clauses (ltms)
  (walk-clauses ltms #'(lambda (l)
                         (format T "~% ")
                         (pretty-print-clause l))))
</pre>
    *
    */
  def prettyPrintClauses: Unit = ???

} // class LTMS

class LtmsError(msg: String) extends RuntimeException(msg)
