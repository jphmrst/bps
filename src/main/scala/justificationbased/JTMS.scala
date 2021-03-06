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
import scala.util.control.NonLocalReturns.*
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}

/** Standalone implementation of justification-based truth maintenance
  * systems.
  *
  * @param title Name of this TMS, for output.
  * @param nodeString Default formatter for TMS nodes.
  * @param debugging Debugging flag.
  * @param contradictionHandler External handler for detecting contradictions.
  * @param checkingContradictions For external systems.
  *
  * @tparam D Type of data associated with each [[Node]] of this JTMS.
  * @tparam I Type of informants in the external system.
  * @tparam R Type of rules which may be associated with each [[Node]]
  * of this JTMS.
  *
  * @constructor The `title` argument is required; others are optional.
  *
  * **Arguments and `val` members translated from**:
  * <pre>
(defstruct (jtms (:PRINT-FUNCTION print-jtms))
  (title nil)
  (node-counter 0)             ;; unique namer for nodes.
  (just-counter 0)             ;; unique namer for justifications.
  (nodes nil)                  ;; list of all tms nodes.
  (justs nil)                  ;; list of all justifications
  (debugging nil)              ;; debugging flag
  (contradictions nil)         ;; list of contradiction nodes.
  (assumptions nil)            ;; list of assumption nodes.
  (checking-contradictions T)  ;; For external systems
  (node-string nil)
  (contradiction-handler nil)
  (enqueue-procedure nil))

(defun create-jtms (title &key (node-string 'default-node-string)
                               debugging
                               (checking-contradictions t)
                               (contradiction-handler 'ask-user-handler)
                               enqueue-procedure)
  (make-jtms :TITLE title
        :NODE-STRING node-string
        :DEBUGGING debugging
        :CHECKING-CONTRADICTIONS checking-contradictions
        :CONTRADICTION-HANDLER contradiction-handler
        :ENQUEUE-PROCEDURE enqueue-procedure
        ))

(defun change-jtms (jtms &key contradiction-handler node-string
                         enqueue-procedure debugging
                              checking-contradictions)
  (if node-string (setf (jtms-node-string jtms) node-string))
  (if debugging (setf (jtms-debugging jtms) debugging))
  (if checking-contradictions
      (setf (jtms-checking-contradictions jtms)
       checking-contradictions))
  (if contradiction-handler
      (setf (jtms-contradiction-handler jtms) contradiction-handler))
  (if enqueue-procedure
      (setf (jtms-enqueue-procedure jtms) enqueue-procedure)))
</pre>
  *
  * @groupname interface Interface methods
  * @groupdesc interface Top-level methods for control of the JTMS
  * from an external system.
  * @groupprio interface 1
  *
  * @groupname diagnostic Diagnostic and debugging methods
  * @groupdesc diagnostic Reporting the current JTMS state as text.
  * @groupprio diagnostic 2
  *
  * @groupname internal Internal methods
  * @groupdesc internal Implementation methods; not generally for use
  * from outside this package.
  * @groupprio internal 10
  */
class JTMS[D, I, R](
  val title: String,
  val nodeString: (Node[D, I, R]) => String =
    (n: Node[D, I, R]) => s"${n.datum.toString()}",
  var debugging: Boolean = false,
  var checkingContradictions: Boolean = true,
  var enqueueProcedure: Option[(R) => Unit] = None,
  var contradictionHandler:
      Option[(JTMS[D, I, R], ListBuffer[Node[D, I, R]]) => Unit] = None
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

  /** Unique namer for justifications.
    * @group internal
    */
  var justCounter: Int = 0
  /** Increment the justifications counter and return its value.
    * @group internal
    */
  def incrJustCounter: Int = {
    val result = justCounter
    justCounter = justCounter + 1
    result
  }

  /** List of all tms nodes.
    * @group internal
    */
  var nodes: ListBuffer[Node[D, I, R]] = ListBuffer.empty

  /** List of all justifications.
    * @group internal
    */
  var justs: ListBuffer[Just[D, I, R]] = ListBuffer.empty

  /** List of contradiction nodes.
    * @group internal
    */
  var contradictions: ListBuffer[Node[D, I, R]] = ListBuffer.empty

  /** List of assumption nodes.
    * @group internal
    */
  var assumptions: ListBuffer[Node[D, I, R]] = ListBuffer.empty

  /** Main gateway for debugging messages.
    *
    * @param msg This debugging message.
    *
    * @group internal
    *
    * **Translated from**:
    * <pre>
(defmacro debugging-jtms (jtms msg &optional node &rest args)
  `(when (jtms-debugging ,jtms)
     (format *trace-output* ,msg (if ,node (node-string ,node)) ,@args)))
</pre>
    */
  inline def dbg(msg: String): Unit = if debugging then println(msg)

  /** Print the JTMS by name.
    *
    * @group interface
    *
    * **Translated from**:
    * <pre>
(defun print-jtms (jtms stream ignore)
  (declare (ignore ignore))
  (format stream "#<JTMS: ~A>" (jtms-title jtms)))
</pre>
    */
  def printJtms(): Unit = println(s"<JTMS: $title>")

  /** Create a new node in this JTMS.
    *
    * @group interface
    *
    * @param datum The piece of data associated with the node.
    * @param assumptionP True indicates that this node might be used
    * as an assumption.  But note that an assumption node must be
    * enabled before a judgment can use the node as a premise.  The
    * default value is `false`.
    * @param contradictionP True indicates that this node denotes a
    * contradiction.  The default value is `false`.
    *
    * **Translated from**:
    * <pre>
(defun tms-create-node (jtms datum &key assumptionp contradictoryp)
  (let ((node (make-tms-node :INDEX (incf (jtms-node-counter jtms))
                        :DATUM datum
                        :ASSUMPTION? assumptionp
                        :CONTRADICTORY? contradictoryp
                        :JTMS jtms)))
    (if assumptionp (push node (jtms-assumptions jtms)))
    (if contradictoryp (push node (jtms-contradictions jtms)))
    (push node (jtms-nodes jtms))
    node))
</pre>
    */
  def createNode(
    datum: D,
    assumptionP: Boolean = false,
    contradictionP: Boolean = false):
      Node[D, I, R] = {
    val node =
      new Node[D, I, R](datum, this, assumptionP, contradictionP)
    if assumptionP then assumptions += node
    if contradictionP then contradictions += node
    nodes += node
    node
  }

  /** Add a rule for concluding belief in a node.
    *
    * @group interface
    *
    * @param informant Information value associated with this
    * justification.
    * @param consequence Node concluded by ths justification.
    * @param antecedents The premises required to trigger belief in
    * the `consequence.
    *
    * **Translated from**:
    * <pre>
(defun justify-node (informant consequence antecedents &aux just jtms)
  (setq jtms (tms-node-jtms consequence)
   just (make-just :INDEX (incf (jtms-just-counter jtms))
                   :INFORMANT informant
                   :CONSEQUENCE consequence
                   :ANTECEDENTS antecedents))
  (push just (tms-node-justs consequence))
  (dolist (node antecedents) (push just (tms-node-consequences node)))
  (push just (jtms-justs jtms))
  (debugging-jtms jtms
             "~%Justifying ~A by ~A using ~A."
             consequence
             informant
             (mapcar #'node-string antecedents))
  (if (or antecedents (out-node? consequence))
      (if (check-justification just) (install-support consequence just))
      (setf (tms-node-support consequence) just))
  (check-for-contradictions jtms))
</pre>
    */
  def justifyNode(
    informant: I,
    consequence: Node[D, I, R],
    antecedents: List[Node[D, I, R]]):
      Unit = {
    // Create the structure to represent this inference rule.
    val just =
      new Just[D, I, R](incrJustCounter, informant, consequence, antecedents)

    // Associate the new justification structure with possible
    // justifiers of the consequence.
    consequence.justs += just

    // For each of the antecedents, include the new justification
    // structure as a consequence.
    for (node <- antecedents) do node.consequences += just

    // Add the new justification structure to the master list of
    // justifications.
    justs += just

    // If debugging
    dbg({
      val antes = antecedents.map(nodeString)
      s"Justifying $consequence by $informant using ${antes}."
    })

    // We attempt to use this new rule right now if either the
    // consequence is currently OUT, or if there actually are
    // antecedents.
    if !antecedents.isEmpty || consequence.isOutNode then {
      // If the antecedents are satisfied, add it as a support for the
      // consequence.
      if just.checkJustification then consequence.installSupport(just)
    } else {
      // Otherwise we can install as a support straightaway.
      consequence.support = Some(just)
    }

    // Detect new contradictions.
    checkForContradictions
  }

  /** Search for support for nodes which were disbelieved after an
    * assumption retraction.
    *
    * The original Lisp code returns the justification when
    * short-circuiting from the inner loop.  But this return value is
    * never used; moreover there is no return value used from callers
    * of this function.  So in this type-checked translation, we
    * return the unit value.
    *
    * @group internal
    *
    * @param outQueue List of nodes which have lost support.  The
    * naming of the parameter as a queue in the Lisp code is odd: the
    * list is only read; nothing is ever enqueued.
    *
    * **Translated from**:
    * <pre>
(defun find-alternative-support (jtms out-queue)
  (debugging-jtms jtms "~%   Looking for alternative supports.")
  (dolist (node out-queue)
    (unless (in-node? node)
      (dolist (just (tms-node-justs node))
        (when (check-justification just)
          (install-support (just-consequence just) just)
          (return just))))))
</pre>
    */
  def findAlternativeSupport(outQueue: Iterable[Node[D, I, R]]):
      Unit = { // Option[Just[D, I, R]] = {
    dbg(s"   Looking for alternative supports for ${outQueue.map(_.datum.toString).mkString(", ")}.")
    for (node <- outQueue) do {
      dbg(s"     Looking for alternative supports for ${node.datum.toString}.")
      if !node.isInNode then {
        returning {
          for (just <- node.justs) do {
            if just.checkJustification then {
              just.consequence.installSupport(just)
              throwReturn(()) // [Option[Just[D, I, R]]](Some(just))
            }
          }
        }
      }
      None
    }
  }

  /** Pass all believed contradiction nodes to the
    * [[#contradictionHandler]].
    *
    * @group internal
    *
    * **Translated from**:
    * <pre>
;;; Contradiction handling interface
(defun check-for-contradictions (jtms &aux contradictions)
  (when (jtms-checking-contradictions jtms)
    (dolist (cnode (jtms-contradictions jtms))
      (if (in-node? cnode) (push cnode contradictions)))
    (if contradictions
        (funcall (jtms-contradiction-handler jtms)
                 jtms contradictions))))
</pre>
    */
  def checkForContradictions: Unit = {
    val localContras: ListBuffer[Node[D, I, R]] = ListBuffer.empty
    if checkingContradictions then {
      for (cNode <- contradictions) {
        if cNode.isInNode then localContras += cNode
      }
      if !localContras.isEmpty then {
        contradictionHandler.map(_(this, localContras))
      }
    }
  }

  /** Propagate the retraction of an assumption by finding all other
    * nodes which used that assumption in their justification.
    *
    * @group internal
    *
    * @param node The node which has been recently disbelieved.
    * @return List of node which may now also no longer be believed.
    *
    * **Translated from**:
    * <pre>
(defun propagate-outness (node jtms &aux out-queue)
  (debugging-jtms jtms "~%   Propagating disbelief in ~A." node)
  (do ((js (tms-node-consequences node) (append (cdr js) new))
       (new nil nil)
       (conseq nil))
      ((null js) out-queue)
    ;;For each justification using the node, check to see if
    ;;it supports some other node.  If so, forget that node,
    ;;queue up the node to look for other support, and recurse
    (setq conseq (just-consequence (car js)))
    (when (eq (tms-node-support conseq) (car js))
      (make-node-out conseq)
      (push conseq out-queue)
      (setq new (tms-node-consequences conseq)))))
</pre>
    */
  def propagateOutness(node: Node[D, I, R]): List[Node[D, I, R]] = {
    dbg(s"   Propagating disbelief in $node.")
    var outQueue = new ListBuffer[Node[D, I, R]]
    val queue = Queue.empty[Just[D, I, R]]
    queue ++= node.consequences
    while (!queue.isEmpty) {
      val j = queue.dequeue
      val conseq = j.consequence
      if conseq.support.map(_ == j).getOrElse(false) then {
        conseq.makeNodeOut
        outQueue += conseq
        for (c <- conseq.consequences) do queue.enqueue(c)
      }
    }
    outQueue.toList
  }

  // (defmacro without-contradiction-check (jtms &body body)
  //   (contradiction-check jtms nil body))
  inline def withoutContradictionCheck[A](body: () => A): A =
    contradictionCheck(false, body)

  // (defmacro with-contradiction-check (jtms &body body)
  //   (contradiction-check jtms t body))
  inline def withContradictionCheck[A](body: () => A): A =
    contradictionCheck(true, body)

  /**
    *
    * @group internal
    *
    * @return
    *
    * **Translated from**:
    * <pre>
(defun contradiction-check (jtms flag body)
  (let ((jtmsv (gensym)) (old-value (gensym)))
    `(let* ((,jtmsv ,jtms)
            (,old-value (jtms-checking-contradictions ,jtmsv)))
       (unwind-protect
           (progn (setf (jtms-checking-contradictions ,jtmsv) ,flag) ,@body)
         (setf (jtms-checking-contradictions ,jtmsv) ,old-value)))))
</pre>
    */
  inline def contradictionCheck[A](flag: Boolean, body: () => A): A = {
    val oldFlag = checkingContradictions
    checkingContradictions = flag
    try {
      body()
    } finally {
      checkingContradictions = oldFlag
    }
  }


  /** Return the list of the currently enabled assumptions.
    *
    * @group internal
    *
    * @return
    *
    * **Translated from**:
    * <pre>
(defun enabled-assumptions (jtms &aux result)
  (dolist (assumption (jtms-assumptions jtms) result)
    (if (eq (tms-node-support assumption) :ENABLED-ASSUMPTION)
      (push assumption result))))
</pre>
    */
  def enabledAssumptions: List[Node[D, I, R]] = {
    val result = ListBuffer.empty[Node[D, I, R]]
    for (assumption <- assumptions)
      do if assumption.support.map(_ == EnabledAssumption).getOrElse(false)
    then result += assumption
    result.toList
  }

  /** Print a verbose list of the current nodes.
    *
    * @group diagnostic
    */
  def debugNodes: Unit = nodes.map(_.debugNode)

  /** Print the justifications of the current nodes.
    *
    * @group diagnostic
    *
    * **Translated from**:
    * <pre>
(defun why-nodes (jtms)
  (dolist (node (jtms-nodes jtms)) (why-node node)))
</pre>
    */
  def whyNodes: Unit = nodes.map(_.whyNode)

  /** Print a verbose debugging output of this JTMS as text.
    *
    * @group diagnostic
    */
  def debugJTMS: Unit = {
    println("-----")
    justs.map(_.detailJust)
    debugNodes
    println("-----")
  }

  /** Print a verbose debugging output list of the contradictions in
    * this JTMS.
    *
    * @param nodes The list of contradictions to be printed.
    *
    * @group diagnostic
    *
    * **Translated from**:
    * <pre>
(defun print-contra-list (nodes)
  (do ((counter 1 (1+ counter))
       (nn nodes (cdr nn)))
      ((null nn))
    (format t "~%~A ~A" counter
       (node-string (car nn)))))
</pre>
    */
  def printContraList(nodes: List[Node[D, I, R]]): Unit = {
    var counter: Int = 1
    for (n <- nodes) do {
      println(s"${counter} ${nodeString(n)}")
      counter = 1 + counter
    }
  }

} // class JTMS
