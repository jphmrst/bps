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

/** Temporary type placeholder, until we work out a final form. */
type ContraAssumptions[D, I] = ListBuffer[Node[D, I]]

/** Implementation of justification-based truth maintenance systems.
  *
  * @param title Name of this TMS, for output.
  * @param nodeString Default formatter for TMS nodes.
  * @param debugging Debugging flag.
  * @param enqueueProcedure
  * @param contradictionHandler External handler for detecting contradictions.
  * @param checkingContradictions For external systems.
  * @tparam I Type of (external) informants in justifications.
  */
class JTMS[D,I](
  val title: String,
  val nodeString: (Node[D, I]) => String =
    (n: Node[D, I]) => s"${n.datum.toString()}",
  var debugging: Boolean = false,
  val checkingContradictions: Boolean = true,
  var enqueueProcedure: Option[(Rule[I]) => Unit] = None,
  var contradictionHandler:
      Option[(JTMS[D, I], ListBuffer[Node[D, I]]) => Unit] = None
) {

  /** Unique namer for nodes. */
  var nodeCounter: Int = 0
  /** Increment the node counter and return its value. */
  def incrNodeCounter: Int = {
    val result = nodeCounter
    nodeCounter = nodeCounter + 1
    result
  }

  /** Unique namer for justifications. */
  var justCounter: Int = 0
  /** Increment the justifications counter and return its value. */
  def incrJustCounter: Int = {
    val result = justCounter
    justCounter = justCounter + 1
    result
  }

  /** List of all tms nodes. */
  var nodes: ListBuffer[Node[D, I]] = ListBuffer.empty

  /** List of all justifications. */
  var justs: ListBuffer[Just[D, I]] = ListBuffer.empty

  /** List of contradiction nodes. */
  var contradictions: ListBuffer[Node[D, I]] = ListBuffer.empty

  /** List of assumption nodes. */
  var assumptions: ListBuffer[Node[D, I]] = ListBuffer.empty

  // (defstruct (jtms (:PRINT-FUNCTION print-jtms))
  //   (title nil)
  //   (node-counter 0)             ;; unique namer for nodes.
  //   (just-counter 0)             ;; unique namer for justifications.
  //   (nodes nil)                  ;; list of all tms nodes.
  //   (justs nil)                  ;; list of all justifications
  //   (debugging nil)              ;; debugging flag
  //   (contradictions nil)         ;; list of contradiction nodes.
  //   (assumptions nil)            ;; list of assumption nodes.
  //   (checking-contradictions T)  ;; For external systems
  //   (node-string nil)
  //   (contradiction-handler nil)
  //   (enqueue-procedure nil))

  // (defun create-jtms (title &key (node-string 'default-node-string)
  //                                debugging
  //                                (checking-contradictions t)
  //                                (contradiction-handler 'ask-user-handler)
  //                                enqueue-procedure)
  //   (make-jtms :TITLE title
  //         :NODE-STRING node-string
  //         :DEBUGGING debugging
  //         :CHECKING-CONTRADICTIONS checking-contradictions
  //         :CONTRADICTION-HANDLER contradiction-handler
  //         :ENQUEUE-PROCEDURE enqueue-procedure
  //         ))

  inline def dbg(msg: String): Unit = if debugging then println(msg)
  // (defmacro debugging-jtms (jtms msg &optional node &rest args)
  //   `(when (jtms-debugging ,jtms)
  //      (format *trace-output* ,msg (if ,node (node-string ,node)) ,@args)))

  /** Print the JTMS by name. */
  def printJtms(): Unit = println(s"<JTMS: $title>")
  // (defun print-jtms (jtms stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "#<JTMS: ~A>" (jtms-title jtms)))

  // (defun change-jtms (jtms &key contradiction-handler node-string
  //                          enqueue-procedure debugging
  //                               checking-contradictions)
  //   (if node-string (setf (jtms-node-string jtms) node-string))
  //   (if debugging (setf (jtms-debugging jtms) debugging))
  //   (if checking-contradictions
  //       (setf (jtms-checking-contradictions jtms)
  //        checking-contradictions))
  //   (if contradiction-handler
  //       (setf (jtms-contradiction-handler jtms) contradiction-handler))
  //   (if enqueue-procedure
  //       (setf (jtms-enqueue-procedure jtms) enqueue-procedure)))

  /**
    * Create a new node in this JTMS.
    *
    * @param datum The piece of data associated with the node.
    * @param assumptionP True indicates that this node might be used
    * as an assumption.  But note that an assumption node must be
    * enabled before a judgment can use the node as a premise.  The
    * default value is `false`.
    * @param contradictionP True indicates that this node denotes a
    * contradiction.  The default value is `false`.
    */
  def createNode(
    datum: D,
    assumptionP: Boolean = false,
    contradictionP: Boolean = false):
      Node[D, I] = {
    val node =
      new Node[D, I](datum, this, assumptionP, contradictionP)
    if assumptionP then assumptions += node
    if contradictionP then contradictions += node
    nodes += node
    node
  }
  // (defun tms-create-node (jtms datum &key assumptionp contradictoryp)
  //   (let ((node (make-tms-node :INDEX (incf (jtms-node-counter jtms))
  //                         :DATUM datum
  //                         :ASSUMPTION? assumptionp
  //                         :CONTRADICTORY? contradictoryp
  //                         :JTMS jtms)))
  //     (if assumptionp (push node (jtms-assumptions jtms)))
  //     (if contradictoryp (push node (jtms-contradictions jtms)))
  //     (push node (jtms-nodes jtms))
  //     node))

  /** Add a rule for concluding belief in a node.
    *
    * @param informant Information value associated with this
    * justification.
    * @param consequence Node concluded by ths justification.
    * @param antecedents The premises required to trigger belief in
    * the `consequence.
    */
  def justifyNode(
    informant: I, consequence: Node[D, I], antecedents: ListBuffer[Node[D, I]]):
      Unit = {
    // Create the structure to represent this inference rule.
    val just =
      new Just[D, I](incrJustCounter, informant, consequence, antecedents)

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
  // (defun justify-node (informant consequence antecedents &aux just jtms)
  //   (setq jtms (tms-node-jtms consequence)
  //    just (make-just :INDEX (incf (jtms-just-counter jtms))
  //                    :INFORMANT informant
  //                    :CONSEQUENCE consequence
  //                    :ANTECEDENTS antecedents))
  //   (push just (tms-node-justs consequence))
  //   (dolist (node antecedents) (push just (tms-node-consequences node)))
  //   (push just (jtms-justs jtms))
  //   (debugging-jtms jtms
  //              "~%Justifying ~A by ~A using ~A."
  //              consequence
  //              informant
  //              (mapcar #'node-string antecedents))
  //   (if (or antecedents (out-node? consequence))
  //       (if (check-justification just) (install-support consequence just))
  //       (setf (tms-node-support consequence) just))
  //   (check-for-contradictions jtms))

  /** Search for support for nodes which were disbelieved after an
    * assumption retraction.
    *
    * The original Lisp code returns the justification when
    * short-circuiting from the inner loop.  But this return value is
    * never used; moreover there is no return value used from callers
    * of this function.  So in this type-checked translation, we
    * return the unit value.
    *
    * @param outQueue List of nodes which have lost support.  The
    * naming of the parameter as a queue in the Lisp code is odd: the
    * list is only read; nothing is ever enqueued.
    */
  def findAlternativeSupport(outQueue: Iterable[Node[D, I]]):
      Unit = { // Option[Just[D, I]] = {
    dbg(s"   Looking for alternative supports for ${outQueue.map(_.datum.toString).mkString(", ")}.")
    for (node <- outQueue) do {
      dbg(s"     Looking for alternative supports for ${node.datum.toString}.")
      if !node.isInNode then {
        returning {
          for (just <- node.justs) do {
            if just.checkJustification then {
              just.consequence.installSupport(just)
              throwReturn(()) // [Option[Just[D, I]]](Some(just))
            }
          }
        }
      }
      None
    }
  }
  // (defun find-alternative-support (jtms out-queue)
  //   (debugging-jtms jtms "~%   Looking for alternative supports.")
  //   (dolist (node out-queue)
  //     (unless (in-node? node)
  //       (dolist (just (tms-node-justs node))
  //         (when (check-justification just)
  //           (install-support (just-consequence just) just)
  //           (return just))))))

  def checkForContradictions: Unit = {
    val localContras: ListBuffer[Node[D, I]] = ListBuffer.empty
    if checkingContradictions then {
      for (cNode <- contradictions) {
        if cNode.isInNode then localContras += cNode
      }
      if !localContras.isEmpty then {
        contradictionHandler.map(_(this, localContras))
      }
    }
  }
  // ;;; Contradiction handling interface
  // (defun check-for-contradictions (jtms &aux contradictions)
  //   (when (jtms-checking-contradictions jtms)
  //     (dolist (cnode (jtms-contradictions jtms))
  //       (if (in-node? cnode) (push cnode contradictions)))
  //     (if contradictions
  //         (funcall (jtms-contradiction-handler jtms)
  //                  jtms contradictions))))

  def propagateOutness(node: Node[D, I]): List[Node[D, I]] = {
    dbg(s"   Propagating disbelief in $node.")
    var outQueue = new ListBuffer[Node[D, I]]
    val queue = Queue.empty[Just[D, I]]
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
  // (defun propagate-outness (node jtms &aux out-queue)
  //   (debugging-jtms jtms "~%   Propagating disbelief in ~A." node)
  //   (do ((js (tms-node-consequences node) (append (cdr js) new))
  //        (new nil nil)
  //        (conseq nil))
  //       ((null js) out-queue)
  //     ;;For each justification using the node, check to see if
  //     ;;it supports some other node.  If so, forget that node,
  //     ;;queue up the node to look for other support, and recurse
  //     (setq conseq (just-consequence (car js)))
  //     (when (eq (tms-node-support conseq) (car js))
  //       (make-node-out conseq)
  //       (push conseq out-queue)
  //       (setq new (tms-node-consequences conseq)))))

  // (defmacro without-contradiction-check (jtms &body body)
  //   (contradiction-check jtms nil body))

  // (defmacro with-contradiction-check (jtms &body body)
  //   (contradiction-check jtms t body))

  def contradictionCheck(
    flag: Boolean,
    body: (JTMS[D, I], Boolean) => Unit):
      Unit = {
    ???
  }
  // (defun contradiction-check (jtms flag body)
  //   (let ((jtmsv (gensym)) (old-value (gensym)))
  //     `(let* ((,jtmsv ,jtms)
  //        (,old-value (jtms-checking-contradictions ,jtmsv)))
  //        (unwind-protect
  //       (progn (setf (jtms-checking-contradictions ,jtmsv) ,flag) ,@body)
  //     (setf (jtms-checking-contradictions ,jtmsv) ,old-value)))))

  // (defmacro with-contradiction-handler (jtms handler &body body)
  //   (let ((jtmsv (gensym)) (old-handler (gensym)))
  //     `(let* ((,jtmsv ,jtms)
  //             (,old-handler (jtms-contradiction-handler ,jtmsv)))
  //        (unwind-protect
  //          (progn
  //            (setf (jtms-contradiction-handler ,jtmsv) ,handler)
  //            ,@body)
  //        (setf (jtms-contradiction-handler ,jtmsv) ,old-handler)))))

  def defaultAssumptions: Unit = {
    ???
  }
  // (defun default-assumptions (jtms)
  //   (with-contradiction-check jtms
  //     (with-contradiction-handler jtms #'(lambda (&rest ignore)
  //                                     (declare (ignore ignore))
  //                                     (throw 'CONTRADICTION t))
  //       (dolist (assumption (jtms-assumptions jtms))
  //    (cond ((eq (tms-node-support assumption) :ENABLED-ASSUMPTION))
  //          ((not (eq :DEFAULT (tms-node-assumption? assumption))))
  //          ((catch 'CONTRADICTION (enable-assumption assumption))
  //           (retract-assumption assumption)))))))

  def enabledAssumptions: List[Node[D, I]] = {
    val result = ListBuffer.empty[Node[D, I]]
    for (assumption <- assumptions)
      do if assumption.support.map(_ == enabledAssumption).getOrElse(false)
    then result += assumption
    result.toList
  }
  // (defun enabled-assumptions (jtms &aux result)
  //   (dolist (assumption (jtms-assumptions jtms) result)
  //     (if (eq (tms-node-support assumption) :ENABLED-ASSUMPTION)
  //       (push assumption result))))

  def debugNodes: Unit = nodes.map(_.debugNode)
  def whyNodes: Unit = nodes.map(_.whyNode)
  // (defun why-nodes (jtms)
  //   (dolist (node (jtms-nodes jtms)) (why-node node)))

  def askUserHandler(contradictions: List[Node[D, I]]): Unit = {
    handleOneContradiction(contradictions.head)
    checkForContradictions
  }
  // (defun ask-user-handler (jtms contradictions)
  //   (handle-one-contradiction (car contradictions))
  //   (check-for-contradictions jtms))

  def debugJTMS: Unit = {
    println("-----")
    justs.map(_.detailJust)
    debugNodes
    println("-----")
  }

  var contraAssumptions: ContraAssumptions[D, I] = ListBuffer.empty
  // (proclaim '(special *contra-assumptions*))

  def handleOneContradiction(contraNode: Node[D, I]): Unit = {
    contraAssumptions = contraNode.assumptionsOfNode
    if contraAssumptions.isEmpty then
      contraNode.tmsError(s"There is a flaw in the universe...$contraNode")
    println(s"Contradiction found: ${nodeString(contraNode)}")
    printContraList(contraAssumptions.toList)
    println(s"Call (TMS-ANSWER <number>) to retract assumption.")
    ???
  }
  // (defun handle-one-contradiction (contra-node
  //                                   &aux the-answer *contra-assumptions*)
  //   (setq *contra-assumptions* (assumptions-of-node contra-node))
  //   (unless *contra-assumptions*
  //     (tms-error "~%There is a flaw in the universe...~A" contra-node))
  //   (format t "~%Contradiction found: ~A" (node-string contra-node))
  //   (print-contra-list *contra-assumptions*)
  //   (format t "~%Call (TMS-ANSWER <number>) to retract assumption.")
  //   (setq the-answer
  //    (catch 'tms-contradiction-handler
  //      (break "JTMS contradiction break")))
  //   (if (and (integerp the-answer)
  //       (> the-answer 0)
  //       (not (> the-answer (length *contra-assumptions*))))
  //       (retract-assumption (nth (1- the-answer)
  //                           *contra-assumptions*))))

  def printContraList(nodes: List[Node[D, I]]): Unit = {
    var counter: Int = 1
    for (n <- nodes) do {
      println(s"${counter} ${nodeString(n)}")
      counter = 1 + counter
    }
  }
  // (defun print-contra-list (nodes)
  //   (do ((counter 1 (1+ counter))
  //        (nn nodes (cdr nn)))
  //       ((null nn))
  //     (format t "~%~A ~A" counter
  //        (node-string (car nn)))))

  def tmsAnswer(num: Int): Unit = if num > 0 then {
    if num <= contraAssumptions.length
    then throw new TmsContradictionHandler(num)
    else println("Ignoring answer, too big.")
  }
  else println("Ignoring answer, too big.")
  // (defun tms-answer (num)
  //   (if (integerp num)
  //       (if (> num 0)
  //           (if (not (> num (length *contra-assumptions*)))
  //             (throw 'tms-contradiction-handler num)
  //             (format t "~%Ignoring answer, too big."))
  //           (format t "~%Ignoring answer, too small"))
  //       (format t "~%Ignoring answer, must be an integer.")))

} // class JTMS

class TmsContradictionHandler(num: Int) extends RuntimeException
