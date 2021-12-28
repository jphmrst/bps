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

/** Wrapper for one possible belief in the TMS.
  *
  * @param datum Datum associated with this node.
  *
  * @param jtms [[JTMS]] with this node is associated.
  *
  * @param isAssumption The explicit designation that a belief is an
  * assumption.  Note that setting this flag only does *not* mean that
  * the TMS will choose to believe it: an assumption must be
  * explicitly activated using [[#enableAssumption]], and can be
  * subsequently disbelieved with [[#retractAssumption]].
  *
  * @param isContradictory The explicit designation that a belief is a
  * contradiction.  Contradictions are never believed by the JTMS.
  * The JTMS will inform the external system (via
  * [[JTMS#enqueueProcedure]]) when a contradictory node becomes
  * believed for the external system to resolve (such as with
  * [[#assumptionsOfNode]]).
  *
  * @tparam D Type of data associated with each node of a [[JTMS]].
  *
  * @tparam I Type of informants in the external system.
  *
  * @tparam R Type of rules which may be associated with each node of
  * a [[JTMS]].
  *
  * @constructor The constructor is internal to the implementation,
  * and should only be called from the [[JTMS#createNode]] method (or
  * some overriding of that method).  However there is no sensible
  * package restriction which will still allow extensions of the
  * overall TMS/node system.
  */
class Node[D, I, R] (
  val datum: D,
  val jtms: JTMS[D, I, R],
  var isAssumption: Boolean = false,
  var isContradictory: Boolean = false
) {

  /**
    * Unique nueric identifier for this node, unique among nodes of
    * the same [[JTMS]].
    */
  val index: Int = jtms.incrNodeCounter

  /**
    * If this node is believed by the [[JTMS]], this member refers to
    * the evidence for this belief.
    */
  var support: Option[Justification[D, I, R]] = None

  /** Whether the current node is `:IN`.  A value of `true` corresponds
    * to a `label` of `:IN` in the old Lisp `struct`ure; `false`, to
    * `:OUT`.
    */
  var believed: Boolean = false

  /**
    * List of justification relations which may be enabled by belief
    * in this node.
    */
  val consequences: ListBuffer[Just[D, I, R]] = ListBuffer.empty

  /**
    * Rules that should be triggered when node goes in.
    */
  val inRules: ListBuffer[R] = ListBuffer.empty

  /**
    * Rules that should be triggered when node goes out.
    */
  val outRules: ListBuffer[R] = ListBuffer.empty

  /**
    * Possible justifications.
    */
  val justs: ListBuffer[Just[D, I, R]] = ListBuffer.empty

  // (defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
  //   (index 0)
  //   (datum nil)           ;; pointer to external problem solver
  //   (label :OUT)          ;; :IN means believed, :OUT means disbelieved
  //   (support nil)         ;; Current justification or premise marker
  //   (justs nil)           ;; Possible justifications
  //   (consequences nil)    ;; Justifications in which it is an antecedent
  //   (mark nil)            ;; Marker for sweep algorithms
  //   (contradictory? nil)  ;; Flag marking it as contradictory
  //   (assumption? nil)     ;; Flag marking it as an assumption.
  //   (in-rules nil)   ;; Rules that should be triggered when node goes in
  //   (out-rules nil)  ;; Rules that should be triggered when node goes out
  //   (jtms nil))           ;; The JTMS in which this node appears.

  /**
    * Return the short tag for this node.
    */
  override def toString(): String = s"<Node: $nodeString>"

  /**
    * Output a brief tag for this node.
    */
  def printTmsNode: Unit = print(toString)
  // (defun print-tms-node (node stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "#<Node: ~A>" (node-string node)))

  /**
    * Returns `true` if either this node is a believed assumption, or
    * if it is concluded by an axiomatic justification with no
    * antecedents.
    */
  def isPremise: Boolean = support match {
    case None => false
    case Some(sup) => sup match {
      case just: Just[D, I, R] => just.antecedents.isEmpty
      case _: EnabledAssumption => true
      case _: UserStipulation => true
    }
  }
  // (defun tms-node-premise? (node &aux support)
  //   (and (setq support (tms-node-support node))
  //        (not (eq support :ENABLED-ASSUMPTION))
  //        (null (just-antecedents support))))

  /**
    * Format this node in the manner standard for its [[JTMS]].
    *
    * @return
    */
  def nodeString: String = jtms.nodeString(this)
  // (defun node-string (node)
  //   (funcall (jtms-node-string (tms-node-jtms node)) node))

  /**
    * Throw an error related to this node of the TMS.
    *
    * @throws TmsError This method is simply a wrapper for throwing an
    * exception of this class.
    */
  def tmsError(string: String): Unit = throw new TmsError(this, string)
  // (defun tms-error (string node) (error string (node-string node)))

  /**
    * API method checking whether this node is believed.
    */
  def isInNode: Boolean = believed
  // (defun in-node? (node) (eq (tms-node-label node) :IN))

  /**
    * API method checking whether this node is disbelieved.
    */
  def isOutNode: Boolean = !believed
  // (defun out-node? (node) (eq (tms-node-label node) :OUT))

  /**
    * Internal method used to flag this node as an assumption, and to
    * enable belief in this assumption.
    */
  def assumeNode: Unit = {
    if !isAssumption && !isPremise then {
      jtms.dbg(s"Converting $this into an assumption")
      isAssumption = true
      jtms.assumptions += this
    }
    enableAssumption
  }
  // ;;; Converts a regular node to an assumption and enables it.
  // (defun assume-node (node &aux (jtms (tms-node-jtms node)))
  //   (unless (or (tms-node-assumption? node) (tms-node-premise? node))
  //     (debugging-jtms jtms "~%Converting ~A into an assumption" node)
  //     (setf (tms-node-assumption? node) t)
  //     (push node (jtms-assumptions jtms)))
  //   (enable-assumption node))

  /**
    * API method used when the external system categorizes this node
    * as representing a contradiction.
    */
  def makeContradiction: Unit = if !isContradictory then {
    isContradictory = true
    jtms.contradictions += this
    jtms.checkForContradictions
  }
  // (defun make-contradiction (node &aux (jtms (tms-node-jtms node)))
  //   (unless (tms-node-contradictory? node)
  //     (setf (tms-node-contradictory? node) t)
  //     (push node (jtms-contradictions jtms))
  //     (check-for-contradictions jtms)))

  /**
    * Add a reason for this node to be believed.
    */
  def installSupport(just: Just[D, I, R]): Unit = {
    makeNodeIn(just)
    propagateInness
  }
  // (defun install-support (conseq just)
  //   (make-node-in conseq just)
  //   (propagate-inness conseq))

  /**
    * Trigger justifications which rely (directly or indirectly) on
    * this node as an antecedent when this node becomes believed.
    */
  def propagateInness: Unit = {
    val q = Queue[Node[D, I, R]](this)
    while (!q.isEmpty) {
      val node = q.dequeue
      jtms.dbg(s"Propagating belief in $node.")
      for (justification <- node.consequences)
        do if justification.checkJustification then {
          val conseq = justification.consequence
          conseq.makeNodeIn(justification)
          q.enqueue(conseq)
        }
    }
  }
  // (defun propagate-inness (node &aux (jtms (tms-node-jtms node))
  //                                    (q (list node)))
  //   (do () ((null (setq node (pop q))))
  //     (debugging-jtms jtms "~%   Propagating belief in ~A." node)
  //     (dolist (justification (tms-node-consequences node))
  //       (when (check-justification justification)
  //         (make-node-in (just-consequence justification) justification)
  //         (push (just-consequence justification) q)))))

  /**
    * Called when the given `reason` causes the JTMS to believe this
    * node.
    */
  def makeNodeIn(reason: Justification[D, I, R]) = {
    jtms.dbg(reason match {
      case _: EnabledAssumption =>
        s"     Making $this in as enabled assumption."
      case _: UserStipulation =>
        s"     Making $this in as a user stipulation."
      case j: Just[D, I, R] => {
        val mapped = j.antecedents.map(jtms.nodeString)
        s"     Making $this in via ${j.informant} :: $mapped."
      }
    })

    believed = true
    support = Some(reason)
    jtms.enqueueProcedure match {
      case None => { }
      case Some(fn) => {
        for inRule <- inRules do fn(inRule)
        inRules.clear
      }
    }
  }
  // (defun make-node-in (conseq reason &aux jtms enqueuef)
  //   (setq jtms (tms-node-jtms conseq)
  //    enqueuef (jtms-enqueue-procedure jtms))
  //   (debugging-jtms jtms "~%     Making ~A in via ~A."
  //         conseq
  //         (if (symbolp reason)
  //             reason
  //             (cons (just-informant reason)
  //                   (mapcar (jtms-node-string jtms)
  //                           (just-antecedents reason)))))
  //   (setf (tms-node-label conseq) :IN)
  //   (setf (tms-node-support conseq) reason)
  //   (when enqueuef
  //     (dolist (in-rule (tms-node-in-rules conseq))
  //       (funcall enqueuef in-rule))
  //     (setf (tms-node-in-rules conseq) nil)))

  /**
    * Called when the external system chooses to disbelieve this
    * assumption represented by this node.
    */
  def retractAssumption: Unit = {
    if support.map(_ == EnabledAssumption).getOrElse(false)
    then {
      jtms.dbg(s"  Retracting assumption $this")
      makeNodeOut
      jtms.findAlternativeSupport(this :: jtms.propagateOutness(this))
    }
  }
  // ;;; Assumption Manipulation
  // (defun retract-assumption (node &aux jtms)
  //   (when (eq (tms-node-support node) :ENABLED-ASSUMPTION)
  //     (setq jtms (tms-node-jtms node))
  //     (debugging-jtms jtms "~%  Retracting assumption ~A." node)
  //     (make-node-out node)
  //     (find-alternative-support jtms
  //                               (cons node
  //                                     (propagate-outness node jtms)))))

  /**
    * Called when the external system chooses to believe this
    * assumption represented by this node.
    */
  def enableAssumption: Unit = {
    if !isAssumption then tmsError(s"Can't enable the non-assumption $this")
    jtms.dbg(s"  Enabling assumption $this.")
    if isOutNode then {
      makeNodeIn(EnabledAssumption)
      propagateInness
    } else {
      if support.map(_ != EnabledAssumption).getOrElse(true)
          && !support.map(_ match {
            case j: Just[D, I, R] => j.antecedents.isEmpty
            case _: EnabledAssumption => true // TODO Really?
            case _: UserStipulation => true // TODO Really?
          }).getOrElse(false) then {
        support = Some(EnabledAssumption)
      }
    }
    jtms.checkForContradictions
  }
  // (defun enable-assumption (node &aux (jtms (tms-node-jtms node)))
  //   (unless (tms-node-assumption? node)
  //     (tms-error "Can't enable the non-assumption ~A" node))
  //   (debugging-jtms jtms "~%  Enabling assumption ~A." node)
  //   (cond ((out-node? node)
  //          (make-node-in node :ENABLED-ASSUMPTION)
  //          (propagate-inness node))
  //    ((or (eq (tms-node-support node) :ENABLED-ASSUMPTION)
  //         (null (just-antecedents (tms-node-support node)))))
  //    (t (setf (tms-node-support node) :ENABLED-ASSUMPTION)))
  //   (check-for-contradictions jtms))

  /**
    * Called when the JTMS disblieves this node.
    */
  def makeNodeOut: Unit = {
    jtms.dbg(s"     retracting belief in $this.")
    support = None
    believed = false
    jtms.enqueueProcedure match {
      case None => { }
      case Some(fn) => for outRule <- outRules do fn(outRule)
    }
    outRules.clear
  }
  // (defun make-node-out (node &aux jtms enqueuef)
  //   (setq jtms (tms-node-jtms node)
  //    enqueuef (jtms-enqueue-procedure jtms))
  //   (debugging-jtms jtms "~%     retracting belief in ~a." node)
  //   (setf (tms-node-support node) nil)
  //   (setf (tms-node-label node) :OUT)
  //   (if enqueuef (dolist (out-rule (tms-node-out-rules node))
  //             (funcall enqueuef out-rule)))
  //   (setf (tms-node-out-rules node) nil))

  /**
    * API method returning the reason the TMS believes this node.
    *
    * @return `None` if the TMS does not believe this node.
    */
  inline def supportingJustificationForNode: Option[Justification[D, I, R]] =
    support
  // ;;; Well-founded support inqueries
  // (defun supporting-justification-for-node (node) (tms-node-support node))

  /**
    * API method returning the believed assumption nodes used to
    * justify belief in this node.
    *
    * @return
    */
  def assumptionsOfNode: ListBuffer[Node[D, I, R]] = {
    val marking = Array.fill[Boolean](jtms.nodeCounter)(false)
    val queue = Queue[Node[D, I, R]](this) // Replaces `new`
    val assumptions = ListBuffer.empty[Node[D, I, R]]
    while (!queue.isEmpty) {
      val node = queue.dequeue()
      if marking(node.index) then {
        // Intentionally empty block
      } else if node.support.map(_ == EnabledAssumption).getOrElse(false) then {
        assumptions += node
      } else if node.isInNode then {
        node.support.map(_ match {
          case j: Just[D, I, R] => { queue ++= j.antecedents }
          case _: EnabledAssumption  => { }
          case _: UserStipulation  => { }
        })
      }
      marking(node.index) = true
    }
    assumptions
  }
  // (defun assumptions-of-node (node &aux assumptions (marker (list :MARK)))
  //   (do ((nodes (list node) (append (cdr nodes) new))
  //        (new nil nil))
  //       ((null nodes) assumptions)
  //     (let ((node (car nodes)))
  //       (cond ((eq (tms-node-mark node) marker))
  //        ((eq (tms-node-support node) :ENABLED-ASSUMPTION)
  //         (push node assumptions))
  //        ((in-node? node)
  //         (setq new (just-antecedents (tms-node-support node)))))
  //       (setf (tms-node-mark node) marker))))

  /**
    * Print the belief state and any justification of this node.
    *
    * @return This node.
    */
  def whyNode: Node[D, I, R] = {
    support match {
      case Some(_: EnabledAssumption)  =>
        println(s"${nodeString} is an enabled assumption")
      case Some(_: UserStipulation)  =>
        println(s"${nodeString} is a user stipulation")
      case Some(j: Just[D, I, R]) => {
        println(s"${nodeString} is IN via ${j.informant} on")
        j.antecedents.map((a) => println(s"  ${a.nodeString}"))
      }
      case None => println(s"${nodeString} is OUT")
    }
    this
  }
  // ;;; Inference engine stub to allow this JTMS to be used stand alone
  // (defun why-node (node &aux justification)
  //   (setq justification (tms-node-support node))
  //   (cond ((eq justification :ENABLED-ASSUMPTION)
  //     (format t "~%~A is an enabled assumption"
  //             (node-string node)))
  //    (justification
  //     (format t "~%~A is IN via ~A on"
  //             (node-string node)
  //             (just-informant justification))
  //     (dolist (anode (just-antecedents justification))
  //       (format t "~%  ~A" (node-string anode))))
  //    (T (format t "~%~A is OUT." (node-string node))))
  //   node)

  /**
    * Print verbose debugging output for this node.
    */
  def debugNode: Unit = {
    println(s"Node $datum (isAssumption $isAssumption, isContradictory $isContradictory, ${if believed then "" else "not "}believed)")

    support match {
      case Some(_: EnabledAssumption) =>
        println("- Supported: enabled assumption")
      case Some(_: UserStipulation) =>
        println("- Supported: user stipulation")
      case Some(j: Just[D, I, R]) =>
        println(s"- IN via ${j.informant} (${j.index})")
      case None => println(s"- OUT")
    }

    if (justs.isEmpty) {
      println("- Concluded by no justification rules")
    } else {
      println(s"- Concluded by rule${if justs.length == 1 then "" else "s"} ${justs.map(_.toString).mkString(", ")}")
    }

    if (consequences.isEmpty) {
      println("- Antecedent to no rules")
    } else {
      println(s"- Antecedent to ${consequences.map(_.toString).mkString(", ")}")
    }
  }

} // class Node

/**
  * Class of runtime errors thrown from the JTMS.
  *
  * @param node Relevant node.
  * @param string Error message.
  */
class TmsError[D, I, R](val node: Node[D, I, R], string: String)
extends RuntimeException(string)
