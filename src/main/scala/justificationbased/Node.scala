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

val enabledAssumption = Symbol("Enabled-assumption")

type Support[D, I] = Justification[D, I] // | Node[D, I]

class Node[D, I](
  val datum: D,
  val jtms: JTMS[D, I],
  var isAssumption: Boolean = false,
  var isContradictory: Boolean = false
) {

  val index: Int = jtms.incrNodeCounter

  var support: Option[Support[D, I]] = None

  /** Whether the current node is `:IN`.  A value of `true` corresponds
    * to a `label` of `:IN` in the old Lisp `struct`ure; `false`, to
    * `:OUT`.
    */
  var believed: Boolean = false

  val consequences: ListBuffer[Just[D, I]] = ListBuffer.empty

  /** Rules that should be triggered when node goes in. */
  val inRules: ListBuffer[Rule[D, I]] = ListBuffer.empty

  /** Rules that should be triggered when node goes out. */
  val outRules: ListBuffer[Rule[D, I]] = ListBuffer.empty

//  /** Marker for sweep algorithms. */
//  var mark: Option[Symbol] = None

  /** Possible justifications. */
  val justs: ListBuffer[Just[D, I]] = ListBuffer.empty

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

  override def toString(): String = s"<Node: $nodeString>"
  def printTmsNode: Unit = print(toString)
  // (defun print-tms-node (node stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "#<Node: ~A>" (node-string node)))

  def isPremise: Boolean = support match {
    case None => false
    case Some(sup) => sup match {
      case s: Symbol => sup != enabledAssumption
      case just: Just[D, I] => just.antecedents.isEmpty
      // case n: Node[D, I] => false // TODO Come back to this --- what if it's a Node here?
    }
  }
  // (defun tms-node-premise? (node &aux support)
  //   (and (setq support (tms-node-support node))
  //        (not (eq support :ENABLED-ASSUMPTION))
  //        (null (just-antecedents support))))

  def nodeString: String = jtms.nodeString(this)
  // (defun node-string (node)
  //   (funcall (jtms-node-string (tms-node-jtms node)) node))

  def tmsError(string: String): Unit = throw new TmsError(this, string)
  // (defun tms-error (string node) (error string (node-string node)))

  def defaultNodeString(n: Node[D, I]): String = n.datum.toString
  // (defun default-node-string (n) (format nil "~A" (tms-node-datum n)))

  def isInNode: Boolean = believed
  // (defun in-node? (node) (eq (tms-node-label node) :IN))

  def isOutNode: Boolean = !believed
  // (defun out-node? (node) (eq (tms-node-label node) :OUT))

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

  def installSupport(just: Just[D, I]): Unit = {
    makeNodeIn(just)
    propagateInness
  }
  // (defun install-support (conseq just)
  //   (make-node-in conseq just)
  //   (propagate-inness conseq))

  def propagateInness: Unit = {
    val q = Queue[Node[D, I]](this)
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

  def makeNodeIn(reason: Justification[D, I]) = {
    jtms.dbg(reason match {
      case s: Symbol  => s"     Making $this in via symbolic $s."
      case j: Just[D, I] => {
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

  def retractAssumption: Unit = {
    if support.map(_ == enabledAssumption).getOrElse(false)
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

  def enableAssumption: Unit = {
    if !isAssumption then tmsError(s"Can't enable the non-assumption $this")
    jtms.dbg(s"  Enabling assumption $this.")
    if isOutNode then {
      makeNodeIn(enabledAssumption)
      propagateInness
    } else {
      if support.map(_ != enabledAssumption).getOrElse(true)
          && !support.map(_ match {
            case j: Just[D, I] => j.antecedents.isEmpty
            case _: Symbol => true // TODO Really?
          }).getOrElse(false) then {
        support = Some(enabledAssumption)
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

  inline def supportingJustificationForNode: Option[Justification[D, I]] =
    support
  // ;;; Well-founded support inqueries
  // (defun supporting-justification-for-node (node) (tms-node-support node))

  def assumptionsOfNode: ListBuffer[Node[D, I]] = {
    val marking = Array.fill[Boolean](jtms.nodeCounter)(false)
    val queue = Queue[Node[D, I]](this) // Replaces `new`
    val assumptions = ListBuffer.empty[Node[D, I]]
    while (!queue.isEmpty) {
      val node = queue.dequeue()
      if marking(node.index) then {
        // Intentionally empty block
      } else if node.support.map(_ == enabledAssumption).getOrElse(false) then {
        assumptions += node
      } else if node.isInNode then {
        node.support.map(_ match {
          case _: Symbol  => { }
          case j: Just[D, I] => { queue ++= j.antecedents }
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

  def whyNode: Node[D, I] = {
    support match {
      case Some(s: Symbol)  =>
        if s == enabledAssumption then
          println(s"${nodeString} is an enabled assumption")
        else
          println(s"${nodeString} is OUT")
      case Some(j: Just[D, I]) => {
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

  def debugNode: Unit = {
    println(s"Node $datum (isAssumption $isAssumption, isContradictory $isContradictory, ${if believed then "" else "not "}believed)")

    support match {
      case Some(s: Symbol) =>
        if s == enabledAssumption then
          println("- Supportted: enabled assumption")
        else
          println("- No support")
      case Some(j: Just[D, I]) =>
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

  // Method exploreNetwork is interactive; omitting for now [JM Dec 2 '21]
  // -----------------------------------------------------------------
  // def exploreNetwork: Node[D, I] = if isInNode then {
  //
  //   // Outer do macro
  //   val stack: ListBuffer[Node[D, I]] = ListBuffer.empty
  //   var current: Node[D, I] = this
  //   var notYetDone: Boolean = true
  //   while (notYetDone) {
  //     current.whyNode
  //     val options: ListBuffer[Node[D, I]] = current.support match {
  //       case Some(j: Just[D, I]) => j.antecedents
  //       case _ => ListBuffer.empty
  //     }
  //     val oLen: Int = options.length
  //
  //     // Inner do macro
  //     var notGoodYet: Boolean = false
  //     var choice: Int = 0
  //
  //     ???
  //   }
  //   current
  // } else {
  //   println(s" Sorry, ${nodeString} not believed.")
  //   this
  // }
  // -----------------------------------------------------------------
  // (defun explore-network (node)
  //   (unless (in-node? node)
  //      (format t "~% Sorry, ~A not believed." (node-string node))
  //      (return-from explore-network node))
  //   (do ((stack nil)
  //        (current node)
  //        (options nil)
  //        (olen 0)
  //        (done? nil))
  //      (done? current)
  //      (why-node current)
  //      (setq options (if (typep (tms-node-support current) 'just)
  //                        (just-antecedents (tms-node-support current))))
  //      (setq olen (length options))
  //      (do ((good? nil) (choice 0))
  //         (good? (case good?
  //                   (q (return-from explore-network current))
  //                   (0 (if stack
  //                          (setq current (pop stack))
  //                          (return-from explore-network current)))
  //                   (t (push current stack)
  //                      (setq current (nth (1- good?) options)))))
  //        (format t "~%>>>")
  //        (setq choice (read))
  //        (cond ((or (eq choice 'q)
  //                   (and (integerp choice)
  //                        (not (> choice olen))
  //                        (not (< choice 0))))
  //               (setq good? choice))
  //              (t (format t "~% Must be q or an integer from 0 to ~D."
  //                   olen))))))

  // Removed.  Used only once, in JTRE; now expanded there.
  //
  // ;; From jdata.lisp
  // (defun view-node (node)
  //   (datum-lisp-form (tms-node-datum node)))

} // class Node

class TmsError[D, I](val node: Node[D, I], string: String)
extends RuntimeException(string)
