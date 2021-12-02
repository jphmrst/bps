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

package org.maraist.tms.jtms
import scala.collection.mutable.{ListBuffer, HashSet, HashMap}

class Node[I](
  val index: Int,
  val datum: Datum[I],
  val jtms: JTMS[I],
  val isAssumption: Boolean = false,
  val isContradoctory: Boolean = false
) {
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

  def isPremise: Boolean = ???
  // (defun tms-node-premise? (node &aux support)
  //   (and (setq support (tms-node-support node))
  //        (not (eq support :ENABLED-ASSUMPTION))
  //        (null (just-antecedents support))))

  def nodeString: String = ???
  // (defun node-string (node)
  //   (funcall (jtms-node-string (tms-node-jtms node)) node))

  def tmsError(string: String): Unit = ???
  // (defun tms-error (string node) (error string (node-string node)))

  // (defun default-node-string (n) (format nil "~A" (tms-node-datum n)))

  def isInNode: Boolean = ???
  // (defun in-node? (node) (eq (tms-node-label node) :IN))

  def isOutNode: Boolean = ???
  // (defun out-node? (node) (eq (tms-node-label node) :OUT))

  def assumeNode: Unit = ???
  // ;;; Converts a regular node to an assumption and enables it.
  // (defun assume-node (node &aux (jtms (tms-node-jtms node)))
  //   (unless (or (tms-node-assumption? node) (tms-node-premise? node))
  //     (debugging-jtms jtms "~%Converting ~A into an assumption" node)
  //     (setf (tms-node-assumption? node) t)
  //     (push node (jtms-assumptions jtms)))
  //   (enable-assumption node))

  def makeContradiction: Unit = ???
  // (defun make-contradiction (node &aux (jtms (tms-node-jtms node)))
  //   (unless (tms-node-contradictory? node)
  //     (setf (tms-node-contradictory? node) t)
  //     (push node (jtms-contradictions jtms))
  //     (check-for-contradictions jtms)))

  def installSupport(just: Just[I]): Unit = ???
  // (defun install-support (conseq just)
  //   (make-node-in conseq just)
  //   (propagate-inness conseq))

  def propagateInness: Unit = ???
  // (defun propagate-inness (node &aux (jtms (tms-node-jtms node)) (q (list node)))
  //   (do () ((null (setq node (pop q))))
  //     (debugging-jtms jtms "~%   Propagating belief in ~A." node)
  //     (dolist (justification (tms-node-consequences node))
  //       (when (check-justification justification)
  //    (make-node-in (just-consequence justification) justification)
  //    (push (just-consequence justification) q)))))

  def makeNodeIn(reason: Just[I]) = ???
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

  def retractAssumption: Unit = ???
  // ;;; Assumption Manipulation
  // (defun retract-assumption (node &aux jtms)
  //   (when (eq (tms-node-support node) :ENABLED-ASSUMPTION)
  //     (setq jtms (tms-node-jtms node))
  //     (debugging-jtms jtms "~%  Retracting assumption ~A." node)
  //     (make-node-out node)
  //     (find-alternative-support jtms
  //                               (cons node
  //                                     (propagate-outness node jtms)))))

  def enableAssumption: Unit = ???
  // (defun enable-assumption (node &aux (jtms (tms-node-jtms node)))
  //   (unless (tms-node-assumption? node)
  //     (tms-error "Can't enable the non-assumption ~A" node))
  //   (debugging-jtms jtms "~%  Enabling assumption ~A." node)
  //   (cond ((out-node? node) (make-node-in node :ENABLED-ASSUMPTION)
  //                      (propagate-inness node))
  //    ((or (eq (tms-node-support node) :ENABLED-ASSUMPTION)
  //         (null (just-antecedents (tms-node-support node)))))
  //    (t (setf (tms-node-support node) :ENABLED-ASSUMPTION)))
  //   (check-for-contradictions jtms))

  def makeNodeOut: Unit = ???
  // (defun make-node-out (node &aux jtms enqueuef)
  //   (setq jtms (tms-node-jtms node)
  //    enqueuef (jtms-enqueue-procedure jtms))
  //   (debugging-jtms jtms "~%     retracting belief in ~a." node)
  //   (setf (tms-node-support node) nil)
  //   (setf (tms-node-label node) :OUT)
  //   (if enqueuef (dolist (out-rule (tms-node-out-rules node))
  //             (funcall enqueuef out-rule)))
  //   (setf (tms-node-out-rules node) nil))

  def supportingJustificationForNode: Just[I] = ???
  // ;;; Well-founded support inqueries
  // (defun supporting-justification-for-node (node) (tms-node-support node))

  def assumptionsOfNode: ContraAssumptions[I] = ???
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

  def whyNode: Node[I] = ???
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

  def exploreNetwork: Unit = ???
  // (defun explore-network (node)
  //   (unless (in-node? node)
  //      (format t "~% Sorry, ~A not believed." (node-string node))
  //      (return-from explore-network node))
  //   (do ((stack nil)
  //        (current node)
  //        (options nil)
  //        (olen 0)
  //        (done? nil))
  //       (done? current)
  //       (why-node current)
  //       (setq options (if (typep (tms-node-support current) 'just)
  //                    (just-antecedents (tms-node-support current))))
  //       (setq olen (length options))
  //       (do ((good? nil)
  //       (choice 0))
  //      (good? (case good?
  //                   (q (return-from explore-network current))
  //                   (0 (if stack
  //                          (setq current (pop stack))
  //                          (return-from explore-network current)))
  //                   (t (push current stack)
  //                      (setq current (nth (1- good?) options)))))
  //      (format t "~%>>>")
  //      (setq choice (read))
  //      (cond ((or (eq choice 'q)
  //                 (and (integerp choice)
  //                      (not (> choice olen))
  //                      (not (< choice 0))))
  //             (setq good? choice))
  //            (t (format t
  //                "~% Must be q or an integer from 0 to ~D."
  //                olen))))))

  def viewNode: Fact = datum.fact
  // ;; From jdata.lisp
  // (defun view-node (node)
  //   (datum-lisp-form (tms-node-datum node)))

} // class Node
