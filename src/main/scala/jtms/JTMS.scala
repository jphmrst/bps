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

inline def dbg[D, I](jtms: JTMS[D, I], msg: String) =
  if jtms.debugging then println(msg)
// (defmacro debugging-jtms (jtms msg &optional node &rest args)
//   `(when (jtms-debugging ,jtms)
//      (format *trace-output* ,msg (if ,node (node-string ,node)) ,@args)))

class JTMS[D,I](
  val title: String,
  val nodeString: (Node[D, I]) => String =
    (n: Node[D, I]) => s"${n.datum.toString()}",
  val debugging: Boolean = false
  //   ,
  // val enqueueProcedure: Option[(Rule) => Unit] = None
) {
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

  // (defun print-jtms (jtms stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "#<JTMS: ~A>" (jtms-title jtms)))

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

  // (defun find-alternative-support (jtms out-queue)
  //   (debugging-jtms jtms "~%   Looking for alternative supports.")
  //   (dolist (node out-queue)
  //     (unless (in-node? node)
  //       (dolist (just (tms-node-justs node))
  //    (when (check-justification just)
  //      (install-support (just-consequence just)
  //                             just)
  //      (return just))))))

  // ;;; Contradiction handling interface
  // (defun check-for-contradictions (jtms &aux contradictions)
  //   (when (jtms-checking-contradictions jtms)
  //     (dolist (cnode (jtms-contradictions jtms))
  //       (if (in-node? cnode) (push cnode contradictions)))
  //     (if contradictions
  //    (funcall (jtms-contradiction-handler jtms) jtms contradictions))))

  // (defmacro without-contradiction-check (jtms &body body)
  //   (contradiction-check jtms nil body))

  // (defmacro with-contradiction-check (jtms &body body)
  //   (contradiction-check jtms t body))

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
  //        (,old-handler (jtms-contradiction-handler ,jtmsv)))
  //      (unwind-protect
  //     (progn (setf (jtms-contradiction-handler ,jtmsv) ,handler) ,@body)
  //        (setf (jtms-contradiction-handler ,jtmsv) ,old-handler)))))

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

  // (defun enabled-assumptions (jtms &aux result)
  //   (dolist (assumption (jtms-assumptions jtms) result)
  //     (if (eq (tms-node-support assumption) :ENABLED-ASSUMPTION)
  //    (push assumption result))))

  // (defun why-nodes (jtms)
  //   (dolist (node (jtms-nodes jtms)) (why-node node)))

  // (defun ask-user-handler (jtms contradictions)
  //   (handle-one-contradiction (car contradictions))
  //   (check-for-contradictions jtms))

  // (proclaim '(special *contra-assumptions*))

  // (defun handle-one-contradiction (contra-node
  //                             &aux the-answer *contra-assumptions*)
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

  // (defun print-contra-list (nodes)
  //   (do ((counter 1 (1+ counter))
  //        (nn nodes (cdr nn)))
  //       ((null nn))
  //     (format t "~%~A ~A" counter
  //        (node-string (car nn)))))

  // (defun tms-answer (num)
  //   (if (integerp num)
  //       (if (> num 0)
  //      (if (not (> num (length *contra-assumptions*)))
  //          (throw 'tms-contradiction-handler num)
  //          (format t "~%Ignoring answer, too big."))
  //      (format t "~%Ignoring answer, too small"))
  //       (format t "~%Ignoring answer, must be an integer.")))

} // class JTMS
