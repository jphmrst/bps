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

/**
  *
  * **Arguments and `val` members translated from**:
  * <pre>
(defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
  (index 0)                    ; unique namer for nodes
  (datum nil)                  ; positive inference engine datum.
  (label :UNKNOWN)             ; :UNKNOWN, :TRUE, or :FALSE.
  (support nil)                ; clause which supports it,
  (true-clauses nil)           ; clauses in which this node is true
  (false-clauses nil)          ; clauses in which this node is false
  (mark nil)                   ; marker for sweep algorithms
  (assumption? nil)
  (true-rules nil)             ; rules run when the node is true
  (false-rules nil)            ; rules run when the node is false
  (ltms nil)                   ; LTMS it is part of.
  (true-literal nil)           ; True literal.
  (false-literal nil))         ; False literal.

;; The last two fields have their names changed because there is
;; an obscure bug in ACLPC that causes it to barf if a field is
;; named TRUE or FALSE, even if there is a conc-name to be added.
;; The new names are more descriptive anyway.
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
class Node[D, I, R](
  val datum: D,
  val ltms: LTMS[D, I, R],
  var isAssumption: Boolean = false
) {

  /**
    * Unique nueric identifier for this node, unique among nodes of
    * the same [[LTMS]].
    */
  val index: Int = ltms.incrNodeCounter

  var label: Label = UnknownLabel

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun default-node-string (n)
  (format nil "~A" (tms-node-datum n)))
</pre>
    *
    */
  def defaultNodeString: String = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun print-tms-node (node stream ignore)
   (declare (ignore ignore))
   (format stream "#<NODE: ~A>" (node-string node)))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun node-string (node)
  (funcall (ltms-node-string (tms-node-ltms node)) node))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defmacro debugging-ltms (ltms msg &optional node &rest args)
  `(when (ltms-debugging ,ltms)
     (format *trace-output*
             ,msg (if ,node (node-string ,node)) ,@args)))
</pre>
    *
    */

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

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun unknown-node? (node) (eq (tms-node-label node) :UNKNOWN))
</pre>
    *
    */
  def isUnknown: Boolean = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun known-node? (node) (not (eq (tms-node-label node) :UNKNOWN)))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun true-node? (node) (eq (tms-node-label node) :TRUE))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun false-node? (node) (eq (tms-node-label node) :FALSE))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun convert-to-assumption (node)
  (unless (tms-node-assumption? node)
    (debugging-ltms (tms-node-ltms node)
                    "~%Converting ~A into an assumption" node)
    (setf (tms-node-assumption? node) T)))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun insert-true-clause (cl node)
  (push cl (tms-node-true-clauses node)))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun insert-false-clause (cl node)
  (push cl (tms-node-false-clauses node)))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun top-set-truth (node value reason &aux *clauses-to-check*)
  (set-truth node value reason)
  (check-clauses (tms-node-ltms node) *clauses-to-check*)
  (check-for-contradictions (tms-node-ltms node)))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun set-truth (node value reason &aux ltms enqueuef)
  (setq ltms (tms-node-ltms node)
        enqueuef (ltms-enqueue-procedure ltms))
  (debugging-ltms
    ltms "~%  Setting ~A to ~A, via ~A." node value reason)
  (setf (tms-node-support node) reason)
  (setf (tms-node-label node) value)
  (ecase value ;figure out which set of rules to queue up
    (:TRUE (when enqueuef
             (dolist (rule (tms-node-true-rules node))
               (funcall enqueuef rule))
             (setf (tms-node-true-rules node) nil))
           (dolist (clause (tms-node-true-clauses node))
             (incf (clause-sats clause)))
           (dolist (clause (tms-node-false-clauses node))
             (if (< (decf (clause-pvs clause)) 2)
                 (push clause *clauses-to-check*))))
    (:FALSE (when enqueuef
              (dolist (rule (tms-node-false-rules node))
                (funcall enqueuef rule)))
            (setf (tms-node-false-rules node) nil)
           (dolist (clause (tms-node-false-clauses node))
             (incf (clause-sats clause)))
            (dolist (clause (tms-node-true-clauses node))
              (if (< (decf (clause-pvs clause)) 2)
                  (push clause *clauses-to-check*))))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun support-for-node (node &aux result support)
  (cond ((null (setq support (tms-node-support node))) nil)
        ((eq support :ENABLED-ASSUMPTION) :ENABLED-ASSUMPTION)
        (t (dolist (pair (clause-literals support))
             (unless (eq (car pair) node)
               (push (car pair) result)))
           (values result (clause-informant support)))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun assumptions-of-node (node)
  (cond ((eq :ENABLED-ASSUMPTION (tms-node-support node)) (list node))
        ((known-node? node)
         (assumptions-of-clause (tms-node-support node)))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun signed-node-string (node)
  (if (true-node? node) (node-string node)
      (format nil "~:[Unknown~;Not~][~A]"
              (false-node? node) (node-string node))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun node-consequences (node &aux conseq conseqs)
  (dolist (cl (ecase (tms-node-label node)
                (:TRUE (tms-node-false-clauses node))
                (:FALSE (tms-node-true-clauses node))))
    (unless (eq cl (tms-node-support node))
      (setq conseq (clause-consequent cl))
      (if conseq (push conseq conseqs))))
  conseqs)
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun why-node (node)
  (cond ((unknown-node? node)
         (format t "~%~A is unknown." (node-string node))
         nil)
        ((eq :ENABLED-ASSUMPTION (tms-node-support node))
         (format t "~%~A is ~A <~A>"
                 (node-string node)
                 (tms-node-label node) (tms-node-support node))
         nil)
        (t (format t "~%~A is ~A via ~A on"
                   (node-string node)
                   (tms-node-label node)
                   (or (clause-informant (tms-node-support node))
                       (tms-node-support node)))
           (dolist (term-pair (clause-literals (tms-node-support node)))
             (unless (equal (tms-node-label (car term-pair))
                            (cdr term-pair))
               (format t "~%   ~A is ~A"
                       (node-string (car term-pair))
                       (tms-node-label (car term-pair)))))))
  node)
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
(defvar *line-count*)

;; From ltms.lisp
(defun explain-node (node &aux *line-count*)
  (unless (eq (tms-node-label node) :UNKNOWN)
    (setq *line-count* 0)
    (maphash #'(lambda (ignore node) (setf (tms-node-mark node) nil))
             (ltms-nodes (tms-node-ltms node)))
    (explain-1 node)))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun explain-1 (node &aux antecedents)
  (cond ((tms-node-mark node))
        ((eq :ENABLED-ASSUMPTION (tms-node-support node))
         (format T "~%~3D ~15<~:[(:NOT ~A)~;~A~]~>~15<()~>   Assumption"
                 (incf *line-count*) (true-node? node) (node-string node))
         (setf (tms-node-mark node) *line-count*))
        (t (setq antecedents
                 (mapcar #'explain-1 (clause-antecedents (tms-node-support node))))
           (format T "~%~3D ~15<~:[(:NOT ~A)~;~A~]~> ~15<~A~>  "
                   (incf *line-count*) (true-node? node)
                   (node-string node) antecedents)
           (pretty-print-clause (tms-node-support node))
           (setf (tms-node-mark node) *line-count*))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun show-node-consequences (node)
  (let ((conseqs (node-consequences node)))
    (cond (conseqs
           (format t "~% Consequences of ~A:" (signed-node-string node))
           (dolist (conseq conseqs)
                   (format t "~%  ~A" (signed-node-string conseq))))
          (t (format t "~% ~A has no consequences." (node-string node))))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun node-show-clauses (node)
  (format t "For ~A:" (node-string node))
  (dolist (cl (tms-node-true-clauses node))
    (format T "~%") (pretty-print-clause cl))
  (dolist (cl (tms-node-false-clauses node))
    (format T "~%") (pretty-print-clause cl)))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun explore-network (node)
  (unless (known-node? node)
          (format t "~% Sorry, ~A not believed." (node-string node))
          (return-from explore-network node))
  (do ((stack nil)
       (current node)
       (mode :ante)
       (options nil)
       (olen 0)
       (done? nil))
      (done? current)
      (cond ((eq mode :ante)
             (why-node current)
             (setq options (if (typep (tms-node-support current) 'clause)
                               (clause-antecedents (tms-node-support current))
                             nil)))
            (t ;; Looking at consequences
             (show-node-consequences current)
             (setq options (node-consequences current))))
      (setq olen (length options))
      (do ((good? nil)
           (choice 0))
          (good? (case good?
                       (q (return-from explore-network current))
                       (c (setq mode :conseq))
                       (a (setq mode :ante))
                       (0 (if stack
                              (setq current (pop stack))
                              (return-from explore-network current)))
                       (t (push current stack)
                          (setq current (nth (1- good?) options)))))
          (format t "~%>>>")
          (setq choice (read))
          (if (or (eq choice 'q)
                  (eq choice 'c)
                  (eq choice 'a)
                  (and (integerp choice)
                       (not (> choice olen))
                       (not (< choice 0))))
              (setq good? choice)
              (format t "~% Must be q, a, c or an integer from 0 to ~D."
                        olen)))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From explain.lisp in biohacker extensions
(defun node-all-antecedents (node)
  (clear-node-marks)
  (do ((todo (list node))
       (result nil)
       (current)
       (support))
      ((null todo) result)
    (setq current (car todo))
    (setq todo (cdr todo))
    (unless (tms-node-mark current)
      (setf (tms-node-mark current) t)
      (setq support (tms-node-support current))
      (unless (eq :ENABLED-ASSUMPTION support)
        (setq todo (append (clause-antecedents support) todo)))
      (push current result))))
</pre>
    *
    */

} // class Node
