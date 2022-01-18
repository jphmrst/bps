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

package org.maraist.truthmaintenancesystems.ruleengine
import scala.util.control.NonLocalReturns.*
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}
import java.io.PrintStream

// Tiny rule engine, translated from F/dK version 61 of 7/21/92.

/**
  * @tparam R Type of the representation of rules.
  * @tparam K The keys by which a fact can be indexed.
  * @tparam F The representation of facts.
  */
trait RuleFactImpl[R, K, F] {
}

type RuleBody = () => Unit

/**
  *
  * **Arguments and `val` members translated from**:
  * <pre>
; From rules.lisp
(proclaim '(special *TRE* *ENV*))
(defvar *ENV* nil)              ; Environment for rules

(defstruct (rule (:PRINT-FUNCTION (lambda (r st ignore)
                                    (format st "<Rule ~D>"
                                            (rule-counter r)))))
     counter            ;Integer to provide unique "name"
     Dbclass            ;Dbclass it is linked to.
     trigger            ;pattern it runs on.
     body               ;code to be evaluated in local environment.
     environment)       ;binding envirionment.

;;;; Interface for rules
</pre>
  *
  * @param title Name of this TMS, for output.
  *
  * @constructor The `title` argument is required; others are optional.
  *
  * @groupname construction Construction methods
  * @groupdesc construction API methods for building and changing
  * an ATMS from an external system.
  * @groupprio construction 1
  *
  * @groupname query Query methods
  * @groupdesc query API methods for querying the TRE and its beliefs
  * from an external system.  Note that most query-style methods are
  * on [[Node]]s.
  * @groupprio query 2
  *
  * @groupname diagnostic Diagnostic and debugging methods
  * @groupdesc diagnostic Reporting the current TRE state as text.
  * @groupprio diagnostic 3
  *
  * @groupname internal Internal methods
  * @groupdesc internal Implementation methods; not generally for use
  * from outside this package.
  * @groupprio internal 10
  */
class Rule[F](
  val counter: Int
) {

  /**
    *
    * <pre>
; From rules.lisp
(defun show-rules (&optional (stream *standard-output*) &aux counter)
  (setq counter 0)
  (maphash #'(lambda (key dbclass)
               (dolist (rule (dbclass-rules dbclass))
                       (incf counter)
                       (format stream "~%  ")
                       (print-rule rule stream)))
           (tre-dbclass-table *TRE*))
  counter)
</pre>
    */
  def showRules(stream: PrintStream = System.out): Unit = ???

  /**
    *
    * <pre>
; From rules.lisp
(defun print-rule (rule &optional (stream *standard-output*))
  (format stream "Rule ~A: ~A; ~A"       ;don't show body, too big
          (rule-counter rule)
          ;; Plug in the variables, to show how much has been done.
          (sublis (rule-environment rule)
                  (rule-trigger rule))
          (rule-environment rule)))
</pre>
    */
  def printRule(stream: PrintStream = System.out): Unit = ???

  /**
    *
    * <pre>
;; Sugar for the user (or other programs!)
; From rules.lisp
(defmacro rule (trigger &rest body) `(add-rule ',trigger ',body))
</pre>
    */
  inline def rule(trigger: F, body: RuleBody): Rule[F] = ???

}
