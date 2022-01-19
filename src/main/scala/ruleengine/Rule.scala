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

type RuleBody = () => Unit

trait Rule[K, F] {
  def counter: Int
  def className: K
  def apply(fact: F): Unit
  type Runnable
  def run(r: Runnable): Unit
  def printRule(stream: PrintStream): Unit
}

object Rule {

  def apply[K, F, E](
    counter: Int,
    className: K,
    environment: Bindings[K, F],
    t: (F) => Option[E],
    b: (E) => Unit,
    formatT: String):
      Rule[K, F] =
    new RuleImpl(counter, className, environment) {
      type Runnable = E
      val trigger: (F) => Option[E] = t
      val body: (Runnable) => Unit = b
      val formatTrigger: String = formatT
    }

}

class RuleRunnable[K, F](val rule: Rule[K, F], val runnable: rule.Runnable) {
  def run: Unit = rule.run(runnable)
}

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
  */
abstract class RuleImpl[K, F](
  val counter: Int,
  val className: K,
  val environment: Bindings[K, F]
) extends Rule[K, F] {
  type Runnable
  def trigger: (F) => Option[Runnable]
  def body: (Runnable) => Unit

  def apply(fact: F): Unit = trigger(fact) match {
    case None => { }
    case Some(r) => body(r)
  }

  /**
    *
    * <pre>
; From rules.lisp
(defun run-rule (pair tre)
  ;; Here pair is (<body> . <bindings>).  The LET makes
  ;; the bindings available to nested rules.
  (let ((*ENV* (cdr pair))
        (*TRE* tre))
    (incf (tre-rules-run tre))
    ;; Now we build a form that creates the right environment.
    ;; We will see better ways to do this later.
    (eval `(let ,(mapcar #'(lambda (binding)
                             `(,(car binding)
                               ',(sublis (cdr pair)
                                         (cdr binding))))
                         (cdr pair))
             ,@ (car pair)))))
</pre>
    */
  def run(r: Runnable): Unit = body(r)

  def formatTrigger: String

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
  def printRule(stream: PrintStream = System.out): Unit = // TODO nsublis
    stream.println(s"Rule $counter: $formatTrigger; $environment")
}
