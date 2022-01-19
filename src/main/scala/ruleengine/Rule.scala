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

trait Rule[F] {
  def counter: Int
  def dbClass: Symbol
}

object Rule {

  def apply[F, E](
    counter: Int,
    dbClass: Symbol,
    environment: Bindings[F],
    t: (F) => Option[E],
    b: (E) => Unit,
    formatT: String):
      Rule[F] =
    new RuleImpl(counter, dbClass, environment) {
      type Extraction = E
      val trigger: (F) => Option[E] = t
      val body: (Extraction) => Unit = b
      val formatTrigger: String = formatT
    }

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
abstract class RuleImpl[F](
  val counter: Int,
  val dbClass: Symbol,
  val environment: Bindings[F]
) extends Rule[F] {
  type Extraction
  def trigger: (F) => Option[Extraction]
  def body: (Extraction) => Unit

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
