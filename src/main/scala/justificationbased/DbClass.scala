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
import scala.collection.mutable.{ListBuffer, HashSet, HashMap}
import scala.util.control.NonLocalReturns.*

type DbClassTable[I] = HashMap[Any, Any]

class DbClass[I](
  val name: Any,
  val jtre: JTRE[I],
  val facts: ListBuffer[Datum[I]],
  val rules: ListBuffer[Rule[I]]
) {
  // (defstruct (dbclass (:PRINT-FUNCTION jtre-dbclass-printer))
  //   name    ; Corresponding symbol
  //   jtre    ; JTRE it is part of.
  //   facts   ; Associated facts
  //   rules)  ; Associated rules

  override def toString(): String = s"<DbClass $name>"
  def jtreDbClassPrinter: Unit = print(toString)
  //  (defun jtre-dbclass-printer (r st ignore)
  //    (declare (ignore ignore))
  //    (format st "<Dbclass ~A>" (dbclass-name r)))

  def insertRule[Intermed](
    matcherFn: (Fact) => Option[Intermed],
    bodyFn: (JTMS[Datum[I], I], JTRE[I], Intermed) => Unit):
      Unit = {
    val id = jtre.incfRuleCounter
    val rule = new Rule[I](id, this) {
      type V = Intermed
      def matcher(m: Fact): Option[V] = matcherFn(m)
      def body(jtms: JTMS[Datum[I], I], jtre: JTRE[I], values: V): Unit =
        bodyFn(jtms, jtre, values)
    }
    rules += rule
    for (candidate <- facts) do rule.tryRuleOn(candidate)
  }
  // (defun insert-rule (dbclass matcher body &aux rule)
  //   (let ((*JTRE* (dbclass-jtre dbclass)))
  //     (setq rule (make-rule :MATCHER matcher
  //                      :BODY body
  //                      :DBCLASS dbclass
  //                      :ID (incf (jtre-rule-counter *JTRE*))))
  //     (push rule (dbclass-rules dbclass))
  //     (dolist (candidate (dbclass-facts dbclass))
  //        (try-rule-on rule candidate))))

  /**
    * FILLIN
    *
    * Note that the post-initialization setup of the datum instance in
    * the Lisp code is now done by the Datum object initialization
    * itself.
    *
    * @param fact
    * @return
    */
  def insert(fact: Fact): (Datum[I], Boolean) = referent1(fact) match {
    case Some(datum) => (datum, true)
    case None => {
      val datum = new Datum[I](jtre, fact)
      tryRules(datum)
      (datum, false)
    }
  }
  // (defun insert (fact &aux datum)
  //   (setq datum (referent1 fact))
  //   (cond (datum (values datum t))
  //    (t (setq datum
  //             (make-datum
  //              :ID (incf (jtre-datum-counter *JTRE*))
  //              :LISP-FORM fact
  //              :DBCLASS (get-dbclass fact)))
  //       (setf (datum-tms-node datum)
  //             (tms-create-node (jtre-jtms *JTRE*) datum))
  //       (push datum (dbclass-facts (datum-dbclass datum)))
  //       (try-rules datum)
  //       (values datum nil))))

  def tryRules(datum: Datum[I]): Unit = {
    for (rule <- datum.dbClass.rules) do rule.tryRuleOn(datum)
  }
  // (defun try-rules (datum)
  //   (dolist (rule (dbclass-rules (datum-dbclass datum)))
  //     (try-rule-on rule datum)))

  def referent1(fact: Fact): Option[Datum[I]] = returning {
    for(candidate <- facts)
      do if candidate.fact == fact
         then throwReturn[Option[Datum[I]]](Some(candidate))
    None
  }
  // (defun referent1 (fact)
  //   (dolist (candidate (dbclass-facts (get-dbclass fact)))
  //      (when (equal (datum-lisp-form candidate) fact)
  //            (return candidate))))

  def getCandidates(pattern: Fact): ListBuffer[Datum[I]] =
    jtre.getDbClass(pattern).facts
  // (defun get-candidates (pattern)
  //   (dbclass-facts (get-dbclass pattern)))
}

  // (defmacro rassert! (fact just)
  //   `(assert! ,(quotize fact) ,(quotize just)))

  // ;;;; Retraction

  // (defmacro rretract! (fact &optional (just 'USER))
  //   `(retract! ,(quotize fact) ,(quotize just)))


  // ;;;; More query routines
