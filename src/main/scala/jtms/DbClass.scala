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

class DbClass[I](
  val name: Any,
  val jtre: JTRE[I],
  val fact: List[Any],
  val rules: List[Rule[I]]
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

  def insertRule[V](
    matcher: (Matchable) => Option[V],
    body: (JTMS[I], JTRE[I], V) => Unit):
      Unit = ???
  // (defun insert-rule (dbclass matcher body &aux rule)
  //   (let ((*JTRE* (dbclass-jtre dbclass)))
  //     (setq rule (make-rule :MATCHER matcher
  //                      :BODY body
  //                      :DBCLASS dbclass
  //                      :ID (incf (jtre-rule-counter *JTRE*))))
  //     (push rule (dbclass-rules dbclass))
  //     (dolist (candidate (dbclass-facts dbclass))
  //        (try-rule-on rule candidate))))

  def insert(fact: Matchable): Datum[I] = ???
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
}

  // (defmacro rassert! (fact just)
  //   `(assert! ,(quotize fact) ,(quotize just)))

  // (defun already-assumed? (fact  &aux r)
  //   (when (setq r (referent fact))
  //     (datum-assumption? r)))

  // ;;;; Retraction

  // (defmacro rretract! (fact &optional (just 'USER))
  //   `(retract! ,(quotize fact) ,(quotize just)))


  // (defun referent1 (fact)
  //   (dolist (candidate (dbclass-facts (get-dbclass fact)))
  //      (when (equal (datum-lisp-form candidate) fact)
  //            (return candidate))))

  // (defun get-candidates (pattern)
  //   (dbclass-facts (get-dbclass pattern)))

  // ;;;; More query routines
