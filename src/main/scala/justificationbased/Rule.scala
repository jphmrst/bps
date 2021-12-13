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

abstract class Rule[D, I](val id: Int
  // , val dbClass: DbClass[I]
) {
  //  type V
  //  def matcher(m: Fact): Option[V]
  //  def body(jtre: JTRE[I], values: V): Unit

  // (defstruct (rule (:PRINT-FUNCTION jtre-rule-printer))
  //   id           ; Unique ID for easy lookup
  //   jtre         ; The JTRE it is part of
  //   dbclass      ; Dbclass of associated pattern
  //   matcher      ; Procedure that performs the match.
  //   body)        ; Procedure that does the work.

  override def toString: String = s"<Rule $id>"
  def jtreRulePrinter: Unit = print(toString)
  def printRule: Unit = print(toString)
  // (defun jtre-rule-printer (r st ignore)
  //   (declare (ignore ignore))
  //   (format st "<Rule ~D>" (rule-id r)))
  //
  // (defun print-rule (rule &optional (stream *standard-output*))
  //   (format stream "~% ~A: ~A, ~A" rule
  //      (rule-matcher rule) (rule-body rule)))
}
