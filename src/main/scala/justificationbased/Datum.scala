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

class Datum[I] private[justificationbased] (
  val id: Int,
  val fact: Fact,
  val dbClass: DbClass[I]
) {
  var node: Node[I] = ???

  var isAssumption: Boolean = ???

  // (defstruct (datum (:PRINT-FUNCTION jtre-datum-printer))
  //   id                   ; Unique ID for easy lookup
  //   lisp-form            ; Expression for pattern-matching
  //   (tms-node nil)       ; Pointer into TMS
  //   dbclass              ; Dbclass of the corresponding pattern
  //   (assumption? nil)    ; if non-nil, indicates informant
  //   (plist nil))         ; local property list

  override def toString: String = s"<Datum $id>"
  def jtreDatumPrinter: Unit = println(this.toString)
  // (defun jtre-datum-printer (d st ignore)
  //   (declare (ignore ignore))
  //   (format st "<Datum ~D>" (datum-id d)))

  def showDatum: String = fact.toString
  // (defun show-datum (datum)
  //   (format nil "~A" (datum-lisp-form datum)))
}

