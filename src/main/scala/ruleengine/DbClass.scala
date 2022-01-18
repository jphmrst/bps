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

// Tiny rule engine, translated from F/dK version 61 of 7/21/92.

/**
  *
  * **Arguments and `val` members translated from**:
  * <pre>
;; From data.lisp
(proclaim '(special *TRE* *ENV*))

(defstruct (dbclass (:PRINT-FUNCTION (lambda (d st ignore)
                                     (format st "<Dbclass ~D>"
                                             (dbclass-name d)))))
     name               ;a symbol
     tre                ;The TRE it belongs to
     facts              ;facts of this dbclass
     rules)             ;rules applicable to this dbclass
</pre>
  *
  * @param name Symbol associated with the facts and rules in this
  * class.
  * @param tre [[TRE]] with which this organizer is associated.
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
class DbClass[F](val name: Symbol, val tre: TRE[F]) {

  /**
    * Facts of this dbclass.
    */
  val facts: ListBuffer[F] = new ListBuffer

  /**
    * Rules applicable to this dbclass.
    */
  val rules: ListBuffer[Rule[F]] = new ListBuffer

}
