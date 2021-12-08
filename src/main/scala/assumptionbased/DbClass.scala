// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University
// and Johan de Kleer, the Xerox Corporation.
// Copyright (C) 2021 John Maraist.
// All rights reserved.

// See the LICENSE.txt and README-forbus-dekleer.txt files distributed
// with this work for a paragraph stating scope of permission and
// disclaimer of warranty, and for additional information regarding
// copyright ownership.  The above copyright notice and that paragraph
// must be included in any separate copy of this file.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.truthmaintenancesystems.assumptionbased
import scala.util.control.NonLocalReturns.*
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}

// Assumption-based truth maintenance system, translated from F/dK
// version 61 of 7/21/92.

class DbClass[F, I]() {

  // ; From ainter.lisp
  // (defstruct (dbclass (:PRINT-FUNCTION print-atre-dbclass))
  //   name    ; Corresponding symbol
  //   atre    ; ATRE it is part of.
  //   facts   ; Associated facts
  //   rules)   ; Associated rules
  //
  // (defun print-atre-dbclass  (r st ignore)
  //   (declare (ignore ignore))
  //   (format st "<Dbclass ~A>" (dbclass-name r)))
}
