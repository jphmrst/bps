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

class Just[D, I] {
  // (defstruct (just (:PRINT-FUNCTION print-just))
  //   (index 0)
  //   informant
  //   consequence
  //   antecedents)

  // (defun create-jtms (title &key (node-string 'default-node-string)
  //                                debugging
  //                                (checking-contradictions t)
  //                                (contradiction-handler 'ask-user-handler)
  //                                enqueue-procedure)
  //   (make-jtms :TITLE title
  //         :NODE-STRING node-string
  //         :DEBUGGING debugging
  //         :CHECKING-CONTRADICTIONS checking-contradictions
  //         :CONTRADICTION-HANDLER contradiction-handler
  //         :ENQUEUE-PROCEDURE enqueue-procedure
  //         ))

  // (defun print-just (just stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "#<Just ~D>" (just-index just)))

  // (defun change-jtms (jtms &key contradiction-handler node-string
  //                          enqueue-procedure debugging
  //                               checking-contradictions)
  //   (if node-string (setf (jtms-node-string jtms) node-string))
  //   (if debugging (setf (jtms-debugging jtms) debugging))
  //   (if checking-contradictions
  //       (setf (jtms-checking-contradictions jtms)
  //        checking-contradictions))
  //   (if contradiction-handler
  //       (setf (jtms-contradiction-handler jtms) contradiction-handler))
  //   (if enqueue-procedure
  //       (setf (jtms-enqueue-procedure jtms) enqueue-procedure)))

  // (defun check-justification (just)
  //   (and (out-node? (just-consequence just))
  //        (justification-satisfied? just)))

  // (defun justification-satisfied? (just)
  //   (every #'in-node? (just-antecedents just)))

} // class Just
