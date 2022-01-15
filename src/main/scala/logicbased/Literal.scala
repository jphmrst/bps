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

package org.maraist.truthmaintenancesystems.logicbased
import scala.util.control.NonLocalReturns.*

class Literal[D, I, R](val node: Node[D, I, R], val label: Label)

object Literal {

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun sort-clause (literals)
  (sort (copy-list literals) ;; Avoids shared structure bugs.
     #'< :KEY #'(lambda (n) (tms-node-index (car n)))))
</pre>
    *
    */
  def sortClause[D, I, R](ls: List[Literal[D, I, R]]):
      List[Literal[D, I, R]] = ???

}
