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

package org.maraist.truthmaintenancesystems.justificationbased.tests
import scala.language.adhocExtensions
import scala.collection.mutable.ListBuffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.maraist.truthmaintenancesystems.justificationbased.*

trait JTMSexample[DatumType, InformantType, R](name: String) {
  val j = new JTMS[DatumType, InformantType, R](name, debugging = false)

  def beliefsString: String
  def contradictoryString: String

  def showAll(tag: String): Unit = {
    println(tag)
    showBeliefs("  Believed :: ")
    showContradictory("  Contradictory :: ")
  }

  def showBeliefs(tag: String = ""): Unit = println(s"$tag$beliefsString")

  def showContradictory(tag: String = ""): Unit =
    println(s"$tag$contradictoryString")
}

// (defun get-node (datum jtms)
//   (dolist (node (jtms-nodes jtms))
//     (if (equal datum (tms-node-datum node)) (return node))))

// (defun get-justification (num jtms)
//   (dolist (just (jtms-justs jtms))
//     (if (= num (just-index just)) (return just))))

// (proclaim '(special na nb nc nd ne nf ng contra *jtms*))


