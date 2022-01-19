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
  * @tparam K The keys by which a fact can be indexed.
  * @tparam F The representation of facts.
  */
trait FactImpl[K, F] {
  def getIndexer(fact: F): K
  def unify(s: F, t: F): Bindings[K, F]
  def subst(t: F, b: Bindings[K, F]): F
}

type Bindings[K, E] = Option[Map[K, E]]

extension [K, E](bnd: Bindings[K, E])
  def failed: Boolean = bnd.isEmpty
  def succeeded: Boolean = !bnd.isEmpty
