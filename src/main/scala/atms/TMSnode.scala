// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University
// and Johan de Kleer, the Xerox Corporation.
// Copyright (C) 2021 John Maraist.
// All rights reserved.
//
// See the LICENSE.txt and README-forbus-dekleer.txt files distributed
// with this work for a paragraph stating scope of permission
// and disclaimer of warranty, and for additional
// information regarding copyright ownership.    The above copyright notice and that
// paragraph must be included in any separate copy of this file.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.


// Translated from KDF/JdK version 61 of 7/21/92.

package org.maraist.tms.atms

// Definitions

class TMSnode(val atms: ATMS) {
  /** Unique name. */
  var index = 0
  /** Pointer to IE data structures. */
  var datum = {}
  /** Minimal envs believed under */
  var label = {}
  /** Providers of support */
  var justs = {}
  /** Provides support for. */
  var consequences = {}
  /** Flag marking it as contradictory. */
  var isContradictory: Boolean = false
  /** Flag marking it as n assumption. */
  var isAssumption: Boolean = false
  /** Run when label non-empty. */
  var rules = {}

  override def toString(): String =
    if isAssumption then s"A-$index" else s"#<NODE: ${atms.nodeString}>"
}
