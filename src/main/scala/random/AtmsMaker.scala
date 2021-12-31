// Copyright (C) 2021, 2022 John Maraist.
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

package org.maraist.truthmaintenancesystems.random
import scala.util.Random
import org.maraist.truthmaintenancesystems.assumptionbased.ATMS

class AtmsMaker(random: Random) extends Maker(random) {
  def this(seed: Int)  = this(new Random(seed))
  def this(seed: Long) = this(new Random(seed))
  def this() = this(new Random())

  def makeATMS(
    assumptionsRange: IntRange = IntRange(300, 300),
    nonassumptionsRange: IntRange = IntRange(8000, 8000),
    contradictionChance: Float = 0.05,
    justificationsPerConclusion: IntRange = IntRange(10, 12),
    antecedentsPerJustifications: IntRange = IntRange(6, 12),
    cyclic: Boolean = false
  ):
      ATMS[Int, String, Nothing] = {
    val atms = new ATMS[Int, String, Nothing]("Random ATMS")

    val assumptions = assumptionsRange.get
    val nonassumptions = nonassumptionsRange.get
    val totalNodes = assumptions + nonassumptions

    for (i <- 0 until assumptions) do atms.createNode(i, isAssumption = true)

    for (i <- 0 until nonassumptions) do {
      val isContradiction = random.nextFloat < contradictionChance
      val idx = assumptions + i
      atms.createNode(idx, isContradictory = isContradiction)
    }

    for (i <- 0 until nonassumptions) do {
      val idx = assumptions + i
      val node = atms.nodes(idx)
      val justifications = justificationsPerConclusion.get
      for (j <- 0 until justifications) do {
        val thisSize = antecedentsPerJustifications.get
        val ants = (
          if cyclic
            then intSet(thisSize, IntRange(0, idx))
          else intSetExcept(thisSize, IntRange(0, totalNodes), j)
        ).map(atms.nodes(_))
        atms.justifyNode(s"$idx.$j", node, ants)
      }
    }

    atms
  }
}

@main def randomAtms: Unit = {
  import java.util.Date
  val maker = new AtmsMaker()
  var totalMS: Long = 0
  val runs: Int = 100

  for (i <- 1 to runs) do {
    java.lang.System.gc()
    print(s"($i) ")
    val start = new Date
    val atms = maker.makeATMS()
    val end = new Date
    val elapsed = end.getTime - start.getTime
    println(f"${atms.nodes.size}%,d nodes, ${atms.justs.size}%,d justifications, elapsed time ${elapsed}%,dms")
    totalMS = totalMS + elapsed
  }
  println(f"Average time ${totalMS / runs}%,d")
}
