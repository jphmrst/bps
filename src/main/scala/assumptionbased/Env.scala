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

package org.maraist.truthmaintenancesystems.assumptionbased
import scala.util.control.NonLocalReturns.*
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}

// Assumption-based truth maintenance system, translated from F/dK
// version 61 of 7/21/92.

/**
  * TODO fill in description
  *
  * @groupname diagnostic Diagnostic and debugging methods
  * @groupdesc diagnostic Reporting the current JTMS state as text.
  * @groupprio diagnostic 3
  *
  * @groupname internal Internal methods
  * @groupdesc internal Implementation methods; not generally for use
  * from outside this package.
  * @groupprio internal 10
  */
class EnvTable[D, I] extends HashMap[Int, ListBuffer[Env[D, I]]] {

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun print-env-table (table stream)
  (dolist (bucket table)
    (dolist (env (cdr bucket))
      (print-env env stream))))
</pre>
    *
    * @group diagnostic
    */
  def printEnvTable(prefix: String): Unit = {
    var e = 0
    for ((length, envs) <- this)
      do for (env <- envs) do {
        e = e + 1
        println(s"$prefix$e. ${env.envString}")
      }
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun insert-in-table (table env &aux count entry)
  (setq count (env-count env)
        entry (assoc count table :TEST #'=))
  (cond (entry (setf (cdr entry) (cons env (cdr entry))) table)
        (t (ordered-insert
             (list count env) table
             #'(lambda (entry1 entry2)
                 (< (car entry1) (car entry2)))))))
</pre>
    *
    * @group internal
    */
  def insertInTable(env: Env[D, I]): Unit = {
    val count = env.count
    this.get(count) match {
      case None => this(count) = ListBuffer(env)
      case Some(entry) => entry += env
    }
  }

  /**
    * Return the number of environments in this table.
    *
    * @group internal
    */
  def envCount: Int = map((n,es) => es.length).foldRight(0)(_ + _)
}

/**
  * TODO fill in description
  */
enum EnvCompare {
  case S12 extends EnvCompare
  case S21 extends EnvCompare
  case EQ extends EnvCompare
  case Disjoint extends EnvCompare
}

/**
  * TODO fill in documentation
  *
  * @param index Internal identifier for this environment, distinct
  * among the environments of this [[ATMS]].
  * @param assumptions The assumption nodes associated with this
  * environment.
  *
  * @groupname diagnostic Diagnostic and debugging methods
  * @groupdesc diagnostic Reporting the current JTMS state as text.
  * @groupprio diagnostic 3
  *
  * @groupname internal Internal methods
  * @groupdesc internal Implementation methods; not generally for use
  * from outside this package.
  * @groupprio internal 10
  */
class Env[D, I](
  val index: Int,
  val assumptions: List[Node[D, I]]
) {

  /** Number of assumptions. */
  val count = assumptions.length

  /** If this node is nogood, stores the evidence. */
  var nogoodEvidence: Option[Justification[D, I] | Env[D, I]] = None

  /** Returns `true` when there is evidence that this environment is nogood.
    *
    * @group query
    */
  inline def isNogood: Boolean = !nogoodEvidence.isEmpty

  val nodes: ListBuffer[Node[D, I]] = ListBuffer.empty

  /** Call this if becomes nogood */
  val rules: ListBuffer[Rule[I]] = ListBuffer.empty

  // ; From atms.lisp
  // (defstruct (env (:PREDICATE env?)
  //                 (:PRINT-FUNCTION print-env-structure))
  //            (index 0)
  //            (count 0)                    ; Number of assumptions.
  //            (assumptions nil)
  //            (nodes nil)
  //            (nogood? nil)
  //            (rules nil))                 ; Call this if becomes nogood.
  //
  // (defun print-env-structure (env stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "E-~D" (env-index env)))

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun weave? (env nodes &aux new-env)
  (cond ((null nodes) t)
        (t (dolist (e (tms-node-label (car nodes)))
             (setq new-env (union-env e env))
             (unless (env-nogood? new-env)
               (if (weave? new-env (cdr nodes))
                   (return T)))))))
</pre>
    *
    * @group internal
    */
  def isWeave(nodes: Iterable[Node[D, I]]): Boolean = {
    ???
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun env-order (e1 e2)
  (< (env-index e1) (env-index e2)))
</pre>
    *
    * @group internal
    */
  def envOrder(e2: Env[D, I]): Boolean = {
    ???
  }

  /**
    * Internal method returning the minimum environment entailing both
    * `this` and `that` environments.
    *
    * @returns If `this` and `that` are not mutually consistent, then
    * the result is the union of `that` with the assumptions of `this`
    * up to the inconsistency.
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun union-env (e1 e2)
  (when (> (env-count e1)
           (env-count e2))
    (psetq e1 e2 e2 e1))
  (dolist (assume (env-assumptions e1))
    (setq e2 (cons-env assume e2))
    (if (env-nogood? e2) (return nil)))
  e2)
</pre>
    *
    * @group internal
    */
  def unionEnv(that: Env[D, I]): Env[D, I] = returning[Env[D, I]]{
    val disordered = count > that.count
    val e1 = if disordered then that else this
    var e2: Env[D, I] = if disordered then this else that
    for (assume <- e1.assumptions) do {
      e2 = e2.consEnv(assume)
      if e2.isNogood then throwReturn(e2)
    }
    e2
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun cons-env (assumption env &aux nassumes)
  (setq nassumes (ordered-insert assumption
                                 (env-assumptions env)
                                 #'assumption-order))
  (or (lookup-env nassumes)
      (create-env (tms-node-atms assumption) nassumes)))
</pre>
    *
    * @group internal
    */
  def consEnv(assumption: Node[D, I]): Env[D, I] =
    assumption.atms.getEnv(
      Env.orderedInsert(assumption, assumptions, Env.assumptionOrder))

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun subset-env? (e1 e2)
  (cond ((eq e1 e2) t)
        ((> (env-count e1)
            (env-count e2)) nil)
        ((subsetp (env-assumptions e1)
                  (env-assumptions e2)))))
</pre>
    *
    * @group internal
    */
  def isSubsetEnv(e2: Env[D, I]): Boolean = compareEnv(e2) match {
    case EnvCompare.S12 => true
    case EnvCompare.EQ => true
    case _ => false
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun compare-env (e1 e2)
  (cond ((eq e1 e2) :EQ)
        ((< (env-count e1) (env-count e2))
         (if (subsetp (env-assumptions e1)
                      (env-assumptions e2))
             :S12))
        ((subsetp (env-assumptions e2) (env-assumptions e1))
         :S21)))
</pre>
    *
    * @group internal
    */
  def compareEnv(e2: Env[D, I]): EnvCompare = {
    if this == e2
    then EnvCompare.EQ
    else if count < e2.count
    then {
      if Env.subsetp(assumptions, e2.assumptions)
      then EnvCompare.S12
      else EnvCompare.Disjoint
    } else {
      if Env.subsetp(e2.assumptions, assumptions)
      then EnvCompare.S21
      else EnvCompare.Disjoint
    }
  }

  def isSupersetEnvOf(e2: Env[D, I]): Boolean = compareEnv(e2) match {
    case EnvCompare.S21 => true
    case EnvCompare.EQ  => true
    case _ => false
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun supporting-antecedent? (nodes env)
  (dolist (node nodes t) (unless (in-node? node env) (return nil))))
</pre>
    *
    * @group internal
    */
  def isSupportingAntecedent(nodes: Iterable[Node[D, I]]): Boolean = {
    ???
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun print-env (e &optional (stream t))
  (format stream "~%~A:~A"
          e (if (env-nogood? e)
                "* " " "))
  (env-string e stream))
</pre>
    *
    * @group diagnostic
    */
  def printEnv(prefix: String): Unit =
    println(s"${prefix}$envString")

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun env-string (e &optional stream
                     &aux assumptions strings printer)
  (setq assumptions (env-assumptions e))
  (when assumptions
    (setq printer (atms-node-string (tms-node-atms (car assumptions)))))
  (dolist (a assumptions) (push (funcall printer a) strings))
  (format stream "{~{~A~^,~}}" (sort strings #'string-lessp)))
</pre>
    *
    * @group diagnostic
    */
  def envString: String = (
    (if this.isNogood then "[X] " else "") +
      (assumptions.length match {
        case 0 => "(empty)"
        case _ =>
          assumptions.map((a) => a.atms.nodeString(a)).toList.mkString(", ")

      })
  )
}

/**
  * Internal helper functions on [[Env][environments]].
  */
object Env {
  /**
    * For Lisp calls to `subsetp`.
    */
  def subsetp[A](xs: List[A], ys: List[A]): Boolean = returning {
    for (x <- xs) do if !ys.contains(x) then throwReturn(false)
    true
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun ordered-insert (item list test)
  (cond ((null list) (list item))
        ((funcall test item (car list)) (cons item list))
        ((eq item (car list)) list)
        (t (cons (car list) (ordered-insert item (cdr list) test)))))
</pre>
    */
  def orderedInsert[D, I](
    item: Node[D, I],
    list: List[Node[D, I]],
    test: (Node[D, I], Node[D, I]) => Boolean):
      List[Node[D, I]] = list match {
    case Nil => List(item)
    case (car :: cdr) => {
      if test(item, car)
      then item :: list
      else if item == car
      then list
      else car :: orderedInsert(item, cdr, test)
    }
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun assumption-order (a1 a2)
  (< (tms-node-index a1) (tms-node-index a2)))
</pre>
    */
  def assumptionOrder[D, I](a1: Node[D, I], a2: Node[D, I]): Boolean =
    a1.index < a2.index
}

// ; From ainter.lisp
// (defun print-table (msg table)
//   (format t msg)
//   (dolist (entry table)
//     (format t "~%   Length ~D, ~D" (car entry)
//             (length (cdr entry)))))
