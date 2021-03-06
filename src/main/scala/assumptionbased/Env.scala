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
  * Table storing [[Env][environments]] in buckets corresponding to
  * [[Env]] assumption list length.
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
class EnvTable[D, I, R] extends HashMap[Int, ListBuffer[Env[D, I, R]]] {

  /**
    * Print the environments stored in this table.
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun print-env-table (table stream)
  (dolist (bucket table)
    (dolist (env (cdr bucket))
      (print-env env stream))))

(defun print-table (msg table)
  (format t msg)
  (dolist (entry table)
    (format t "~%   Length ~D, ~D" (car entry)
            (length (cdr entry)))))
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
    * Update this table to store a new environment.
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
  def insertInTable(env: Env[D, I, R]): Unit = {
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
  * Internal class representing the outcomes of comparing two
  * environments.
  */
enum EnvCompare {
  /**
    * Represents the case that the first environment is a proper
    * subset of the second.
    */
  case S12 extends EnvCompare
  /**
    * Represents the case that the first environment is a proper
    * superset of the second.
    */
  case S21 extends EnvCompare
  /**
    * Represents the case that the two environments contain the same
    * assumptions.
    */
  case EQ extends EnvCompare
  /**
    * Represents the case that neither environment is a proper subset
    * of the other.
    */
  case Disjoint extends EnvCompare
}

/**
  * Representation of a set of beliefs in some subset of the nodes of
  * an ATMS.
  *
  * **Arguments and `val` members translated from**:
  *
  * <pre>
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
</pre>
  *
  * **Untranslated.** These Lisp functions are not used in the rest of
  * F&dK's code, and are omitted from the Scala translation.
  *
  * <pre>
; From ainter.lisp
(defun env-order (e1 e2)
  (< (env-index e1) (env-index e2)))
</pre>
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
class Env[D, I, R](
  val index: Int,
  val assumptions: List[Node[D, I, R]]
) {

  /** Number of assumptions. */
  val count = assumptions.length

  /** If this node is nogood, stores the evidence. */
  var nogoodEvidence: Option[Justification[D, I, R] | Env[D, I, R]] = None

  /** Returns `true` when there is evidence that this environment is nogood.
    *
    * @group query
    */
  inline def isNogood: Boolean = !nogoodEvidence.isEmpty

  val nodes: ListBuffer[Node[D, I, R]] = ListBuffer.empty

  /** Call this if becomes nogood */
  val rules: ListBuffer[R] = ListBuffer.empty

  /**
    * Check whether any union of antecedent environments is
    * consistent.
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
                   ;; Strictly speaking, this return exits the
                   ;; DOLIST only.  But that result will also
                   ;; be the overall result of the function.
                   (return T)))))))
</pre>
    *
    * @group internal
    */
  def isWeave(nodes: List[Node[D, I, R]]): Boolean = returning {
    if nodes.isEmpty then true
    else {
      for (e <- nodes.head.label) do {
        val newEnv = e.unionEnv(this)
        if !newEnv.isNogood && newEnv.isWeave(nodes.tail)
        then throwReturn(true)
      }
      false
    }
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
  def unionEnv(that: Env[D, I, R]): Env[D, I, R] = returning[Env[D, I, R]]{
    val disordered = count > that.count
    val e1 = if disordered then that else this
    var e2: Env[D, I, R] = if disordered then this else that
    for (assume <- e1.assumptions) do {
      e2 = e2.consEnv(assume)
      if e2.isNogood then throwReturn(e2)
    }
    e2
  }

  /**
    * Returns the result of extending this `Env` with an additional
    * [[Node]].  Note that by using [[Env#orderedInsert]], this method
    * will drop duplicate [[Node]]s.
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
  def consEnv(assumption: Node[D, I, R]): Env[D, I, R] =
    assumption.atms.getEnv(
      Env.orderedInsert(assumption, assumptions, Env.assumptionOrder))

  /**
    * Test whether one `Env`'s assumptions are a subset of another
    * `Env`'s assumptions.
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
  def isSubsetEnv(e2: Env[D, I, R]): Boolean = compareEnv(e2) match {
    case EnvCompare.S12 => true
    case EnvCompare.EQ => true
    case _ => false
  }

  /**
    * Compare the assumption lists of two `Env`s, returning one of the
    * four cases of [[EnvCompare]].
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
  def compareEnv(e2: Env[D, I, R]): EnvCompare = {
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

  def isSupersetEnvOf(e2: Env[D, I, R]): Boolean = compareEnv(e2) match {
    case EnvCompare.S21 => true
    case EnvCompare.EQ  => true
    case _ => false
  }

  /**
    * Returns `true` if every given node is believed under the given
    * environment.
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
  def isSupportingAntecedent(
    nodes: Iterable[Node[D, I, R]], env: Env[D, I, R]):
      Boolean =
    !nodes.exists(!_.isInNodeUnder(env))

  /**
    * Print the assumptions of this `Env` on one line.
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
    * Return a single-line string detailing the assumptions in this
    * `Env`.
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
    * Return the result of inserting a [[Node]] into the given ordered
    * (by internal index) list of [[Node]]s, inserting nothing if the
    * new [[Node]] is already present.
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
  def orderedInsert[D, I, R](
    item: Node[D, I, R],
    list: List[Node[D, I, R]],
    test: (Node[D, I, R], Node[D, I, R]) => Boolean):
      List[Node[D, I, R]] = list match {
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
    * Comparison function on the internal index of two nodes.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun assumption-order (a1 a2)
  (< (tms-node-index a1) (tms-node-index a2)))
</pre>
    */
  def assumptionOrder[D, I, R](a1: Node[D, I, R], a2: Node[D, I, R]): Boolean =
    a1.index < a2.index
}
