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
import scala.collection.mutable.{ListBuffer, HashSet, HashMap}

// Definitions

inline def dbg[D](atms: ATMS[D], msg: String) =
  if atms.debugging then println(msg)
// (defmacro debugging (atms msg &optional node &rest args)
//   `(when (atms-debugging ,atms)
//      (format *trace-output*
//              ,msg (if ,node (nodeString ,node)) ,@args)))

class Contra private ()
object Contra extends Contra {
  override def toString(): String = "contradiction"
}

type Rule = Unit

/*

(defmacro ordered-push (item list test)
  `(setq ,list (ordered-insert ,item ,list ,test)))

;;; Basic inference engine interface.


// (defun change-atms (atms &key nodeString
//                               enqueueProcedure debugging)
//   (if nodeString (setf (atms-nodeString atms) nodeString))
//   (if debugging (setf (atms-debugging atms) debugging))
//   (if enqueueProcedure
//       (setf (atms-enqueueProcedure atms) enqueueProcedure)))

;;; Methods on other classes

;; (defun update-label (node new-envs &aux envs) ==> On TMSnode

*/

class EnvTable[D] extends HashMap[Int, EnvList[D]] {
  def insert(env: Env[D]): Unit = {
    ???

    // (defun insert-in-table (table env &aux count entry)
    //   (setq count (env-count env)
    //         entry (assoc count table :TEST #'=))
    //   (cond (entry (setf (cdr entry) (cons env (cdr entry))) table)
    //         (t (ordered-insert
    //              (list count env) table
    //              #'(lambda (entry1 entry2)
    //                  (< (car entry1) (car entry2)))))))
  }
}

/**
  *
  *
  * @param title
  * @param nodeString
  * @param debugging Trace grungy details.
  */
class ATMS[D](
  val title: String,
  val nodeString: (TMSnode[D]) => String =
    (n: TMSnode[D]) => s"${n.datum.toString()}",
  val debugging: Boolean = false,
  val enqueueProcedure: Option[(Rule) => Unit] = None
) {
  // (defstruct (atms (:PRINT-FUNCTION print-atms))
  //   (title nil)
  //   (node-counter 0)              ; unique namer for nodes.
  //   (just-counter 0)              ; unique namer for justifications.
  //   (env-counter 0)               ; Unique id for environments.
  //   (nodes nil)                   ; List of all atms nodes.
  //   (justs nil)                   ; List of all justifications.
  //   (contradictions nil)          ; List of contradiction nodes.
  //   (assumptions nil)             ; List of all atms assumptions.
  //   (debugging nil)               ; Trace grungy details.
  //   (nogood-table nil)
  //   (contra-node nil)             ; A dummy contradiction node.
  //   (env-table nil)
  //   (empty-env nil)               ; Empty environment.
  //   (node-string nil)
  //   (enqueue-procedure nil))

  // (defun create-atms (title &key (node-string 'default-node-string)
  //                           (debugging NIL)
  //                           (enqueue-procedure NIL))
  //   (let ((atms (make-atms :TITLE title
  //                     :NODE-STRING node-string
  //                     :DEBUGGING debugging
  //                     :ENQUEUE-PROCEDURE enqueue-procedure)))
  //     (setf (atms-contra-node atms)
  //      (tms-create-node atms "The contradiction"
  //                            :CONTRADICTORYP t))
  //     (setf (atms-empty-env atms) (create-env atms nil))
  //     atms))

  /** Unique names for nodes. */
  private var nodeCounter: Int = 0

  /** Unique namer for justifications. */
  var justCounter: Int = 0

  /** Unique id for environments. */
  var envCounter = 0

  /** List of all atms nodes. */
  val nodes: HashSet[TMSnode[D]] = new HashSet[TMSnode[D]]

  /** List of all justifications. */
  var justs = {}

  /** List of contradiction nodes. */
  var contradictions: HashSet[TMSnode[D]] = new HashSet[TMSnode[D]]

  /** List of all atms assumptions. */
  var assumptions: HashSet[TMSnode[D]] = new HashSet[TMSnode[D]]

  /** A dummy contradiction node. */
  val contraNode: TMSnode[D] =
    new TMSnode(this, Contra, isContradictory = true)

  /** Empty environment. */
  var emptyEnv = new Env(this, Nil)

  var nogoodTable = {}

  var envTable: EnvTable[D] = new EnvTable[D]

  override def toString(): String = s"<ATMS: $title>"
  // (defun print-atms (atms stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "#<ATMS: ~A>" (atms-title atms)))

  def nextNodeIndex: Int = {
    val result = nodeCounter
    nodeCounter = nodeCounter + 1
    result
  }

  def assumeNode(node: TMSnode[D]): Unit = if !node.isAssumption then {
    dbg(this, s"Converting $node into an assumption")
    node.isAssumption = true
    assumptions += node
    update(ListBuffer(Some(createEnv(List(node)))), node, AssumeNode.JUST)

    // (defun assume-node (node &aux atms)
    //   (unless (tms-node-assumption? node)
    //     (setq atms (tms-node-atms node))
    //     (debugging atms  "~%Converting ~A into an assumption" node)
    //     (setf (tms-node-assumption? node) t)
    //     (push node (atms-assumptions atms))
    //     (update (list (create-env atms (list node)))
    //             node
    //             'ASSUME-NODE)))
  }

  def update(
    newEnvs: EnvList[D],
    consequence: TMSnode[D],
    just: Justification[D]):
      Unit =
    if consequence.isContradictory then {
      for (envOpt <- newEnvs) do envOpt match {
        case Some(env) => newNogood(env, just)
        case None => { }
      }
    } else {
      consequence.updateLabel(newEnvs)
      if !newEnvs.isEmpty then {
        enqueueProcedure.map((enqueuef) => {
          for (rule <- consequence.rules) do enqueuef(rule)
          consequence.rules = List.empty
        })
        for (supportedJust <- consequence.consequences)
          do propagate(supportedJust, consequence, newEnvs)

        for (i <- 0 until newEnvs.length) {
          newEnvs(i) match {
            case Some(env) =>
              if !consequence.label.contains(env) then newEnvs(i) = None
            case None => { }
          }
        }
      }

      // (defun update (new-envs consequence just &aux  enqueuef)
      //   (when (tms-node-isContradictory consequence)
      //     (dolist (env new-envs) (new-nogood atms env just))
      //     (return-from update nil))
      //   (setq new-envs (update-label consequence new-envs))  ;  --> on TMSnode
      //   (unless new-envs (return-from update nil))
      //   (when (setq enqueuef (atms-enqueueProcedure atms)) ; is val field
      //     (dolist (rule (tms-node-rules consequence))
      //       (funcall enqueuef rule))
      //     (setf (tms-node-rules consequence) nil))
      //   (dolist (supported-just (tms-node-consequences consequence))
      //     (propagate supported-just consequence new-envs)
      //   (do ((new-envs new-envs (cdr new-envs)))
      //       ((null new-envs))
      //     (unless (member (car new-envs) (tms-node-label consequence))
      //       (rplaca new-envs nil)))
      //   (setq new-envs (delete nil new-envs :TEST #'eq))
      //   (unless new-envs (return-from update nil))))
  }

  /** Label updating. */
  def propagate(just: TMSnode[D], antecedent: TMSnode[D], envs: EnvList[D]):
      Unit = {
    // val newEnvs = weave(antecedent, envs, just.antecedents)
    ???
    // (defun propagate (just antecedent envs &aux new-envs)
    //   (if (setq new-envs (weave antecedent envs (just-antecedents just)))
    //       (update new-envs (just-consequence just) just)))
  }

  def createEnv(assumptions: List[TMSnode[D]]): Env[D] = {
    envCounter = 1 + envCounter
    val e = new Env[D](assumptions, envCounter, assumptions.length)
    envTable.insert(e)
    setEnvContradictory(e)
    e
  }

  def newNogood(cenv: Env[D], just: Justification[D]): Unit = {
    ???
    // (defun new-nogood (atms cenv just &aux count)
    //   (debugging atms (format nil "~%  ~A new minimal nogood." cenv))
    //   (setf (env-nogood? cenv) just)
    //   (remove-env-from-labels cenv atms)
    //   (setf (atms-nogoodTable atms)
    //         (insert-in-table (atms-nogoodTable atms) cenv)) ;  mutator method on EnvTable
    //   (setq count (env-count cenv))
    //   (dolist (entry (atms-nogoodTable atms))
    //     (when (> (car entry) count)
    //       (dolist (old (cdr entry))
    //         (if (subset-env? cenv old)
    //             (setf (cdr entry) (delete old (cdr entry) :COUNT 1))))))
    //   (dolist (entry (atms-envTable atms))
    //     (when (> (car entry) count)
    //       (dolist (old (cdr entry))
    //         (when (and (not (env-nogood? old))
    //                    (subset-env? cenv old))
    //           (setf (env-nogood? old) cenv)
    //           (remove-env-from-labels old atms))))))
  }

  def setEnvContradictory(env: Env[D]): Boolean = {
    ???

    // (defun set-env-contradictory (atms env &aux count)
    //   (cond ((env-nogood? env) t)
    //         (t (setq count (env-count env))
    //            (dolist (entry (atms-nogoodTable atms))
    //              (cond ((> (car entry) count)
    //                     (return nil))
    //                    (t (dolist (cenv (cdr entry))
    //                         (when (subset-env? cenv env)
    //                           (setf (env-nogood? env)
    //                                 cenv)
    //                           (return t)))))))))
  }

  def makeContradiction(node: TMSnode[D]): Unit = ???
  // (defun make-contradiction
  //        (node &aux (atms (tms-node-atms node)) nogood)
  //   (unless (tms-node-isContradictory node)
  //     (setf (tms-node-isContradictory node) t)
  //     (push node (atms-contradictions atms))
  //     (do nil (nil)
  //       (if (setq nogood (car (tms-node-label node)))
  //           (new-nogood atms nogood 'MAKE-CONTRADICTION)
  //           (return nil)))))

  def justifyNode(
    informant: Any, consequence: TMSnode[D], antecedents: List[TMSnode[D]]):
      Just[D] = ???
  // (defun justify-node (informant consequence antecedents &aux just atms)
  //   (setq atms (tms-node-atms consequence)
  //         just (make-just :INDEX (incf (atms-justCounter atms))
  //                         :INFORMANT informant
  //                         :CONSEQUENCE consequence
  //                         :ANTECEDENTS antecedents))
  //   (push just (tms-node-justs consequence))
  //   (dolist (node antecedents) (push just (tms-node-consequences node)))
  //   (push just (atms-justs atms))
  //   (debugging atms
  //              "~%Justifying ~A in terms of ~A on ~A"
  //              consequence
  //              informant
  //              (mapcar #'nodeString antecedents))
  //   (propagate just nil (list (atms-emptyEnv atms)))
  //   just)

  def nogoodNodes(informant: Any, nodes: List[TMSnode[D]]): Just[D] = ???
  // (defun nogood-nodes (informant nodes)
  //   (justify-node informant
  //                 (atms-contraNode (tms-node-atms (car nodes)))
  //                 nodes))

  def weave(
    antecedent: TMSnode[D], envs: EnvList[D], antecedents: List[TMSnode[D]]):
      EnvList[D] = ???
  // (defun weave (antecedent envs antecedents &aux new-envs new-env)
  //   (setq envs (copy-list envs))
  //   (dolist (node antecedents)
  //     (unless (eq node antecedent)
  //       (setq new-envs nil)
  //       (dolist (env envs)
  //         (if env
  //             (dolist (node-env (tms-node-label node))
  //               (setq new-env (union-env env node-env))
  //               (unless (env-nogood? new-env)
  //                 (do ((nnew-envs new-envs (cdr nnew-envs)))
  //                     ((null nnew-envs) (push new-env new-envs))
  //                   (when (car nnew-envs)
  //                     (case (compare-env new-env (car nnew-envs))
  //                       ((:EQ :S21) (return nil))
  //                       (:S12 (rplaca nnew-envs nil)))))))))
  //       (setq envs (delete nil new-envs :TEST #'eq))
  //       (unless envs (return-from weave nil))))
  //   envs)

  def isInAntecedent(nodes: List[TMSnode[D]]): Boolean = ???
  // (defun in-antecedent? (nodes)
  //   (or (null nodes)
  //       (weave? (atms-emptyEnv (tms-node-atms (car nodes))) nodes)))
  //

  def findOrMakeEnv(assumptions: List[TMSnode[D]]): Env[D] = ???
  // (defun find-or-make-env (assumptions atms)
  //   (unless assumptions
  //     (return-from find-or-make-env (atms-emptyEnv atms)))
  //   ;; Presumes the list of assumptions is ordered properly
  //   (or (lookup-env assumptions)
  //       (create-env atms assumptions)))


  // ;;; Env tables.

  def lookupEnv(assumes: List[TMSnode[D]]): Env[D] = ???
  // (defun lookup-env (assumes)
  //   (dolist (env (cdr (assoc (length assumes)
  //                            (atms-envTable (tms-node-atms (car assumes)))
  //                            :TEST #'=))
  //                nil)
  //     (if (equal (env-assumptions env) assumes)
  //         (return env))))

  // ;;; Processing nogoods

  def removeEnvFromLabels(env: Env[D]): Unit = ???
  // (defun remove-env-from-labels (env atms &aux enqueuef)
  //   (when (setq enqueuef (atms-enqueueProcedure atms))
  //     (dolist (rule (env-rules env))
  //       (funcall enqueuef rule))
  //     (setf (env-rules env) nil))
  //   (dolist (node (env-nodes env))
  //     (setf (tms-node-label node)
  //           (delete env (tms-node-label node) :COUNT 1))))

  def interpretations(choiceSets: Any): List[Any] = ???
  // ;;; Interpretation construction
  //
  // (proclaim '(special *solutions*))
  //
  // (defun interpretations (atms choice-sets
  //                         &optional defaults &aux solutions)
  //   (if (atms-debugging atms)
  //    (format *trace-output*
  //            "~% Constructing interpretations depth-first..."))
  //   (let ((*solutions* nil)
  //         (choice-sets
  //           (mapcar #'(lambda (alt-set)
  //                       (mapcan #'(lambda (alt)
  //                                   (copy-list (tms-node-label alt)))
  //                               alt-set))
  //                   choice-sets)))
  //     (dolist (choice (car choice-sets))
  //       (get-depth-solutions1 choice (cdr choice-sets)))
  //     (setq *solutions* (delete nil *solutions* :TEST #'eq))
  //     (unless *solutions*
  //       (if choice-sets (return-from interpretations nil)
  //                       (setq *solutions* (list (atms-emptyEnv atms)))))
  //     (when defaults
  //       (setq solutions *solutions* *solutions* nil)
  //       (dolist (solution solutions)
  //         (extend-via-defaults solution defaults defaults)))
  //     (delete nil *solutions* :TEST #'eq)))

  def getDepthSolutions1(solution: Any, choiceSets: Any): Unit = ???
  // (defun get-depth-solutions1 (solution choice-sets
  //                                       &aux new-solution)
  //   (cond ((null choice-sets)
  //          (unless (do ((old-solutions *solutions* (cdr old-solutions)))
  //                      ((null old-solutions))
  //                    (when (car old-solutions)
  //                      (case (compare-env (car old-solutions) solution)
  //                        ((:EQ :S12) (return t))
  //                        (:S21 (rplaca old-solutions nil)))))
  //            (push solution *solutions*)))
  //         ((env-nogood? solution)) ;something died.
  //         (t (dolist (choice (car choice-sets))
  //              (setq new-solution (union-env solution choice))
  //              (unless (env-nogood? new-solution)
  //                (get-depth-solutions1 new-solution
  //                                      (cdr choice-sets)))))))
  //

  // (defun extend-via-defaults (solution remaining original)
  //   (do ((new-solution)
  //        (defaults remaining (cdr defaults)))
  //       ((null defaults)
  //        (or (member solution *solutions* :TEST #'eq)
  //            (dolist (default original)
  //              (or (member default (env-assumptions solution)
  //                          :TEST #'eq)
  //                  (env-nogood? (cons-env default solution))
  //                  (return t)))
  //            (push solution *solutions*)))
  //     (setq new-solution (cons-env (car defaults) solution))
  //     (unless (env-nogood? new-solution)
  //       (extend-via-defaults new-solution (cdr defaults) original))))

  def whyNodes: Unit = ???
  // (defun why-nodes (atms &optional (stream t))
  //   (dolist (n (reverse (atms-nodes atms))) (why-node n stream)))

  def e(n: Int): Env[D] = ???
  // (defun e (atms n)
  //   (dolist (bucket (atms-envTable atms))
  //     (dolist (env (cdr bucket))
  //     (if (= (env-index env) n) (return-from e env)))))

  // ;;; Printing global data

  def printNogoods: Unit = ???
  // (defun print-nogoods (atms &optional (stream t))
  //   (print-envTable (atms-nogoodTable atms) stream))

  def printEnvs: Unit = ???
  // (defun print-envs (atms &optional (stream t))
  //   (print-envTable (atms-envTable atms) stream))

  def printEnvTable(table: Any): Unit = ???
  // (defun print-envTable (table stream)
  //   (dolist (bucket table)
  //     (dolist (env (cdr bucket))
  //       (print-env env stream))))

  def printAtmsStatistics: Unit = ???
  // (defun print-atms-statistics (atms)
  //   (print-table "~% For env table:" (atms-envTable atms))
  //   (print-table "~% For nogood table:" (atms-nogoodTable atms)))

  def printTable(msg: String, table: Any): Unit = ???
  // (defun print-table (msg table)
  //   (format t msg)
  //   (dolist (entry table)
  //     (format t "~%   Length ~D, ~D" (car entry)
  //             (length (cdr entry)))))

}

extension [T](xs: List[T]) {
  def orderedInsert(item: T, test: (T, T) => Boolean): List[T] = xs match {
    // (defun ordered-insert (item list test)
    //   (cond ((null list) (list item))
    //  ((funcall test item (car list)) (cons item list))
    //  ((eq item (car list)) list)
    //  (t (cons (car list) (ordered-insert item (cdr list) test)))))

    case x :: xs => { if test(item, x) then item :: xs
      else if item.equals(x) then xs
      else x :: xs.orderedInsert(item, test)
    }
    case Nil => List(item)
  }
}

