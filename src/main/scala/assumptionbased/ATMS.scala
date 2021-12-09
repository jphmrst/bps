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

type ChoiceSets[D, I] = Unit

/** Implementation of assmuption-based truth maintenance systems.
  *
  * @param title Name of this TMS, for output.
  */
class ATMS[D, I](
  val title: String,
  var nodeString: (Node[D, I]) => String =
    (n: Node[D, I]) => s"${n.datum.toString()}",
  var debugging: Boolean = false,
  var enqueueProcedure: Option[(Rule[I]) => Unit] = None
) {

  /** Unique namer for nodes. */
  var nodeCounter: Int = 0
  /** Increment the node counter and return its value. */
  def incrNodeCounter: Int = {
    val result = nodeCounter
    nodeCounter = nodeCounter + 1
    result
  }

  /** Unique namer for justifications. */
  var justCounter: Int = 0
  /** Increment the justifications counter and return its value. */
  def incrJustCounter: Int = {
    val result = justCounter
    justCounter = justCounter + 1
    result
  }

  /** Unique namer for environments. */
  var envCounter: Int = 0
  /** Increment the environments counter and return its value. */
  def incrEnvCounter: Int = {
    val result = envCounter
    envCounter = envCounter + 1
    result
  }

  /** List of all tms nodes. */
  var nodes: ListBuffer[Node[D, I]] = ListBuffer.empty

  /** List of all justifications. */
  var justs: ListBuffer[Just[D, I]] = ListBuffer.empty

  /** List of contradiction nodes. */
  var contradictions: ListBuffer[Node[D, I]] = ListBuffer.empty

  /** List of assumption nodes. */
  var assumptions: ListBuffer[Node[D, I]] = ListBuffer.empty

  /** Dummy contradiction node. */
  val contraNode: Node[D, I] =
    createNode("The contradiction", isContradictory = true)

  /** Empty environment. */
  val emptyEnv: Env[D, I] = createEnv(???)

  // ; From atms.lisp
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
  //
  // (defun create-atms (title &key (node-string 'default-node-string)
  //                                (debugging NIL)
  //                                (enqueue-procedure NIL))
  //   (let ((atms (make-atms :TITLE title
  //                          :NODE-STRING node-string
  //                          :DEBUGGING debugging
  //                          :ENQUEUE-PROCEDURE enqueue-procedure)))
  //     (setf (atms-contra-node atms)
  //           (tms-create-node atms "The contradiction"
  //                            :CONTRADICTORYP t))
  //     (setf (atms-empty-env atms) (create-env atms nil))
  //     atms))
  //
  // (defun change-atms (atms &key node-string
  //                               enqueue-procedure debugging)
  //   (if node-string (setf (atms-node-string atms) node-string))
  //   (if debugging (setf (atms-debugging atms) debugging))
  //   (if enqueue-procedure
  //       (setf (atms-enqueue-procedure atms) enqueue-procedure)))

  override def toString: String = s"<ATMS $title>"
  def printAtms: Unit = println(toString)
  // ; From atms.lisp
  // (defun print-atms (atms stream ignore)
  //   (declare (ignore ignore))
  //   (format stream "#<ATMS: ~A>" (atms-title atms)))

  inline def dbg(msg: String): Unit = if debugging then println(msg)
  // ; From atms.lisp
  // (defmacro debugging (atms msg &optional node &rest args)
  //   `(when (atms-debugging ,atms)
  //      (format *trace-output*
  //              ,msg (if ,node (node-string ,node)) ,@args)))

  def createNode(
    datum: D | String,
    isAssumption: Boolean = false, isContradictory: Boolean = false):
      Node[D, I] = ???
  // ; From atms.lisp
  // (defun tms-create-node (atms datum &key assumptionp contradictoryp
  //                                    &aux node)
  //   (setq node (make-tms-node :INDEX (incf (atms-node-counter atms))
  //                             :DATUM datum
  //                             :ASSUMPTION? assumptionp
  //                             :CONTRADICTORY? contradictoryp
  //                             :ATMS atms))
  //   (push node (atms-nodes atms))
  //   (if contradictoryp (push node (atms-contradictions atms)))
  //   (when assumptionp
  //     (push node (atms-assumptions atms))
  //     (push (create-env atms (list node)) (tms-node-label node)))
  //   node)

  def createEnv(assumptions: ListBuffer[Node[D, I]]): Env[D, I] = ???
  // ; From atms.lisp
  // (defun create-env (atms assumptions &aux e)
  //   (setq e (make-env :INDEX (incf (atms-env-counter atms))
  //                     :ASSUMPTIONS assumptions
  //                     :COUNT (length assumptions)))
  //   (setf (atms-env-table atms)
  //         (insert-in-table (atms-env-table atms) e))
  //   (set-env-contradictory atms e)
  //   e)

  def assumeNode(node: Node[D, I]): Unit = ???
  // ; From atms.lisp
  // (defun assume-node (node &aux atms)
  //   (unless (tms-node-assumption? node)
  //     (setq atms (tms-node-atms node))
  //     (debugging atms  "~%Converting ~A into an assumption" node)
  //     (setf (tms-node-assumption? node) t)
  //     (push node (atms-assumptions atms))
  //     (update (list (create-env atms (list node)))
  //             node
  //             'ASSUME-NODE)))

  def makeContradiction(node: Node[D, I]): Unit = ???
  // ; From atms.lisp
  // (defun make-contradiction
  //        (node &aux (atms (tms-node-atms node)) nogood)
  //   (unless (tms-node-contradictory? node)
  //     (setf (tms-node-contradictory? node) t)
  //     (push node (atms-contradictions atms))
  //     (do nil (nil)
  //       (if (setq nogood (car (tms-node-label node)))
  //           (new-nogood atms nogood 'MAKE-CONTRADICTION)
  //           (return nil)))))

  def justifyNode(
    informant: I, consequence: Node[D, I], antecedents: ListBuffer[Node[D, I]]):
      Unit = ???
  // ; From atms.lisp
  // (defun justify-node (informant consequence antecedents &aux just atms)
  //   (setq atms (tms-node-atms consequence)
  //         just (make-just :INDEX (incf (atms-just-counter atms))
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
  //              (mapcar #'node-string antecedents))
  //   (propagate just nil (list (atms-empty-env atms)))
  //   just)

  def nogoodNodes(informant: I, nodes: ListBuffer[Node[D, I]]): Unit = ???
  // ; From atms.lisp
  // (defun nogood-nodes (informant nodes)
  //   (justify-node informant
  //                 (atms-contra-node (tms-node-atms (car nodes)))
  //                 nodes))

  def propagate(
    just: Just[D, I], antecedent: Node[D, I], env: Env[D, I]):
      Unit = ???
  // ; From atms.lisp
  // (defun propagate (just antecedent envs &aux new-envs)
  //   (if (setq new-envs (weave antecedent envs (just-antecedents just)))
  //       (update new-envs (just-consequence just) just)))

  def update(
    newEnvs: Iterable[Env[D, I]], consequence: Node[D, I], just: Just[D, I]):
      Unit = ???
  // ; From atms.lisp
  // (defun update (new-envs consequence just &aux atms enqueuef)
  //   (setq atms (tms-node-atms consequence))
  //   (when (tms-node-contradictory? consequence)
  //     (dolist (env new-envs) (new-nogood atms env just))
  //     (return-from update nil))
  //   (setq new-envs (update-label consequence new-envs))
  //   (unless new-envs (return-from update nil))
  //   (when (setq enqueuef (atms-enqueue-procedure atms))
  //     (dolist (rule (tms-node-rules consequence))
  //       (funcall enqueuef rule))
  //     (setf (tms-node-rules consequence) nil))
  //   (dolist (supported-just (tms-node-consequences consequence))
  //     (propagate supported-just consequence new-envs)
  //     (do ((new-envs new-envs (cdr new-envs)))
  //         ((null new-envs))
  //       (unless (member (car new-envs) (tms-node-label consequence))
  //         (rplaca new-envs nil)))
  //     (setq new-envs (delete nil new-envs :TEST #'eq))
  //     (unless new-envs (return-from update nil))))

  def weave(
    antecedent: Node[D, I],
    envs: Iterable[Env[D, I]],
    antecedents: ListBuffer[Node[D, I]]):
      Iterable[Env[D, I]] = ???
  // ; From atms.lisp
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

  def isInAntecedent(nodes: Iterable[Node[D, I]]): Boolean = ???
  // ; From atms.lisp
  // (defun in-antecedent? (nodes)
  //   (or (null nodes)
  //       (weave? (atms-empty-env (tms-node-atms (car nodes))) nodes)))

  def removeNode(node: Node[D, I]): Unit = ???
  // ; From atms.lisp
  // (defun remove-node (node &aux atms)
  //   (if (tms-node-consequences node)
  //       (error "Can't remove node with consequences"))
  //   (setq atms (tms-node-atms node))
  //   (setf (atms-nodes atms)
  //         (delete node (atms-nodes atms) :test #'eq :count 1))
  //   (dolist (just (tms-node-justs node))
  //     (dolist (ant (just-antecedents just))
  //       (setf (tms-node-consequences ant)
  //             (delete just (tms-node-consequences ant)
  //                     :test #'eq :count 1))))
  //   (dolist (env (tms-node-label node))
  //     (setf (env-nodes env)
  //           (delete node (env-nodes env) :test #'eq :count 1))))

  def lookupEnv(assumes: Iterable[Node[D, I]]): Env[D, I] = ???
  // ; From atms.lisp
  // (defun lookup-env (assumes)
  //   (dolist (env (cdr (assoc (length assumes)
  //                            (atms-env-table (tms-node-atms (car assumes)))
  //                            :TEST #'=))
  //                nil)
  //     (if (equal (env-assumptions env) assumes)
  //         (return env))))

  def newNogood(cenv: Env[D, I], just: Just[D, I]): Unit = ???
  // ; From atms.lisp
  // (defun new-nogood (atms cenv just &aux count)
  //   (debugging atms (format nil "~%  ~A new minimal nogood." cenv))
  //   (setf (env-nogood? cenv) just)
  //   (remove-env-from-labels cenv atms)
  //   (setf (atms-nogood-table atms)
  //         (insert-in-table (atms-nogood-table atms) cenv))
  //   (setq count (env-count cenv))
  //   (dolist (entry (atms-nogood-table atms))
  //     (when (> (car entry) count)
  //       (dolist (old (cdr entry))
  //         (if (subset-env? cenv old)
  //             (setf (cdr entry) (delete old (cdr entry) :COUNT 1))))))
  //   (dolist (entry (atms-env-table atms))
  //     (when (> (car entry) count)
  //       (dolist (old (cdr entry))
  //         (when (and (not (env-nogood? old))
  //                    (subset-env? cenv old))
  //           (setf (env-nogood? old) cenv)
  //           (remove-env-from-labels old atms))))))

  def setEnvContradictory(env: Env[D, I]): Unit = ???
  // ; From atms.lisp
  // (defun set-env-contradictory (atms env &aux count)
  //   (cond ((env-nogood? env) t)
  //         (t (setq count (env-count env))
  //            (dolist (entry (atms-nogood-table atms))
  //              (cond ((> (car entry) count)
  //                     (return nil))
  //                    (t (dolist (cenv (cdr entry))
  //                         (when (subset-env? cenv env)
  //                           (setf (env-nogood? env)
  //                                 cenv)
  //                           (return t)))))))))

  def removeEnvFromLabels(env: Env[D, I]): Unit = ???
  // ; From atms.lisp
  // (defun remove-env-from-labels (env atms &aux enqueuef)
  //   (when (setq enqueuef (atms-enqueue-procedure atms))
  //     (dolist (rule (env-rules env))
  //       (funcall enqueuef rule))
  //     (setf (env-rules env) nil))
  //   (dolist (node (env-nodes env))
  //     (setf (tms-node-label node)
  //           (delete env (tms-node-label node) :COUNT 1))))

  // ; From atms.lisp
  // (proclaim '(special *solutions*))

  def interpretations(choiceSets: ChoiceSets[D, I]): ListBuffer[Env[D, I]] = ???
  // ; From atms.lisp
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
  //                       (setq *solutions* (list (atms-empty-env atms)))))
  //     (when defaults
  //       (setq solutions *solutions* *solutions* nil)
  //       (dolist (solution solutions)
  //         (extend-via-defaults solution defaults defaults)))
  //     (delete nil *solutions* :TEST #'eq)))

  def getDepthSolutions1(
    solution: Env[D, I], choiceSets: ChoiceSets[D, I]):
      ListBuffer[Env[D, I]] = ???
  // ; From atms.lisp
  // (defun get-depth-solutions1 (solution choice-sets
  //                                       &aux new-solution)
  //   (cond
  //     ((null choice-sets)
  //      (unless (do ((old-solutions *solutions* (cdr old-solutions)))
  //                  ((null old-solutions))
  //                (when (car old-solutions)
  //                  (case (compare-env (car old-solutions) solution)
  //                    ((:EQ :S12) (return t))
  //                    (:S21 (rplaca old-solutions nil)))))
  //        (push solution *solutions*)))
  //     ((env-nogood? solution)) ;something died.
  //     (t (dolist (choice (car choice-sets))
  //          (setq new-solution (union-env solution choice))
  //          (unless (env-nogood? new-solution)
  //            (get-depth-solutions1 new-solution
  //                                  (cdr choice-sets)))))))

  def extendViaDefaults(
    solution: Env[D, I],
    remaining: ListBuffer[Node[D, I]],
    original: ListBuffer[Node[D, I]]):
      Unit = ???
  // ; From atms.lisp
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

  def whyNodes: Unit = for (node <- nodes.reverse) do node.whyNode
  // ; From atms.lisp
  // (defun why-nodes (atms &optional (stream t))
  //   (dolist (n (reverse (atms-nodes atms))) (why-node n stream)))

  def e(n: Node[D, I]): Unit = ???
  // ; From atms.lisp
  // (defun e (atms n)
  //   (dolist (bucket (atms-env-table atms))
  //     (dolist (env (cdr bucket))
  //       (if (= (env-index env) n) (return-from e env)))))

  def printNogoods: String = ???
  // ; From ainter.lisp
  // (defun print-nogoods (atms &optional (stream t))
  //   (print-env-table (atms-nogood-table atms) stream))

  def printEnvs: String = ???
  // ; From ainter.lisp
  // (defun print-envs (atms &optional (stream t))
  //   (print-env-table (atms-env-table atms) stream))

  def printAtmsStatistics: Unit = ???
  // ; From ainter.lisp
  // (defun print-atms-statistics (atms)
  //   (print-table "~% For env table:" (atms-env-table atms))
  //   (print-table "~% For nogood table:" (atms-nogood-table atms)))
}

// ; From atms.lisp
// (defun ordered-insert (item list test)
//   (cond ((null list) (list item))
//         ((funcall test item (car list)) (cons item list))
//         ((eq item (car list)) list)
//         (t (cons (car list) (ordered-insert item (cdr list) test)))))

// ; From atms.lisp
// (defmacro ordered-push (item list test)
//   `(setq ,list (ordered-insert ,item ,list ,test)))
