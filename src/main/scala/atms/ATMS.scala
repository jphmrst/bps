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
import scala.collection.mutable.{HashSet, HashMap}

// Definitions

class Contra private ()
object Contra extends Contra {
  override def toString(): String = "contradiction"
}

type Rule = Unit

class EnvTable[D]
    extends HashMap[Int, List[Env[D]]] {
  def insert(env: Env[D]): Unit = {
    ???
/*
(defun insert-in-table (table env &aux count entry)
  (setq count (env-count env)
        entry (assoc count table :TEST #'=))
  (cond (entry (setf (cdr entry) (cons env (cdr entry))) table)
        (t (ordered-insert
             (list count env) table
             #'(lambda (entry1 entry2)
                 (< (car entry1) (car entry2)))))))
 */
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

  def nextNodeIndex: Int = {
    val result = nodeCounter
    nodeCounter = nodeCounter + 1
    result
  }

  def assumeNode(node: TMSnode[D]): Unit = if !node.isAssumption then {
    if debugging then println(s"Converting $node into an assumption")
    node.isAssumption = true
    assumptions += node
    update(List(createEnv(List(node))), node, AssumeNode.JUST)
  }

  def update(
    newEnvs: List[Env[D]], consequence: TMSnode[D], just: Justification):
      Unit = {
    ???

    /*
(defun update (new-envs consequence just &aux  enqueuef)
  (when (tms-node-isContradictory consequence)
    (dolist (env new-envs) (new-nogood atms env just))
    (return-from update nil))
  (setq new-envs (update-label consequence new-envs))  ;  --> on TMSnode
  (unless new-envs (return-from update nil))
  (when (setq enqueuef (atms-enqueueProcedure atms)) ; is val field
    (dolist (rule (tms-node-rules consequence))
      (funcall enqueuef rule))
    (setf (tms-node-rules consequence) nil))
  (dolist (supported-just (tms-node-consequences consequence))
    (propagate supported-just consequence new-envs)
  (do ((new-envs new-envs (cdr new-envs)))
      ((null new-envs))
    (unless (member (car new-envs) (tms-node-label consequence))
      (rplaca new-envs nil)))
  (setq new-envs (delete nil new-envs :TEST #'eq))
  (unless new-envs (return-from update nil))))
     */
  }

  def createEnv(assumptions: List[TMSnode[D]]): Env[D] = {
    envCounter = 1 + envCounter
    val e = new Env[D](assumptions, envCounter, assumptions.length)
    envTable.insert(e)
    setEnvContradictory(e)
    e
  }

  def newNogood(cenv: Env[D], just: TMSnode[D]): Unit = {
    ???
  /*
(defun new-nogood (atms cenv just &aux count)
  (debugging atms (format nil "~%  ~A new minimal nogood." cenv))
  (setf (env-nogood? cenv) just)
  (remove-env-from-labels cenv atms)
  (setf (atms-nogoodTable atms)
        (insert-in-table (atms-nogoodTable atms) cenv)) ;  mutator method on EnvTable
  (setq count (env-count cenv))
  (dolist (entry (atms-nogoodTable atms))
    (when (> (car entry) count)
      (dolist (old (cdr entry))
        (if (subset-env? cenv old)
            (setf (cdr entry) (delete old (cdr entry) :COUNT 1))))))
  (dolist (entry (atms-envTable atms))
    (when (> (car entry) count)
      (dolist (old (cdr entry))
        (when (and (not (env-nogood? old))
                   (subset-env? cenv old))
          (setf (env-nogood? old) cenv)
          (remove-env-from-labels old atms))))))
   */
  }

  def setEnvContradictory(env: Env[D]): Boolean = {
    ???

  /*
(defun set-env-contradictory (atms env &aux count)
  (cond ((env-nogood? env) t)
        (t (setq count (env-count env))
           (dolist (entry (atms-nogoodTable atms))
             (cond ((> (car entry) count)
                    (return nil))
                   (t (dolist (cenv (cdr entry))
                        (when (subset-env? cenv env)
                          (setf (env-nogood? env)
                                cenv)
                          (return t)))))))))
   */
  }

  /*
   */

  /*
   */

  /*
   */

  /*
   */
}

extension [T](xs: List[T]) {
  def orderedInsert(item: T, test: (T, T) => Boolean): List[T] = xs match {
    case x :: xs => { if test(item, x) then item :: xs
      else if item.equals(x) then xs
      else x :: xs.orderedInsert(item, test)
    }
    case Nil => List(item)
  }
}


/*

(defmacro debugging (atms msg &optional node &rest args)
  `(when (atms-debugging ,atms)
     (format *trace-output*
             ,msg (if ,node (nodeString ,node)) ,@args)))

(defmacro ordered-push (item list test)
  `(setq ,list (ordered-insert ,item ,list ,test)))

;;; Basic inference engine interface.

// (defun change-atms (atms &key nodeString
//                               enqueueProcedure debugging)
//   (if nodeString (setf (atms-nodeString atms) nodeString))
//   (if debugging (setf (atms-debugging atms) debugging))
//   (if enqueueProcedure
//       (setf (atms-enqueueProcedure atms) enqueueProcedure)))
//
// (defun true-node? (node)
//   (eq (car (tms-node-label node))
//       (atms-emptyEnv (tms-node-atms node))))


;;; Methods on other classes

;; (defun update-label (node new-envs &aux envs) ==> On TMSnode


(defun make-contradiction
       (node &aux (atms (tms-node-atms node)) nogood)
  (unless (tms-node-isContradictory node)
    (setf (tms-node-isContradictory node) t)
    (push node (atms-contradictions atms))
    (do nil (nil)
      (if (setq nogood (car (tms-node-label node)))
          (new-nogood atms nogood 'MAKE-CONTRADICTION)
          (return nil)))))

(defun justify-node (informant consequence antecedents &aux just atms)
  (setq atms (tms-node-atms consequence)
        just (make-just :INDEX (incf (atms-justCounter atms))
                        :INFORMANT informant
                        :CONSEQUENCE consequence
                        :ANTECEDENTS antecedents))
  (push just (tms-node-justs consequence))
  (dolist (node antecedents) (push just (tms-node-consequences node)))
  (push just (atms-justs atms))
  (debugging atms
             "~%Justifying ~A in terms of ~A on ~A"
             consequence
             informant
             (mapcar #'nodeString antecedents))
  (propagate just nil (list (atms-emptyEnv atms)))
  just)

(defun nogood-nodes (informant nodes)
  (justify-node informant
                (atms-contraNode (tms-node-atms (car nodes)))
                nodes))

;;; Label updating

(defun propagate (just antecedent envs &aux new-envs)
  (if (setq new-envs (weave antecedent envs (just-antecedents just)))
      (update new-envs (just-consequence just) just)))




(defun weave (antecedent envs antecedents &aux new-envs new-env)
  (setq envs (copy-list envs))
  (dolist (node antecedents)
    (unless (eq node antecedent)
      (setq new-envs nil)
      (dolist (env envs)
        (if env
            (dolist (node-env (tms-node-label node))
              (setq new-env (union-env env node-env))
              (unless (env-nogood? new-env)
                (do ((nnew-envs new-envs (cdr nnew-envs)))
                    ((null nnew-envs) (push new-env new-envs))
                  (when (car nnew-envs)
                    (case (compare-env new-env (car nnew-envs))
                      ((:EQ :S21) (return nil))
                      (:S12 (rplaca nnew-envs nil)))))))))
      (setq envs (delete nil new-envs :TEST #'eq))
      (unless envs (return-from weave nil))))
  envs)

(defun in-antecedent? (nodes)
  (or (null nodes)
      (weave? (atms-emptyEnv (tms-node-atms (car nodes))) nodes)))

(defun weave? (env nodes &aux new-env)
  (cond ((null nodes) t)
        (t (dolist (e (tms-node-label (car nodes)))
             (setq new-env (union-env e env))
             (unless (env-nogood? new-env)
               (if (weave? new-env (cdr nodes))
                   (return T)))))))

(defun supporting-antecedent? (nodes env)
  (dolist (node nodes t) (unless (isInNode node env) (return nil))))


(defun remove-node (node &aux atms)
  (if (tms-node-consequences node)
      (error "Can't remove node with consequences"))
  (setq atms (tms-node-atms node))
  (setf (atms-nodes atms)
        (delete node (atms-nodes atms) :test #'eq :count 1))
  (dolist (just (tms-node-justs node))
    (dolist (ant (just-antecedents just))
      (setf (tms-node-consequences ant)
            (delete just (tms-node-consequences ant)
                    :test #'eq :count 1))))
  (dolist (env (tms-node-label node))
    (setf (env-nodes env)
          (delete node (env-nodes env) :test #'eq :count 1))))

;;; Creating and extending environments.


(defun union-env (e1 e2)
  (when (> (env-count e1)
           (env-count e2))
    (psetq e1 e2 e2 e1))
  (dolist (assume (env-assumptions e1))
    (setq e2 (cons-env assume e2))
    (if (env-nogood? e2) (return nil)))
  e2)

(defun cons-env (assumption env &aux nassumes)
  (setq nassumes (ordered-insert assumption
                                 (env-assumptions env)
                                 #'assumption-order))
  (or (lookup-env nassumes)
      (create-env (tms-node-atms assumption) nassumes)))

(defun find-or-make-env (assumptions atms)
  (unless assumptions
    (return-from find-or-make-env (atms-emptyEnv atms)))
  ;; Presumes the list of assumptions is ordered properly
  (or (lookup-env assumptions)
      (create-env atms assumptions)))

;;; Env tables.


(defun lookup-env (assumes)
  (dolist (env (cdr (assoc (length assumes)
                           (atms-envTable (tms-node-atms (car assumes)))
                           :TEST #'=))
               nil)
    (if (equal (env-assumptions env) assumes)
        (return env))))

(defun subset-env? (e1 e2)
  (cond ((eq e1 e2) t)
        ((> (env-count e1)
            (env-count e2)) nil)
        ((subsetp (env-assumptions e1)
                  (env-assumptions e2)))))

(defun compare-env (e1 e2)
  (cond ((eq e1 e2) :EQ)
        ((< (env-count e1) (env-count e2))
         (if (subsetp (env-assumptions e1)
                      (env-assumptions e2))
             :S12))
        ((subsetp (env-assumptions e2) (env-assumptions e1))
         :S21)))

;;; Processing nogoods


(defun remove-env-from-labels (env atms &aux enqueuef)
  (when (setq enqueuef (atms-enqueueProcedure atms))
    (dolist (rule (env-rules env))
      (funcall enqueuef rule))
    (setf (env-rules env) nil))
  (dolist (node (env-nodes env))
    (setf (tms-node-label node)
          (delete env (tms-node-label node) :COUNT 1))))

;;; Interpretation construction

(proclaim '(special *solutions*))

(defun interpretations (atms choice-sets
                        &optional defaults &aux solutions)
  (if (atms-debugging atms)
   (format *trace-output*
           "~% Constructing interpretations depth-first..."))
  (let ((*solutions* nil)
        (choice-sets
          (mapcar #'(lambda (alt-set)
                      (mapcan #'(lambda (alt)
                                  (copy-list (tms-node-label alt)))
                              alt-set))
                  choice-sets)))
    (dolist (choice (car choice-sets))
      (get-depth-solutions1 choice (cdr choice-sets)))
    (setq *solutions* (delete nil *solutions* :TEST #'eq))
    (unless *solutions*
      (if choice-sets (return-from interpretations nil)
                      (setq *solutions* (list (atms-emptyEnv atms)))))
    (when defaults
      (setq solutions *solutions* *solutions* nil)
      (dolist (solution solutions)
        (extend-via-defaults solution defaults defaults)))
    (delete nil *solutions* :TEST #'eq)))

(defun get-depth-solutions1 (solution choice-sets
                                      &aux new-solution)
  (cond ((null choice-sets)
         (unless (do ((old-solutions *solutions* (cdr old-solutions)))
                     ((null old-solutions))
                   (when (car old-solutions)
                     (case (compare-env (car old-solutions) solution)
                       ((:EQ :S12) (return t))
                       (:S21 (rplaca old-solutions nil)))))
           (push solution *solutions*)))
        ((env-nogood? solution)) ;something died.
        (t (dolist (choice (car choice-sets))
             (setq new-solution (union-env solution choice))
             (unless (env-nogood? new-solution)
               (get-depth-solutions1 new-solution
                                     (cdr choice-sets)))))))


(defun extend-via-defaults (solution remaining original)
  (do ((new-solution)
       (defaults remaining (cdr defaults)))
      ((null defaults)
       (or (member solution *solutions* :TEST #'eq)
           (dolist (default original)
             (or (member default (env-assumptions solution)
                         :TEST #'eq)
                 (env-nogood? (cons-env default solution))
                 (return t)))
           (push solution *solutions*)))
    (setq new-solution (cons-env (car defaults) solution))
    (unless (env-nogood? new-solution)
      (extend-via-defaults new-solution (cdr defaults) original))))

;;; Generating explanations
;;; This returns a list of justifications which form a DAG for the
;;; derivation. This is quite complicated because this is really a
;;; simple consequent JTMS.

(defun explain-node (node env) (explain-node-1 env node nil nil))

(defun explain-node-1 (env node queued-nodes explanation)
  (cond ((member node queued-nodes) nil)
        ((and (tms-node-isAssumption node)
              (member node (env-assumptions env)))
         (cons (cons 'ASSUME node) explanation))
        ((dolist (just explanation)
           (if (if (listp just)
                   (eq (cdr just) node) (eq (just-consequence just) node))
               (return explanation))))
        (t (setq queued-nodes (cons node queued-nodes))
           (dolist (just (tms-node-justs node))
             (unless (dolist (a (just-antecedents just))
                       (unless (isInNode a env) (return t)))
              (let ((new-explanation explanation))
                (dolist (a (just-antecedents just)
                           (return-from explain-node-1 (cons just new-explanation)))
                  (setq new-explanation
                        (explain-node-1 env a queued-nodes new-explanation))
                  (unless new-explanation (return nil)))))))))

;;; Printing
(defun why-node (node &optional (stream t) (prefix ""))
  (format stream "~%<~A~A,{" prefix (tms-node-datum node))
  (dolist (e (tms-node-label node))
    (env-string e stream))
  (format stream "}>"))

(defun why-nodes (atms &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (why-node n stream)))

(defun node-justifications (node &optional (stream t))
  (format t "~% For ~A:" (nodeString node))
  (dolist (j (tms-node-justs node))
    (print-justification j stream)))

(defun print-justification (j &optional (stream t))
  (format stream "~%  ~A, " (just-informant j))
  (dolist (a (just-antecedents j))
    (why-node a stream "     ")))

(defun e (atms n)
  (dolist (bucket (atms-envTable atms))
    (dolist (env (cdr bucket))
    (if (= (env-index env) n) (return-from e env)))))

(defun print-env (e &optional (stream t))
  (format stream "~%~A:~A"
          e (if (env-nogood? e)
                "* " " "))
  (env-string e stream))

(defun env-string (e &optional stream
                     &aux assumptions strings printer)
  (setq assumptions (env-assumptions e))
  (when assumptions
    (setq printer (atms-nodeString (tms-node-atms (car assumptions)))))
  (dolist (a assumptions) (push (funcall printer a) strings))
  (format stream "{~{~A~^,~}}" (sort strings #'string-lessp)))

;;; Printing global data

(defun print-nogoods (atms &optional (stream t))
  (print-envTable (atms-nogoodTable atms) stream))

(defun print-envs (atms &optional (stream t))
  (print-envTable (atms-envTable atms) stream))

(defun print-envTable (table stream)
  (dolist (bucket table)
    (dolist (env (cdr bucket))
      (print-env env stream))))

(defun print-atms-statistics (atms)
  (print-table "~% For env table:" (atms-envTable atms))
  (print-table "~% For nogood table:" (atms-nogoodTable atms)))

(defun print-table (msg table)
  (format t msg)
  (dolist (entry table)
    (format t "~%   Length ~D, ~D" (car entry)
            (length (cdr entry)))))

*/
