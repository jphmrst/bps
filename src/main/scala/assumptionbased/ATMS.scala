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
import org.maraist.truthmaintenancesystems.utils.Printing.*
import org.maraist.truthmaintenancesystems.assumptionbased.Blurb

// Assumption-based truth maintenance system, translated from F/dK
// version 61 of 7/21/92.

type ChoiceSets[D, I] = ListBuffer[ListBuffer[Node[D, I]]]

/** Implementation of assumption-based truth maintenance systems.
  *
  * **Arguments and `val` members translated from**:
  * <pre>
; From atms.lisp
(defstruct (atms (:PRINT-FUNCTION print-atms))
  (title nil)
  (node-counter 0)              ; unique namer for nodes.
  (just-counter 0)              ; unique namer for justifications.
  (env-counter 0)               ; Unique id for environments.
  (nodes nil)                   ; List of all atms nodes.
  (justs nil)                   ; List of all justifications.
  (contradictions nil)          ; List of contradiction nodes.
  (assumptions nil)             ; List of all atms assumptions.
  (debugging nil)               ; Trace grungy details.
  (nogood-table nil)
  (contra-node nil)             ; A dummy contradiction node.
  (env-table nil)
  (empty-env nil)               ; Empty environment.
  (node-string nil)
  (enqueue-procedure nil))

(defun create-atms (title &key (node-string 'default-node-string)
                               (debugging NIL)
                               (enqueue-procedure NIL))
  (let ((atms (make-atms :TITLE title
                         :NODE-STRING node-string
                         :DEBUGGING debugging
                         :ENQUEUE-PROCEDURE enqueue-procedure)))
    (setf (atms-contra-node atms)
          (tms-create-node atms "The contradiction"
                           :CONTRADICTORYP t))
    (setf (atms-empty-env atms) (create-env atms nil))
    atms))

(defun change-atms (atms &key node-string
                              enqueue-procedure debugging)
  (if node-string (setf (atms-node-string atms) node-string))
  (if debugging (setf (atms-debugging atms) debugging))
  (if enqueue-procedure
      (setf (atms-enqueue-procedure atms) enqueue-procedure)))
</pre>
  *
  * @param title Name of this TMS, for output.
  *
  * @constructor The `title` argument is required; others are optional.
  *
  * @groupname construction Construction methods
  * @groupdesc construction API methods for building and changing
  * an ATMS from an external system.
  * @groupprio construction 1
  *
  * @groupname query Query methods
  * @groupdesc query API methods for querying the ATMS and its beliefs
  * from an external system.
  * @groupprio query 2
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
class ATMS[D, I](
  val title: String,
  var nodeString: (Node[D, I]) => String =
    (n: Node[D, I]) => s"${n.datum.toString()}",
  var debugging: Boolean = false,
  var enqueueProcedure: Option[(Rule[I]) => Unit] = None
) {

  /** Unique namer for nodes. */
  var nodeCounter: Int = 0
  /** Increment the node counter and return its value.
    *
    * @group internal
    */
  def incrNodeCounter: Int = {
    val result = nodeCounter
    nodeCounter = nodeCounter + 1
    result
  }

  /** Unique namer for justifications. */
  var justCounter: Int = 0
  /** Increment the justifications counter and return its value.
    *
    * @group internal
    */
  def incrJustCounter: Int = {
    val result = justCounter
    justCounter = justCounter + 1
    result
  }

  /** Unique namer for environments. */
  var envCounter: Int = 0
  /** Increment the environments counter and return its value.
    *
    * @group internal
    */
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

  val envTable = new EnvTable[D, I]

  val nogoodTable = new EnvTable[D, I]

  /** Empty environment. */
  val emptyEnv: Env[D, I] = createEnv(List.empty)

  /** Dummy contradiction node. */
  val contraNode: Node[D, I] =
    createNode("The contradiction", isContradictory = true)

  val makeContradictionStipulation = MakeContradiction[D, I]()

  /**
    * Return a short string with the title of this ATMS.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun print-atms (atms stream ignore)
  (declare (ignore ignore))
  (format stream "#<ATMS: ~A>" (atms-title atms)))
</pre>
    *
    * @group diagnostic
    */
  override def toString: String = s"<ATMS $title>"

  /**
    * Print a short tag with the title of this ATMS.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun print-atms (atms stream ignore)
  (declare (ignore ignore))
  (format stream "#<ATMS: ~A>" (atms-title atms)))
</pre>
    *
    * @group diagnostic
    */
  def printAtms: Unit = println(toString)

  /**
    * Print a diagnostic message when in debugging mode.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defmacro debugging (atms msg &optional node &rest args)
  `(when (atms-debugging ,atms)
     (format *trace-output*
             ,msg (if ,node (node-string ,node)) ,@args)))
</pre>
    *
    * @group diagnostic
    */
  inline def dbg(msg: String): Unit = if debugging then println(msg)

  /**
    * Add a new node to the ATMS.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun tms-create-node (atms datum &key assumptionp contradictoryp
                                   &aux node)
  (setq node (make-tms-node :INDEX (incf (atms-node-counter atms))
                            :DATUM datum
                            :ASSUMPTION? assumptionp
                            :CONTRADICTORY? contradictoryp
                            :ATMS atms))
  (push node (atms-nodes atms))
  (if contradictoryp (push node (atms-contradictions atms)))
  (when assumptionp
    (push node (atms-assumptions atms))
    (push (create-env atms (list node)) (tms-node-label node)))
  node)
</pre>
    *
    * @param datum Datum associated with this node.
    * @param isAssumption If set to `true`, then this node represents
    * an assumption.
    * @param isContradictory If set to `true`, then this node
    * represents a contradiction.
    *
    * @group construction
    */
  def createNode(
    datum: D | String,
    isAssumption: Boolean = false, isContradictory: Boolean = false):
      Node[D, I] = {
    val node = new Node[D, I](this, datum, isAssumption, isContradictory)
    nodes += node
    if isContradictory then contradictions += node
    if isAssumption then assumptions += node
    // The `(push (create-env ...` call is now in the initialization
    // of the label field of the Node.
    node
  }

  /**
    * Create a new [[Env][environment]] from a list of
    * [[Node][assumption nodes]].
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun create-env (atms assumptions &aux e)
  (setq e (make-env :INDEX (incf (atms-env-counter atms))
                    :ASSUMPTIONS assumptions
                    :COUNT (length assumptions)))
  (setf (atms-env-table atms)
        (insert-in-table (atms-env-table atms) e))
  (set-env-contradictory atms e)
  e)
</pre>
    *
    * @param assumptions
    *
    * @group internal
    */
  def createEnv(assumptions: List[Node[D, I]]): Env[D, I] = {
    val e = new Env(incrEnvCounter, assumptions)
    envTable.insertInTable(e)
    setEnvContradictory(e)
    e
  }

  /**
    * Convert an existing node into a possible assumption.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun assume-node (node &aux atms)
  (unless (tms-node-assumption? node)
    (setq atms (tms-node-atms node))
    (debugging atms  "~%Converting ~A into an assumption" node)
    (setf (tms-node-assumption? node) t)
    (push node (atms-assumptions atms))
    (update (list (create-env atms (list node)))
            node
            'ASSUME-NODE)))
</pre>
    *
    * @param node
    *
    * @group construction
    */
  def assumeNode(node: Node[D, I]): Unit = {
    if !node.isAssumption then {
      dbg(s"Converting $node into an assumption")
      node.isAssumption = true
      assumptions += node
      update(ListBuffer(getEnv(List(node))), node, NodeAssumed(node))
    }
  }

  /**
    * Designate a node as representing a contradiction.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun make-contradiction
       (node &aux (atms (tms-node-atms node)) nogood)
  (unless (tms-node-contradictory? node)
    (setf (tms-node-contradictory? node) t)
    (push node (atms-contradictions atms))
    (do nil (nil)
      (if (setq nogood (car (tms-node-label node)))
          (new-nogood atms nogood 'MAKE-CONTRADICTION)
          (return nil)))))
</pre>
    *
    * @param node
    *
    * @group construction
    */
  def makeContradiction(node: Node[D, I]): Unit = {
    if !node.isContradictory then {
      node.isContradictory = true
      var nogood = node.label.headOption
      while (!nogood.isEmpty) {
        newNogood(nogood.get, makeContradictionStipulation)
        nogood = node.label.headOption
      }
    }
  }

  /**
    * Inform the ATMS that it should believe the `consequence` node
    * whenever it believes all of the nodes in the `antecedents`.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun justify-node (informant consequence antecedents &aux just atms)
  (setq atms (tms-node-atms consequence)
        just (make-just :INDEX (incf (atms-just-counter atms))
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
             (mapcar #'node-string antecedents))
  (propagate just nil (list (atms-empty-env atms)))
  just)
</pre>
    *
    * @param informant
    * @param consequence
    * @param antecedents
    *
    * @group construction
    */
  def justifyNode(
    informant: I, consequence: Node[D, I], antecedents: ListBuffer[Node[D, I]]):
      Just[D, I] = {
    val just = new Just(incrJustCounter, informant, consequence, antecedents)
    dbg(s"Adding justification ${just.blurb}")
    consequence.justs += just
    for (node <- antecedents) do node.consequences += just
    justs += just
    dbg(s"Justifying ${consequence.datum} by $informant on ${antecedents.map(nodeString).mkString(", ")}")
    propagate(just, None, ListBuffer(emptyEnv))
    just
  }

  /**
    * Inform the ATMS that a certain group of nodes should not be
    * considered together.  The ATMS will turn this list into the
    * antecedents of a justification which concludes the built-in
    * contradiction.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun nogood-nodes (informant nodes)
  (justify-node informant
                (atms-contra-node (tms-node-atms (car nodes)))
                nodes))
</pre>
    *
    * @param informant
    * @param nodes
    *
    * @group construction
    */
  def nogoodNodes(informant: I, nodes: ListBuffer[Node[D, I]]): Unit =
    justifyNode(informant, contraNode, nodes)

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun propagate (just antecedent envs &aux new-envs)
  (if (setq new-envs (weave antecedent envs (just-antecedents just)))
      (update new-envs (just-consequence just) just)))
</pre>
    *
    * @param just
    * @param antecedent
    * @param envs This list is not mutated: `weave` returns a
    * non-shared copy of the list, so the mutations in `update` do not
    * impact the original argument.
    *
    * @group internal
    */
  def propagate(
    just: Just[D, I],
    antecedent: Option[Node[D, I]],
    envs: ListBuffer[Env[D, I]]):
      Unit = {
    dbg(s"Calling propagate with\n  just ${Blurb.justification(just)}\n  antecedent ${Blurb.nodeOption(antecedent)}\n  ${Blurb.envLB(envs)}")
    val newEnvs = weave(antecedent, envs, just.antecedents)
    if !newEnvs.isEmpty then update(newEnvs, just.consequence, just)
  }

  /**
    *
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun update (new-envs consequence just &aux atms enqueuef)
  (setq atms (tms-node-atms consequence))
  (when (tms-node-contradictory? consequence)
    (dolist (env new-envs) (new-nogood atms env just))
    (return-from update nil))
  (setq new-envs (update-label consequence new-envs))
  (unless new-envs (return-from update nil))
  (when (setq enqueuef (atms-enqueue-procedure atms))
    (dolist (rule (tms-node-rules consequence))
      (funcall enqueuef rule))
    (setf (tms-node-rules consequence) nil))
  (dolist (supported-just (tms-node-consequences consequence))
    (propagate supported-just consequence new-envs)
    (do ((new-envs new-envs (cdr new-envs)))
        ((null new-envs)) ; Exit condition only, no result value
      (unless (member (car new-envs) (tms-node-label consequence))
        (rplaca new-envs nil)))
    (setq new-envs (delete nil new-envs :TEST #'eq))
    (unless new-envs (return-from update nil))))
</pre>
    *
    * @param newEnvs This list may be mutated by this method.
    * @param consequence
    * @param just
    *
    * @group internal
    */
  def update(
    newEnvs: ListBuffer[Env[D, I]],
    consequence: Node[D, I],
    just: Justification[D, I]):
      Unit = {
    dbg(s"Calling update with\n  ${Blurb.envLB(newEnvs)}\n  consequence ${Blurb.node(consequence)}\n  just ${Blurb.justification(just)}")

    if consequence.isContradictory then {
      for (env <- newEnvs) do newNogood(env, just)
      dbg("  Registered each newEnv as nogood in ATMS")
      return
    }

    // `updateLabel` mutates its argument.
    consequence.updateLabel(newEnvs)
    if newEnvs.isEmpty then return

    enqueueProcedure.map((enqueuef) => {
      for (rule <- consequence.rules) do enqueuef(rule)
      consequence.rules.clear
    })

    returning[Unit] {
      for (supportedJust <- consequence.consequences) do {
        dbg("  Relaying to propagate for ${supportedJust.toString}")
        propagate(supportedJust, Some(consequence), newEnvs)
        val envsToRemove = ListBuffer.empty[Env[D, I]]
        for (newEnv <- newEnvs) {
          if !consequence.label.contains(newEnv) then envsToRemove += newEnv
        }
        newEnvs --= envsToRemove
        if newEnvs.isEmpty then throwReturn(())
      }
    }
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun weave (antecedent envs antecedents &aux new-envs new-env)
  (setq envs (copy-list envs))
  (dolist (node antecedents)
    (unless (eq node antecedent)

      ;; We will update ENVS with the list built in NEW-ENVS.
      (setq new-envs nil)

      ;; We look at all pairs of
      ;;  - An Env from the passed-in ENVS, plus
      ;;  - An Env from the NODE's label.
      ;; The union of these two is NEW-ENV, and the body of
      ;; the loop considers how we should incorporate NEW-ENV
      ;; into NEW-ENVS.
      (dolist (env envs)
        (if env
            (dolist (node-env (tms-node-label node))
              (setq new-env (union-env env node-env))
              (unless (env-nogood? new-env)

                ;; If NEW-ENV is a superset of (or is equal to)
                ;; anything already in NEW-ENVS, then NEW-ENV
                ;; is redundant, and we abort the body of the
                ;; inner match-searching loop without adding
                ;; NEW-ENV to NEW-ENVS.

                ;; Otherwise if anything already in NEW-ENVS is
                ;; a superset of NEW-ENV, then (1) NEW-ENV
                ;; makes that element redundant, and we strip
                ;; it out of NEW-ENVS; and (2) we add NEW-ENV
                ;; to NEW-ENVS.
                (do ((nnew-envs new-envs (cdr nnew-envs)))
                    ((null nnew-envs) (push new-env new-envs))
                  (when (car nnew-envs)
                    (case (compare-env new-env (car nnew-envs))
                      ((:EQ :S21) (return nil))
                      (:S12 (rplaca nnew-envs nil))
                          ; Could also be NIL, for mutually
                          ; non-contained sets --- ignored.
                     ))) ;; End of DO-macro.

                ;; Note that at this point the exit condition of the
                ;; DO will have added NEW-ENV to the NEW-ENVS list.

                ))))

      ;; So we have nearly produced the refinement of ENVS for
      ;; this NODE in the ANTECEDENTS.  It might have spurious
      ;; NILs, so we strip those out and update ENVS.  If ever
      ;; we narrow ENVS down to nothing, then we can short-
      ;; curcuit returning that empty list.
      (setq envs (delete nil new-envs :TEST #'eq))
      (unless envs (return-from weave nil))))

  ;; Finally, return the last refinemwnt of ENVS.
  envs)
</pre>
    * (Comments by JM.)
    *
    * @param antecedent
    * @param envs TODO
    *
    * Note that this list is duplicated at the start of the method, so
    * no changes are made to the passed-in argument.
    *
    * @param antecedents
    * @return
    *
    * @group internal
    */
  def weave(
    antecedent: Option[Node[D, I]],
    origEnvs: ListBuffer[Env[D, I]],
    antecedents: ListBuffer[Node[D, I]]):
      ListBuffer[Env[D, I]] = {
    dbg(s"Calling weave with\n  antecedent ${Blurb.nodeOption(antecedent)}\n  origEnvs ${Blurb.envLB(origEnvs)}\n  antecedents ${Blurb.nodeLB(antecedents)}")
    var envs = origEnvs.clone
    returning[Unit] {
      for (node <- antecedents; if node.differsFrom(antecedent)) do {
        dbg(s" - For antecedent node ${Blurb.node(node)}")
        val newEnvs = ListBuffer.empty[Env[D, I]]

        for (env <- envs; nodeEnv <- node.label) do {
          dbg(s"    - For ${Blurb.env(env)} from env, ${Blurb.env(nodeEnv)} from node label")
          val newEnv = env.unionEnv(nodeEnv)
          dbg(s"      Union is ${Blurb.env(newEnv)}")
          if !newEnv.isNogood then {
            if newEnvs.exists(newEnv.isSupersetEnvOf(_))
            then {
              dbg("       * Found newEnvs element subset of newEnv")
            } else {
              val toRemove = newEnvs.filter((n) => !n.isSupersetEnvOf(newEnv))
              newEnvs --= toRemove
              dbg(s"       - Removed ${Blurb.envLB(toRemove)}")
              newEnvs += newEnv
              dbg(s"       - Added ${Blurb.env(newEnv)}")
              dbg(s"       - newEnvs now ${Blurb.envLB(newEnvs)}")
            }
          } else dbg("       * newEnv is nogood")

          envs = newEnvs
          if envs.isEmpty then throwReturn(())
        }
      }
    }
    dbg(s" --> result of weave is ${Blurb.envLB(envs)}")
    envs
  }

  /**
    *
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun in-antecedent? (nodes)
  (or (null nodes)
      (weave? (atms-empty-env (tms-node-atms (car nodes))) nodes)))
</pre>
    *
    * @param nodes
    * @return
    *
    * @group internal
    */
  def isInAntecedent(nodes: Iterable[Node[D, I]]): Boolean = {
    nodes.isEmpty || emptyEnv.isWeave(nodes)
  }

  /**
    * Remove a node from the ATMS.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
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
</pre>
    *
    * @param node
    * @throws TmsError if the node is used as the consequence of any
    * justifications.
    *
    * @group construction
    */
  def removeNode(node: Node[D, I]): Unit = {
    if !node.consequences.isEmpty
    then throw new TmsError("Can't remove node with consequences")

    nodes -= node
    for (just <- node.justs) do
      for (ant <- just.antecedents) do ant.consequences -= just

    for (env <- node.label) do env.nodes -= node
  }

  /**
    * Returns the [[Env][environment]] of the ATMS containing the
    * given `assumes` nodes, if one exists.
    *
    * This method is safe to use from outside this package, since will
    * never mutate the ATMS, and just return `None` if no such
    * environment currently exists.  However, this method is sensitive
    * to the order of the nodes; it will only finds matches where the
    * nodes are given in the same order as in the environment.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun lookup-env (assumes)
  (dolist (env (cdr (assoc (length assumes)
                           (atms-env-table (tms-node-atms (car assumes)))
                           :TEST #'=))
               nil) ;; Result of the loop if exited
    (if (equal (env-assumptions env) assumes)
        (return env))))
</pre>
    *
    * @param assumes
    * @return
    *
    * @group internal
    */
  def lookupEnv(assumes: List[Node[D, I]]): Option[Env[D, I]] =
    returning {
      envTable.get(assumes.length) match {
        case Some(envs) => {
          for (env <- envs)
            do if env.assumptions.corresponds(assumes)((x,y) => x == y) then {
              throwReturn[Option[Env[D, I]]](Some(env))
            }
          None
        }
        case None => None
      }
    }

  /** Either lookup or create an [[Env]] for the given assumptions, if
    * one does not already exists.  This method should not be called
    * as an API method on the ATMS, since it may create spurious
    * records of unused environments.
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun find-or-make-env (assumptions atms)
  (unless assumptions
    (return-from find-or-make-env (atms-empty-env atms)))
  ;; Presumes the list of assumptions is ordered properly
  (or (lookup-env assumptions)
      (create-env atms assumptions)))
</pre>
    * Note also the related method [[Node#findOrMakeEnv]].
    *
    * @group internal
    */
  def getEnv(assumes: List[Node[D, I]]): Env[D, I] =
    lookupEnv(assumes).getOrElse(createEnv(assumes))

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun new-nogood (atms cenv just &aux count)
  (debugging atms (format nil "~%  ~A new minimal nogood." cenv))
  (setf (env-nogood? cenv) just)
  (remove-env-from-labels cenv atms)
  (setf (atms-nogood-table atms)
        (insert-in-table (atms-nogood-table atms) cenv))
  (setq count (env-count cenv))
  (dolist (entry (atms-nogood-table atms))
    (when (> (car entry) count)
      (dolist (old (cdr entry))
        (if (subset-env? cenv old)
            (setf (cdr entry) (delete old (cdr entry) :COUNT 1))))))
  (dolist (entry (atms-env-table atms))
    (when (> (car entry) count)
      (dolist (old (cdr entry))
        (when (and (not (env-nogood? old))
                   (subset-env? cenv old))
          (setf (env-nogood? old) cenv)
          (remove-env-from-labels old atms))))))
</pre>
    *
    * @param cenv
    * @param just
    *
    * @group internal
    */
  def newNogood(cenv: Env[D, I], just: Justification[D, I]): Unit = {
    dbg(s"        * New minimal nogood ${cenv.envString}")
    cenv.nogoodEvidence = Some(just)
    removeEnvFromLabels(cenv)
    nogoodTable.insertInTable(cenv)
    val count = cenv.count
    for ((size, sizeEnvs) <- nogoodTable)
      do if (size > count)
        then for (old <- sizeEnvs)
          do if cenv.isSubsetEnv(old) then sizeEnvs -= old
    for ((size, sizeEnvs) <- nogoodTable)
      do if (size > count)
        then for (old <- sizeEnvs)
          do if (!old.isNogood && cenv.isSubsetEnv(old))
            then {
              old.nogoodEvidence = Some(cenv)
              removeEnvFromLabels(old)
            }
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun set-env-contradictory (atms env &aux count)
  (cond ((env-nogood? env) t)
        (t (setq count (env-count env))
           (dolist (entry (atms-nogood-table atms))
             (cond ((> (car entry) count)
                    (return nil))
                   (t (dolist (cenv (cdr entry))
                        (when (subset-env? cenv env)
                          (setf (env-nogood? env)
                                cenv)
                          (return t)))))))))
</pre>
    *
    * @param env
    *
    * @group internal
    */
  def setEnvContradictory(env: Env[D, I]): Unit =
    if (!env.isNogood) then {
      val count = env.count
      returning {
        for ((size, sizeEnvs) <- nogoodTable)
          do if (count <= size)
            then returning {
              for (cenv <- sizeEnvs)
                do if (cenv.isSubsetEnv(env))
                  then {
                    env.nogoodEvidence = Some(cenv)
                    throwReturn(())
                  }
            }
      }
    }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun remove-env-from-labels (env atms &aux enqueuef)
  (when (setq enqueuef (atms-enqueue-procedure atms))
    (dolist (rule (env-rules env))
      (funcall enqueuef rule))
    (setf (env-rules env) nil))
  (dolist (node (env-nodes env))
    (setf (tms-node-label node)
          (delete env (tms-node-label node) :COUNT 1))))
</pre>
    *
    * @param env
    *
    * @group internal
    */
  def removeEnvFromLabels(env: Env[D, I]): Unit = {
    enqueueProcedure.map((enqueuef) => {
      for (rule <- env.rules) do enqueuef(rule)
      env.rules.clear
    })

    for (node <- env.nodes) do node.label -= env
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(proclaim '(special *solutions*))
</pre>
    */
  val solutions: ListBuffer[Env[D, I]] = ListBuffer.empty

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
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
                      (setq *solutions* (list (atms-empty-env atms)))))
    (when defaults
      (setq solutions *solutions* *solutions* nil)
      (dolist (solution solutions)
        (extend-via-defaults solution defaults defaults)))
    (delete nil *solutions* :TEST #'eq)))
</pre>
    *
    * @param givenChoiceSets
    * @return
    *
    * @group query
    *
    * @group internal
    */
  def interpretations(
    givenChoiceSets: ChoiceSets[D, I]):
      ListBuffer[Env[D, I]] = {
    dbg(s"Constructing interpretations depth-first...")
    solutions.clear
    val choiceSets =
      givenChoiceSets.map((altSet) => altSet.map(_.label.clone).concat)

    // for (choice <- choiceSets)
    //   do getDepthSolution1(choice, )

    ???

    solutions
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun get-depth-solutions1 (solution choice-sets
                                      &aux new-solution)
  (cond
    ((null choice-sets)
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
</pre>
    *
    * @param solution
    * @param choiceSets
    * @return
    *
    * @group internal
    */
  def getDepthSolutions1(
    solution: Env[D, I], choiceSets: ChoiceSets[D, I]):
      ListBuffer[Env[D, I]] = {
    ???
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
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
</pre>
    *
    * @param solution
    * @param remaining
    * @param original
    *
    * @group internal
    */
  def extendViaDefaults(
    solution: Env[D, I],
    remaining: ListBuffer[Node[D, I]],
    original: ListBuffer[Node[D, I]]):
      Unit = {
    ???
  }

  /**
    * Diagnostic method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun why-nodes (atms &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (why-node n stream)))
</pre>
    *
    * @group diagnostic
    */
  def whyNodes: Unit = {
    for (node <- nodes) do {
      node.whyNode("   ", " - ")
    }
  }

  /**
    * Internal method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From atms.lisp
(defun e (atms n)
  (dolist (bucket (atms-env-table atms))
    (dolist (env (cdr bucket))
      (if (= (env-index env) n) (return-from e env)))))
</pre>
    *
    * @param n
    *
    * @group internal
    */
  def e(n: Int): Env[D, I] = returning {
    for ((length, envs) <- envTable)
      do for (env <- envs)
        do if env.count == n then throwReturn(env)
    emptyEnv
  }

  /**
    * Diagnostic method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun print-nogoods (atms &optional (stream t))
  (print-env-table (atms-nogood-table atms) stream))
</pre>
    *
    * @group diagnostic
    */
  def printNogoods: Unit = {
    val count = nogoodTable.envCount
    println(s"$count nogood environment${plural(count)}")
    nogoodTable.printEnvTable(" ")
  }

  /**
    * Diagnostic method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun print-envs (atms &optional (stream t))
  (print-env-table (atms-env-table atms) stream))
</pre>
    *
    * @group diagnostic
    */
  def printEnvs: Unit = {
    println(s"${envTable.envCount} environment${plural(envTable.envCount)}")
    envTable.printEnvTable(" ")
  }

  /**
    * Diagnostic method TODO fill in description
    *
    * **Translated from**:
    * <pre>
; From ainter.lisp
(defun print-atms-statistics (atms)
  (print-table "~% For env table:" (atms-env-table atms))
  (print-table "~% For nogood table:" (atms-nogood-table atms)))
</pre>
    *
    * @group diagnostic
    */
  def printAtmsStatistics: Unit = {
    envTable.printEnvTable("For env table: ")
    nogoodTable.printEnvTable("For nogood table: ")
  }

  /**
    * Diagnostic method TODO fill in description
    *
    * @group diagnostic
    */
  inline def debugAtms: Unit = if debugging then {
    println("----------")
    printAtms
    debugNodes
    printEnvs
    printNogoods
    println("----------")
  }

  /**
    * Diagnostic method TODO fill in description
    *
    * @group diagnostic
    */
  def debugNodes: Unit = {
    println(s"${nodes.length} node${plural(nodes.length)}")
    for (node <- nodes) do {
      node.debugNode
    }
  }
}

/** Type of exceptions thrown from ATMS package classes. */
class TmsError(val msg: String) extends RuntimeException(msg)

// ; From atms.lisp
// (defmacro ordered-push (item list test)
//   `(setq ,list (ordered-insert ,item ,list ,test)))
