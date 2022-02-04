{-|
Module      : JTMS
Description : Justification-based truth maintenance systems (JTMSes)
Copyright   : (c) John Maraist, 2022
              Kenneth D. Forbus, Johan de Kleer and Xerox Corporation, 1986-1993
License     : AllRightsReserved
Maintainer  : haskell-tms@maraist.org
Stability   : experimental
Portability : POSIX

Translation of Forbus and de Kleer's justification-based truth
maintenance systems (JTMSes) from Common Lisp to Haskell.

This is not a very \"Haskelly\" implementation; rather, it is a
translation of the original code with minimal changes.  Most of the
deviations from the original are due to either Haskell's strong
typing, which necessitates some additional tagging, and to the
abomination which is Lisp's @do@ macro.  The translation relies on
mutable data structures using `STT` state thread references.  A more
pure translation, possibly not relying on the [@ST@
monad]("Control.Monad.ST")/[@STT@
transformer]("Control.Monad.ST.Trans"), is a significant piece of
future work.

Note also there are restrictions on the embedded monad @m@ which can
be wrapped in the `STT` transformer; see [the @Control.Monad.ST.Trans@
documentation]("Control.Monad.ST.Trans") for details.

See the @LICENSE.txt@ and @README-forbus-dekleer.txt@ files
distributed with this work for a paragraph stating scope of permission
and disclaimer of warranty, and for additional information regarding
copyright ownership.  The above copyright notice and that paragraph
must be included in any separate copy of this file.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied, for NON-COMMERCIAL use.  See the License for the specific
language governing permissions and limitations under the License.

-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module JTMS where
import Control.Monad.State
import Control.Monad.ST.Trans

-- The process of building and using a mutable JTMS.
-- type JtmsT s m a = StateT (JS s) (STT s m) a

-- |Standalone implementation of justification-based truth maintenance
-- systems.
--
--  * @d@ is the type of data associated with each `Node` of this JTMS.
--  * @i@ is the type of informants in the external system.
--  * @r@ is the type of rules which may be associated with each `Node` of this JTMS.
--  * @s@ is the (phantom) type of the state thread.
--  * @m@ is the monad in which this computation lives.
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defstruct (jtms (:PRINT-FUNCTION print-jtms))
-- >   (title nil)
-- >   (node-counter 0)             ;; unique namer for nodes.
-- >   (just-counter 0)             ;; unique namer for justifications.
-- >   (nodes nil)                  ;; list of all tms nodes.
-- >   (justs nil)                  ;; list of all justifications
-- >   (debugging nil)              ;; debugging flag
-- >   (contradictions nil)         ;; list of contradiction nodes.
-- >   (assumptions nil)            ;; list of assumption nodes.
-- >   (checking-contradictions T)  ;; For external systems
-- >   (node-string nil)
-- >   (contradiction-handler nil)
-- >   (enqueue-procedure nil))
data Monad m => JTMS d i r s m = JTMS {
  -- |Name of this JTMS.
  jtmsTitle :: String,
  -- |Unique namer for nodes.
  jtmsNodeCounter :: STRef s Int,
  -- |Unique namer for justifications.
  jtmsJustCounter :: STRef s Int,
  -- |List of all TMS nodes.
  jtmsNodes :: STRef s [Node d i r s m],
  -- |List of all justifications.
  jtmsJusts :: STRef s [JustRule d i r s m],
  -- |List of all contradiction nodes.
  jtmsContradictions :: STRef s [Node d i r s m],
  -- |List of all assumption nodes.
  jtmsAssumptions :: STRef s [Node d i r s m],
  -- |For external systems.
  jtmsCheckingContradictions :: STRef s Bool,
  jtmsNodeString :: STRef s (Node d i r s m -> String),
  jtmsEnqueueProcedure :: STRef s (r -> STT s m ()),
  jtmsContradictionHandler :: STRef s ([Node d i r s m] -> STT s m ())

}

-- |Create and return a new JTMS.
newJTMS :: Monad m => String -> STT s m (JTMS d i r s m)
newJTMS title = do
  nc <- newSTRef 0
  jc <- newSTRef 0
  nodes <- newSTRef ([] :: [Node d i r s m])
  justs <- newSTRef ([] :: [JustRule d i r s m])
  contradictions <- newSTRef ([] :: [Node d i r s m])
  assumptions <- newSTRef ([] :: [Node d i r s m])
  checkingContradictions <- newSTRef True
  nodeString <- newSTRef (\ node -> "?")
  enqueueProcedure <- newSTRef (\ _ -> return ())
  contradictionHandler <- newSTRef (\ _ -> return ())
  return (JTMS title nc jc nodes justs contradictions assumptions
               checkingContradictions nodeString enqueueProcedure
               contradictionHandler)

-- |Print a simple tag with the title of this JTMS.  Forces the
-- enclosed monad to be `MonadIO`.
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun print-jtms (jtms stream ignore)
-- >   (declare (ignore ignore))
-- >   (format stream "#<JTMS: ~A>" (jtms-title jtms)))
printJTMS jtms = liftIO $ print $ jtmsTitle jtms

-- |Wrapper for one possible belief known to the TMS.
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
-- >   (index 0)
-- >   (datum nil)           ;; pointer to external problem solver
-- >   (label :OUT)          ;; :IN means believed, :OUT means disbelieved
-- >   (support nil)         ;; Current justification or premise marker
-- >   (justs nil)           ;; Possible justifications
-- >   (consequences nil)    ;; Justifications in which it is an antecedent
-- >   (mark nil)            ;; Marker for sweep algorithms
-- >   (contradictory? nil)  ;; Flag marking it as contradictory
-- >   (assumption? nil)     ;; Flag marking it as an assumption.
-- >   (in-rules nil)     ;; Rules that should be triggered when node goes in
-- >   (out-rules nil)    ;; Rules that should be triggered when node goes out
-- >   (jtms nil))           ;; The JTMS in which this node appears.
data Monad m => Node d i r s m = Node {
  nodeIndex :: Int,
  nodeDatum :: d,
  nodeJTMS :: JTMS d i r s m,
  nodeIsAssumption :: STRef s Bool,
  nodeIsContradictory :: STRef s Bool,
  nodeSupport :: STRef s (Maybe (Justification d i r s m)),
  nodeBelieved :: STRef s Bool,
  nodeConsequences :: STRef s [JustRule d i r s m],
  nodeInRules :: STRef s [r],
  nodeOutRules :: STRef s [r],
  nodeJusts :: STRef s [JustRule d i r s m]
}

-- |Write one node in the standard way for this JTMS.  Forces the
-- wrapped monad to be `MonadIO`.
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun print-tms-node (node stream ignore)
-- >   (declare (ignore ignore))
-- >   (format stream "#<Node: ~A>" (node-string node)))
printTmsNode :: MonadIO m => Node d i r s m -> STT s m ()
printTmsNode node = do
  s <- nodeString node
  liftIO $ print s

-- |Wrapper for one justification relationship between many antecedent
-- nodes and one consequent node.
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defstruct (just (:PRINT-FUNCTION print-just))
-- >   (index 0)
-- >   informant
-- >   consequence
-- >   antecedents)
data Monad m => JustRule d i r s m = JustRule {
  justIndex :: Int,
  justInformant :: i,
  justConsequence :: Node d i r s m,
  justAntecedents :: [Node d i r s m]
}

-- |Print the tag of a JTMS justification.
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun print-just (just stream ignore)
-- >   (declare (ignore ignore))
-- >   (format stream "#<Just ~D>" (just-index just)))
printJustRule :: MonadIO m => JustRule d i r s m -> STT s m ()
printJustRule just = liftIO $ print $ justIndex just

-- |Forms of data which might signal support for a node.  The original
-- Lisp does not need this declaration since it is untyped; the latter
-- two cases are simply symbols.
data Monad m => Justification d i r s m =
  ByRule (JustRule d i r s m) | EnabledAssumption | UserStipulation

-- Returns @True@ when the node is supported by a `JustRule` with no
-- antecedents.
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun tms-node-premise? (node &aux support)
-- >   (and (setq support (tms-node-support node))
-- >        (not (eq support :ENABLED-ASSUMPTION))
-- >        (null (just-antecedents support))))
nodeIsPremise :: Monad m => Node d i r s m -> STT s m Bool
nodeIsPremise node = do
  support <- readSTRef $ nodeSupport node
  case support of
    Just (ByRule (JustRule _ _ _ antecedents)) -> return $ null antecedents
    _ -> return False

-- * Simple utilities

-- |Produce a representation of the node in the default manner for its
-- JTMS.
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun node-string (node)
-- >   (funcall (jtms-node-string (tms-node-jtms node)) node))
nodeString :: Monad m => Node d i r s m -> STT s m String
nodeString node = do
  ns <- readSTRef $ jtmsNodeString $ nodeJTMS node
  return (ns node)

-- TODO
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defmacro debugging-jtms (jtms msg &optional node &rest args)
-- >   `(when (jtms-debugging ,jtms)
-- >      (format *trace-output* ,msg (if ,node (node-string ,node)) ,@args)))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun tms-error (string node) (error string (node-string node)))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun default-node-string (n) (format nil "~A" (tms-node-datum n)))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun create-jtms (title &key (node-string 'default-node-string)
-- >                                debugging
-- >                                (checking-contradictions t)
-- >                                (contradiction-handler 'ask-user-handler)
-- >                                enqueue-procedure)
-- >   (make-jtms :TITLE title
-- >           :NODE-STRING node-string
-- >           :DEBUGGING debugging
-- >           :CHECKING-CONTRADICTIONS checking-contradictions
-- >           :CONTRADICTION-HANDLER contradiction-handler
-- >           :ENQUEUE-PROCEDURE enqueue-procedure
-- >           ))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun change-jtms (jtms &key contradiction-handler node-string
-- >                            enqueue-procedure debugging
-- >                               checking-contradictions)
-- >   (if node-string (setf (jtms-node-string jtms) node-string))
-- >   (if debugging (setf (jtms-debugging jtms) debugging))
-- >   (if checking-contradictions
-- >       (setf (jtms-checking-contradictions jtms)
-- >          checking-contradictions))
-- >   (if contradiction-handler
-- >       (setf (jtms-contradiction-handler jtms) contradiction-handler))
-- >   (if enqueue-procedure
-- >       (setf (jtms-enqueue-procedure jtms) enqueue-procedure)))


-- * Basic inference-engine interface

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun in-node? (node) (eq (tms-node-label node) :IN))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun out-node? (node) (eq (tms-node-label node) :OUT))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun tms-create-node (jtms datum &key assumptionp contradictoryp)
-- >   (let ((node (make-tms-node :INDEX (incf (jtms-node-counter jtms))
-- >                           :DATUM datum
-- >                           :ASSUMPTION? assumptionp
-- >                           :CONTRADICTORY? contradictoryp
-- >                           :JTMS jtms)))
-- >     (if assumptionp (push node (jtms-assumptions jtms)))
-- >     (if contradictoryp (push node (jtms-contradictions jtms)))
-- >     (push node (jtms-nodes jtms))
-- >     node))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > ;;; Converts a regular node to an assumption and enables it.
-- > (defun assume-node (node &aux (jtms (tms-node-jtms node)))
-- >   (unless (or (tms-node-assumption? node) (tms-node-premise? node))
-- >     (debugging-jtms jtms "~%Converting ~A into an assumption" node)
-- >     (setf (tms-node-assumption? node) t)
-- >     (push node (jtms-assumptions jtms)))
-- >   (enable-assumption node))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun make-contradiction (node &aux (jtms (tms-node-jtms node)))
-- >   (unless (tms-node-contradictory? node)
-- >     (setf (tms-node-contradictory? node) t)
-- >     (push node (jtms-contradictions jtms))
-- >     (check-for-contradictions jtms)))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun justify-node (informant consequence antecedents &aux just jtms)
-- >   (setq jtms (tms-node-jtms consequence)
-- >      just (make-just :INDEX (incf (jtms-just-counter jtms))
-- >                      :INFORMANT informant
-- >                      :CONSEQUENCE consequence
-- >                      :ANTECEDENTS antecedents))
-- >   (push just (tms-node-justs consequence))
-- >   (dolist (node antecedents) (push just (tms-node-consequences node)))
-- >   (push just (jtms-justs jtms))
-- >   (debugging-jtms jtms
-- >                "~%Justifying ~A by ~A using ~A."
-- >                consequence
-- >                informant
-- >                (mapcar #'node-string antecedents))
-- >   (if (or antecedents (out-node? consequence))
-- >       (if (check-justification just) (install-support consequence just))
-- >       (setf (tms-node-support consequence) just))
-- >   (check-for-contradictions jtms))

-- * Support for adding justifications

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun check-justification (just)
-- >   (and (out-node? (just-consequence just))
-- >        (justification-satisfied? just)))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun justification-satisfied? (just)
-- >   (every #'in-node? (just-antecedents just)))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun install-support (conseq just)
-- >   (make-node-in conseq just)
-- >   (propagate-inness conseq))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun propagate-inness (node &aux (jtms (tms-node-jtms node)) (q (list node)))
-- >   (do () ((null (setq node (pop q))))
-- >     (debugging-jtms jtms "~%   Propagating belief in ~A." node)
-- >     (dolist (justification (tms-node-consequences node))
-- >       (when (check-justification justification)
-- >      (make-node-in (just-consequence justification) justification)
-- >      (push (just-consequence justification) q)))))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun make-node-in (conseq reason &aux jtms enqueuef)
-- >   (setq jtms (tms-node-jtms conseq)
-- >      enqueuef (jtms-enqueue-procedure jtms))
-- >   (debugging-jtms jtms "~%     Making ~A in via ~A."
-- >           conseq
-- >           (if (symbolp reason)
-- >               reason
-- >               (cons (just-informant reason)
-- >                     (mapcar (jtms-node-string jtms)
-- >                             (just-antecedents reason)))))
-- >   (setf (tms-node-label conseq) :IN)
-- >   (setf (tms-node-support conseq) reason)
-- >   (when enqueuef
-- >     (dolist (in-rule (tms-node-in-rules conseq))
-- >       (funcall enqueuef in-rule))
-- >     (setf (tms-node-in-rules conseq) nil)))


-- > * Assumption Manipulation

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun retract-assumption (node &aux jtms)
-- >   (when (eq (tms-node-support node) :ENABLED-ASSUMPTION)
-- >     (setq jtms (tms-node-jtms node))
-- >     (debugging-jtms jtms "~%  Retracting assumption ~A." node)
-- >     (make-node-out node)
-- >     (find-alternative-support jtms (cons node (propagate-outness node jtms)))))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun enable-assumption (node &aux (jtms (tms-node-jtms node)))
-- >   (unless (tms-node-assumption? node)
-- >     (tms-error "Can't enable the non-assumption ~A" node))
-- >   (debugging-jtms jtms "~%  Enabling assumption ~A." node)
-- >   (cond ((out-node? node) (make-node-in node :ENABLED-ASSUMPTION)
-- >                        (propagate-inness node))
-- >      ((or (eq (tms-node-support node) :ENABLED-ASSUMPTION)
-- >           (null (just-antecedents (tms-node-support node)))))
-- >      (t (setf (tms-node-support node) :ENABLED-ASSUMPTION)))
-- >   (check-for-contradictions jtms))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun make-node-out (node &aux jtms enqueuef)
-- >   (setq jtms (tms-node-jtms node)
-- >      enqueuef (jtms-enqueue-procedure jtms))
-- >   (debugging-jtms jtms "~%     retracting belief in ~a." node)
-- >   (setf (tms-node-support node) nil)
-- >   (setf (tms-node-label node) :OUT)
-- >   (if enqueuef (dolist (out-rule (tms-node-out-rules node))
-- >               (funcall enqueuef out-rule)))
-- >   (setf (tms-node-out-rules node) nil))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun propagate-outness (node jtms &aux out-queue)
-- >   (debugging-jtms jtms "~%   Propagating disbelief in ~A." node)
-- >   (do ((js (tms-node-consequences node) (append (cdr js) new))
-- >        (new nil nil)
-- >        (conseq nil))
-- >       ((null js) out-queue)
-- >     ;;For each justification using the node, check to see if
-- >     ;;it supports some other node.  If so, forget that node,
-- >     ;;queue up the node to look for other support, and recurse
-- >     (setq conseq (just-consequence (car js)))
-- >     (when (eq (tms-node-support conseq) (car js))
-- >       (make-node-out conseq)
-- >       (push conseq out-queue)
-- >       (setq new (tms-node-consequences conseq)))))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun find-alternative-support (jtms out-queue)
-- >   (debugging-jtms jtms "~%   Looking for alternative supports.")
-- >   (dolist (node out-queue)
-- >     (unless (in-node? node)
-- >       (dolist (just (tms-node-justs node))
-- >      (when (check-justification just)
-- >        (install-support (just-consequence just)
-- >                               just)
-- >        (return just))))))


-- > * Contradiction handling interface

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun check-for-contradictions (jtms &aux contradictions)
-- >   (when (jtms-checking-contradictions jtms)
-- >     (dolist (cnode (jtms-contradictions jtms))
-- >       (if (in-node? cnode) (push cnode contradictions)))
-- >     (if contradictions
-- >      (funcall (jtms-contradiction-handler jtms) jtms contradictions))))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defmacro without-contradiction-check (jtms &body body)
-- >   (contradiction-check jtms nil body))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defmacro with-contradiction-check (jtms &body body)
-- >   (contradiction-check jtms t body))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun contradiction-check (jtms flag body)
-- >   (let ((jtmsv (gensym)) (old-value (gensym)))
-- >     `(let* ((,jtmsv ,jtms)
-- >          (,old-value (jtms-checking-contradictions ,jtmsv)))
-- >        (unwind-protect
-- >         (progn (setf (jtms-checking-contradictions ,jtmsv) ,flag) ,@body)
-- >       (setf (jtms-checking-contradictions ,jtmsv) ,old-value)))))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defmacro with-contradiction-handler (jtms handler &body body)
-- >   (let ((jtmsv (gensym)) (old-handler (gensym)))
-- >     `(let* ((,jtmsv ,jtms)
-- >          (,old-handler (jtms-contradiction-handler ,jtmsv)))
-- >      (unwind-protect
-- >       (progn (setf (jtms-contradiction-handler ,jtmsv) ,handler) ,@body)
-- >        (setf (jtms-contradiction-handler ,jtmsv) ,old-handler)))))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun default-assumptions (jtms)
-- >   (with-contradiction-check jtms
-- >     (with-contradiction-handler jtms #'(lambda (&rest ignore)
-- >                                       (declare (ignore ignore))
-- >                                       (throw 'CONTRADICTION t))
-- >       (dolist (assumption (jtms-assumptions jtms))
-- >      (cond ((eq (tms-node-support assumption) :ENABLED-ASSUMPTION))
-- >            ((not (eq :DEFAULT (tms-node-assumption? assumption))))
-- >            ((catch 'CONTRADICTION (enable-assumption assumption))
-- >             (retract-assumption assumption)))))))


-- > * Well-founded support inqueries

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun supporting-justification-for-node (node) (tms-node-support node))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun assumptions-of-node (node &aux assumptions (marker (list :MARK)))
-- >   (do ((nodes (list node) (append (cdr nodes) new))
-- >        (new nil nil))
-- >       ((null nodes) assumptions)
-- >     (let ((node (car nodes)))
-- >       (cond ((eq (tms-node-mark node) marker))
-- >          ((eq (tms-node-support node) :ENABLED-ASSUMPTION)
-- >           (push node assumptions))
-- >          ((in-node? node)
-- >           (setq new (just-antecedents (tms-node-support node)))))
-- >       (setf (tms-node-mark node) marker))))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun enabled-assumptions (jtms &aux result)
-- >   (dolist (assumption (jtms-assumptions jtms) result)
-- >     (if (eq (tms-node-support assumption) :ENABLED-ASSUMPTION)
-- >      (push assumption result))))


-- > * Inference engine stub to allow this JTMS to be used stand alone

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun why-node (node &aux justification)
-- >   (setq justification (tms-node-support node))
-- >   (cond ((eq justification :ENABLED-ASSUMPTION)
-- >       (format t "~%~A is an enabled assumption"
-- >               (node-string node)))
-- >      (justification
-- >       (format t "~%~A is IN via ~A on"
-- >               (node-string node)
-- >               (just-informant justification))
-- >       (dolist (anode (just-antecedents justification))
-- >         (format t "~%  ~A" (node-string anode))))
-- >      (T (format t "~%~A is OUT." (node-string node))))
-- >   node)

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun why-nodes (jtms)
-- >   (dolist (node (jtms-nodes jtms)) (why-node node)))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (proclaim '(special *contra-assumptions*))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun ask-user-handler (jtms contradictions)
-- >   (handle-one-contradiction (car contradictions))
-- >   (check-for-contradictions jtms))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun handle-one-contradiction (contra-node
-- >                               &aux the-answer *contra-assumptions*)
-- >   (setq *contra-assumptions* (assumptions-of-node contra-node))
-- >   (unless *contra-assumptions*
-- >     (tms-error "~%There is a flaw in the universe...~A" contra-node))
-- >   (format t "~%Contradiction found: ~A" (node-string contra-node))
-- >   (print-contra-list *contra-assumptions*)
-- >   (format t "~%Call (TMS-ANSWER <number>) to retract assumption.")
-- >   (setq the-answer
-- >      (catch 'tms-contradiction-handler
-- >        (break "JTMS contradiction break")))
-- >   (if (and (integerp the-answer)
-- >         (> the-answer 0)
-- >         (not (> the-answer (length *contra-assumptions*))))
-- >       (retract-assumption (nth (1- the-answer)
-- >                             *contra-assumptions*))))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun print-contra-list (nodes)
-- >   (do ((counter 1 (1+ counter))
-- >        (nn nodes (cdr nn)))
-- >       ((null nn))
-- >     (format t "~%~A ~A" counter
-- >          (node-string (car nn)))))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > (defun tms-answer (num)
-- >   (if (integerp num)
-- >       (if (> num 0)
-- >        (if (not (> num (length *contra-assumptions*)))
-- >            (throw 'tms-contradiction-handler num)
-- >            (format t "~%Ignoring answer, too big."))
-- >        (format t "~%Ignoring answer, too small"))
-- >       (format t "~%Ignoring answer, must be an integer.")))

--
--
-- /Translated from/:
--
-- > ;; In jtms.lisp:
-- > 
-- > (defun explore-network (node)
-- >   (unless (in-node? node)
-- >        (format t "~% Sorry, ~A not believed." (node-string node))
-- >        (return-from explore-network node))
-- >   (do ((stack nil)
-- >        (current node)
-- >        (options nil)
-- >        (olen 0)
-- >        (done? nil))
-- >       (done? current)
-- >       (why-node current)
-- >       (setq options (if (typep (tms-node-support current) 'just)
-- >                      (just-antecedents (tms-node-support current))))
-- >       (setq olen (length options))
-- >       (do ((good? nil)
-- >         (choice 0))
-- >        (good? (case good?
-- >                     (q (return-from explore-network current))
-- >                     (0 (if stack
-- >                            (setq current (pop stack))
-- >                            (return-from explore-network current)))
-- >                     (t (push current stack)
-- >                        (setq current (nth (1- good?) options)))))
-- >        (format t "~%>>>")
-- >        (setq choice (read))
-- >        (cond ((or (eq choice 'q)
-- >                   (and (integerp choice)
-- >                        (not (> choice olen))
-- >                        (not (< choice 0))))
-- >               (setq good? choice))
-- >              (t (format t
-- >                  "~% Must be q or an integer from 0 to ~D."
-- >                  olen))))))


-- |This instance declaration is not part of `STT`, but it is
-- convenient.
instance MonadIO m => MonadIO (STT s m) where
  liftIO = lift . liftIO
