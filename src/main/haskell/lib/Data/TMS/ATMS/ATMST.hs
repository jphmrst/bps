{-|
Module      : ATMS
Description : Mutable assumption-based truth maintenance systems (ATMSes)
Copyright   : (c) John Maraist, 2022
              Kenneth D. Forbus, Johan de Kleer and Xerox Corporation, 1986-1993
License     : AllRightsReserved
Maintainer  : haskell-tms@maraist.org
Stability   : experimental
Portability : POSIX

Translation of Forbus and de Kleer's assumption-based truth
maintenance systems (ATMSes) from Common Lisp to Haskell.

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

{-# LANGUAGE RankNTypes #-}

module Data.TMS.ATMS.ATMST (
  -- * The ATMST monad
  ATMST, AtmsErr, runATMST,

  ATMS, createATMS,

  Node, createNode, assumeNode, makeContradiction,
  justifyNode, removeNode, nodeString, defaultNodeString,
  isTrueNode, isInNode, isOutNode, isNodeConsistentWith,
  getNodeLabels,

  JustRule, Justification, Explanation,

  Env, EnvTable,

  interpretations,

  printAtms, printNode, printJust, printEnvStructure,
  printJustification, printEnv, printNogoods,
  printEnvs, printEnvTable, printAtmsStatistics, printTable,
  whyNodes, whyNode,

  explainNode
  ) where

import Control.Monad.State
import Control.Monad.ST.Trans
import Control.Monad.Except
import Control.Monad.Extra
import Data.TMS.Helpers

-- * The @ATMST@ monad transformer
--
-- Construction and manipulation of a ATMS happens inside this monad
-- wrapper.

-- |Errors which can arise from ATMS operations.
data AtmsErr = CannotEnableNonassumption String Int deriving Show

{- ===== Internal state of an ATMST. =================================== -}

-- |Internal state of an ATMST process
data AtmstState = AtmstState {
  initialEnvTableAlloc :: Int,
  envTableIncr :: Int
  }

-- |Initial state of an ATMST process.
initialAtmstState :: AtmstState
initialAtmstState = AtmstState 50 75

-- |Update the initial table size of an ATMST state.
withInitialEnvTableAlloc :: AtmstState -> Int -> AtmstState
withInitialEnvTableAlloc (AtmstState _ ei) ia = AtmstState ia ei

-- |Update the table increment size of an ATMST state.
withEnvTableIncr :: AtmstState -> Int -> AtmstState
withEnvTableIncr (AtmstState ia _) ei = AtmstState ia ei

{- ===== ATMST definition. ============================================= -}

-- |The process of building and using a mutable ATMS.
type ATMSTInner s m a =
  Monad m => ExceptT AtmsErr (StateT AtmstState (STT s m)) a

-- |The process of building and using a mutable ATMS.
newtype Monad m => ATMST s m a = AtmsT { unwrap :: ATMSTInner s m a }

-- |Internal unwrapper preserving rank-2 polymorphism of the state
-- thread in the wrapper `STT`.
unwrap2 :: Monad m => (forall s . ATMST s m a) -> (forall s . ATMSTInner s m a)
unwrap2 (AtmsT m) = m

instance (Monad m) => Functor (ATMST s m) where
  fmap f (AtmsT m) = AtmsT $ do
    v <- m
    return $ f v

instance (Monad m, Functor m) => Applicative (ATMST s m) where
  pure v = AtmsT $ pure v
  (AtmsT m1) <*> (AtmsT m2) = AtmsT $ do
    f <- m1
    v <- m2
    return (f v)

instance (Monad m, Functor m) => Monad (ATMST s m) where
  -- (>>=) :: ATMST s m a -> (a -> ATMST s m b) -> ATMST s m b
  (AtmsT m) >>= f = AtmsT $ m >>= (unwrap . f)

  -- (>>) :: ATMST s m a -> ATMST s m b -> ATMST s m b
  (AtmsT m1) >> (AtmsT m2) = AtmsT $ m1 >> m2

  -- return :: a -> ATMST s m a
  return v = AtmsT $ return v

instance MonadTrans (ATMST s) where
  lift m = AtmsT $ lift $ lift $ lift m

instance MonadIO m => MonadIO (ATMST s m) where
  liftIO = lift . liftIO

-- |Lift `STT` behavior to the `ATMST` level.
sttLayer :: Monad m => STT s m r -> ATMST s m r
sttLayer md = AtmsT $ lift $ lift $ md

-- |Lift `ExceptT` behavior to the `ATMST` level.
exceptLayer ::
  Monad m => ExceptT AtmsErr (StateT AtmstState (STT s m)) r -> ATMST s m r
exceptLayer = AtmsT

-- |Lift `StateT` behavior to the `ATMST` level.
stateLayer ::
  Monad m => StateT AtmstState (STT s m) r -> ATMST s m r
stateLayer = AtmsT . lift

-- |Execute a computation in the `ATMST` monad transformer.
runATMST :: Monad m => (forall s . ATMST s m r) -> m (Either AtmsErr r)
runATMST atmst =
  let core = unwrap2 atmst
      afterExcept = runExceptT core
      afterState = do
        (result, endState) <- runStateT afterExcept initialAtmstState
        return result
  in runSTT afterState

-- > ;; In atms.lisp
-- > (defstruct (atms (:PRINT-FUNCTION print-atms))
-- >   (title nil)
-- >   (node-counter 0)              ; unique namer for nodes.
-- >   (just-counter 0)              ; unique namer for justifications.
-- >   (env-counter 0)               ; Unique id for environments.
-- >   (nodes nil)                   ; List of all atms nodes.
-- >   (justs nil)                   ; List of all justifications.
-- >   (contradictions nil)          ; List of contradiction nodes.
-- >   (assumptions nil)             ; List of all atms assumptions.
-- >   (debugging nil)               ; Trace grungy details.
-- >   (nogood-table nil)
-- >   (contra-node nil)             ; A dummy contradiction node.
-- >   (env-table nil)
-- >   (empty-env nil)               ; Empty environment.
-- >   (node-string nil)
-- >   (enqueue-procedure nil))
data Monad m => ATMS d i r s m = ATMS {
  -- |Name of this ATMS.
  atmsTitle :: String,
  -- |Unique namer for nodes.
  atmsNodeCounter :: STRef s Int,
  -- |Unique namer for justifications.
  atmsJustCounter :: STRef s Int,
  -- |Unique namer for environments.
  atmsEnvCounter :: STRef s Int,
  -- |List of all TMS nodes.
  atmsNodes :: STRef s [Node d i r s m],
  -- |List of all justifications.
  atmsJusts :: STRef s [JustRule d i r s m],
  -- |List of all contradiction nodes.
  atmsContradictions :: STRef s [Node d i r s m],
  -- |List of all assumption nodes.
  atmsAssumptions :: STRef s [Node d i r s m],
  -- TODO nogood-table, contra-node, env-table, empty-env
  atmsNodeString :: STRef s (Node d i r s m -> String),
  atmsJustString :: STRef s (JustRule d i r s m -> String),
  atmsDatumString :: STRef s (d -> String),
  atmsInformantString :: STRef s (i -> String),
  atmsEnqueueProcedure :: STRef s (r -> ATMST s m ()),
  atmsDebugging :: STRef s Bool
}

-- > ;; In atms.lisp
-- > (defun print-atms (atms stream ignore)
-- >   (declare (ignore ignore))
-- >   (format stream "#<ATMS: ~A>" (atms-title atms)))
printAtms :: MonadIO m => ATMS d i r s m -> ATMST s m ()
printAtms = error "< TODO unimplemented >"

-- > ;; In atms.lisp
-- > (defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
-- >   (index 0)                                        ;; Unique name.
-- >   (datum nil)                   ; Pointer to IE data structures.
-- >   (label nil)                   ; minimal envs believed under
-- >   (justs nil)                   ; providers of support
-- >   (consequences nil)            ; provides support for.
-- >   (contradictory? nil)          ; flag marking it as contradictory.
-- >   (assumption? nil)             ; flag marking it as n assumption.
-- >   (rules nil)                   ; run when label non-empty.
-- >   (atms nil))
data Monad m => Node d i r s m = Node {
  nodeIndex :: Int
}

-- > (defun print-tms-node (node stream ignore)
-- >   (declare (ignore ignore))
-- >   (if (tms-node-assumption? node)
-- >       (format stream "A-~D" (tms-node-index node))
-- >       (format stream "#<NODE: ~A>" (node-string node))))
printNode :: MonadIO m => Node d i r s m -> ATMST s m ()
printNode = error "< TODO unimplemented >"

-- > ;; In atms.lisp
-- > (defstruct (just (:PRINT-FUNCTION print-just))
-- >       (index 0)
-- >       (informant nil)
-- >       (consequence nil)
-- >       (antecedents nil))
data Monad m => JustRule d i r s m = JustRule {
  justIndex :: Int
}

-- > ;; In atms.lisp
-- > (defun print-just (just stream ignore)
-- >   (declare (ignore ignore))
-- >   (format stream "<~A ~D>" (just-informant just)
-- >      (just-index just)))
printJust :: MonadIO m => JustRule d i r s m -> ATMST s m ()
printJust = error "< TODO unimplemented printJust >"

data Justification d i r s m =
  ByRule (JustRule d i r s m) | ByAssumption (Node d i r s m) | ByContradiction

data Explanation d i r s m =
  IsRule (JustRule d i r s m) | IsAssumption (Node d i r s m)

-- > ;; In atms.lisp
-- > (defstruct (env (:PREDICATE env?)
-- >            (:PRINT-FUNCTION print-env-structure))
-- >       (index 0)
-- >       (count 0)                            ; Number of assumptions.
-- >       (assumptions nil)
-- >       (nodes nil)
-- >       (nogood? nil)
-- >       (rules nil))                         ; Call this if becomes nogood.
data Monad m => Env d i r s m = Env {
}

getNodeLabels :: ATMS d i r s m -> Node d i r s m  -> ATMST s m [Env d i r s m]
getNodeLabels atms node = error "< TODO unimplemented getNodeLabels >"

data Monad m => EnvTable d i r s m = EnvTable {
}

-- > ;; In atms.lisp
-- > (defun print-env-structure (env stream ignore)
-- >   (declare (ignore ignore))
-- >   (format stream "E-~D" (env-index env)))
printEnvStructure :: MonadIO m => Env d i r s m -> ATMST s m ()
printEnvStructure = error "< TODO unimplemented printEnvStructure >"

-- > ;; In atms.lisp
-- > (defun node-string (node)
-- >   (funcall (atms-node-string (tms-node-atms node)) node))
nodeString :: Monad m => Node d i r s m -> ATMST s m String
nodeString = error "< TODO unimplemented nodeString >"

-- > ;; In atms.lisp
-- > (defmacro debugging (atms msg &optional node &rest args)
-- >   `(when (atms-debugging ,atms)
-- >      (format *trace-output*
-- >         ,msg (if ,node (node-string ,node)) ,@args)))

-- > ;; In atms.lisp
-- > (defun default-node-string (n) (format nil "~A" (tms-node-datum n)))
defaultNodeString :: Monad m => Node d i r s m -> ATMST s m String
defaultNodeString = error "< TODO unimplemented defaultNodeString >"

-- > ;; In atms.lisp
-- > (defun ordered-insert (item list test)
-- >   (cond ((null list) (list item))
-- >    ((funcall test item (car list)) (cons item list))
-- >    ((eq item (car list)) list)
-- >    (t (cons (car list) (ordered-insert item (cdr list) test)))))
orderedInsert :: a -> [a] -> (a -> a -> Bool) -> [a]
orderedInsert = error "< TODO unimplemented orderedInsert >"

-- > ;; In atms.lisp
-- > (defmacro ordered-push (item list test)
-- >   `(setq ,list (ordered-insert ,item ,list ,test)))
orderedPush :: a -> [a] -> (a -> a -> Bool) -> [a]
orderedPush = error "< TODO unimplemented orderedPush >"

-- > ;; In atms.lisp
-- > (defun assumption-order (a1 a2)
-- >   (< (tms-node-index a1) (tms-node-index a2)))
assumptionOrder :: Monad m => Node d i r s m -> Node d i r s m -> Bool
assumptionOrder = error "< TODO unimplemented assumptionOrder >"

-- > ;; In atms.lisp
-- > (defun env-order (e1 e2)
-- >   (< (env-index e1) (env-index e2)))
envOrder :: Monad m => Node d i r s m -> Node d i r s m -> Bool
envOrder = error "< TODO unimplemented envOrder >"

-- * Basic inference engine interface.

-- > ;; In atms.lisp
-- > (defun create-atms (title &key (node-string 'default-node-string)
-- >                           (debugging NIL)
-- >                           (enqueue-procedure NIL))
-- >   (let ((atms (make-atms :TITLE title
-- >                     :NODE-STRING node-string
-- >                     :DEBUGGING debugging
-- >                     :ENQUEUE-PROCEDURE enqueue-procedure)))
-- >     (setf (atms-contra-node atms)
-- >      (tms-create-node atms "The contradiction"
-- >                       :CONTRADICTORYP t))
-- >     (setf (atms-empty-env atms) (create-env atms nil))
-- >     atms))
createATMS :: Monad m => String -> ATMST s m (ATMS d i r s m)
createATMS title = do
  AtmsT $ lift $ lift $ do
    nc <- newSTRef 0
    jc <- newSTRef 0
    ec <- newSTRef 0
    nodes <- newSTRef ([] :: [Node d i r s m])
    justs <- newSTRef ([] :: [JustRule d i r s m])
    contradictions <- newSTRef ([] :: [Node d i r s m])
    assumptions <- newSTRef ([] :: [Node d i r s m])
    nodeString <- newSTRef (show . nodeIndex)
    justString <- newSTRef (show . justIndex)
    datumString <- newSTRef (\ datum -> "?")
    informantString <- newSTRef (\ inf -> "?")
    enqueueProcedure <- newSTRef (\ _ -> return ())
    debugging <- newSTRef False
    return (ATMS title nc jc ec nodes justs contradictions assumptions
             nodeString justString datumString informantString
             enqueueProcedure debugging)


-- > ;; In atms.lisp
-- > (defun change-atms (atms &key node-string
-- >                          enqueue-procedure debugging)
-- >   (if node-string (setf (atms-node-string atms) node-string))
-- >   (if debugging (setf (atms-debugging atms) debugging))
-- >   (if enqueue-procedure
-- >       (setf (atms-enqueue-procedure atms) enqueue-procedure)))

-- > ;; In atms.lisp
-- > (defun true-node? (node)
-- >   (eq (car (tms-node-label node))
-- >       (atms-empty-env (tms-node-atms node))))
isTrueNode :: Monad m => Node d i r s m -> ATMST s m Bool
isTrueNode = error "< TODO unimplemented isTrueNode >"

-- > ;; In atms.lisp
-- > (defun in-node? (n &optional env)
-- >   (if env
-- >       (some #'(lambda (le) (subset-env? le env))
-- >        (tms-node-label n))
-- >       (not (null (tms-node-label n)))))
isInNode :: Monad m => Node d i r s m -> Env d i r s m -> ATMST s m Bool
isInNode = error "< TODO unimplemented isInNode >"

-- > ;; In atms.lisp
-- > (defun out-node? (n env) (not (in-node? n env)))
isOutNode :: Monad m => Node d i r s m -> Env d i r s m -> ATMST s m Bool
isOutNode = error "< TODO unimplemented isOutNode >"

-- > ;; In atms.lisp
-- > (defun node-consistent-with? (n env)
-- >   (some #'(lambda (le) (not (env-nogood? (union-env le env))))
-- >    (tms-node-label n)))
isNodeConsistentWith :: Monad m => Node d i r s m -> Env d i r s m -> ATMST s m Bool
isNodeConsistentWith = error "< TODO unimplemented isNodeConsistentWith >"

-- > ;; In atms.lisp
-- > (defun tms-create-node (atms datum &key assumptionp contradictoryp
-- >                               &aux node)
-- >   (setq node (make-tms-node :INDEX (incf (atms-node-counter atms))
-- >                        :DATUM datum
-- >                        :ASSUMPTION? assumptionp
-- >                        :CONTRADICTORY? contradictoryp
-- >                        :ATMS atms))
-- >   (push node (atms-nodes atms))
-- >   (if contradictoryp (push node (atms-contradictions atms)))
-- >   (when assumptionp
-- >     (push node (atms-assumptions atms))
-- >     (push (create-env atms (list node)) (tms-node-label node)))
-- >   node)
createNode ::
  Monad m => ATMS d i r s m -> d -> Bool -> Bool -> ATMST s m (Node d i r s m)
createNode atms datum isAssumption isContradictory =
  error "< TODO unimplemented createNode >"

-- > ;; In atms.lisp
-- > (defun assume-node (node &aux atms)
-- >   (unless (tms-node-assumption? node)
-- >     (setq atms (tms-node-atms node))
-- >     (debugging atms  "~%Converting ~A into an assumption" node)
-- >     (setf (tms-node-assumption? node) t)
-- >     (push node (atms-assumptions atms))
-- >     (update (list (create-env atms (list node)))
-- >        node
-- >        'ASSUME-NODE)))
assumeNode :: Monad m => Node d i r s m -> ATMST s m ()
assumeNode = error "< TODO unimplemented assumeNode >"

-- > ;; In atms.lisp
-- > (defun make-contradiction
-- >        (node &aux (atms (tms-node-atms node)) nogood)
-- >   (unless (tms-node-contradictory? node)
-- >     (setf (tms-node-contradictory? node) t)
-- >     (push node (atms-contradictions atms))
-- >     (do nil (nil)
-- >       (if (setq nogood (car (tms-node-label node)))
-- >      (new-nogood atms nogood 'MAKE-CONTRADICTION)
-- >      (return nil)))))
makeContradiction :: Monad m => Node d i r s m -> ATMST s m ()
makeContradiction = error "< TODO unimplemented makeContradiction >"

-- > (defun justify-node (informant consequence antecedents &aux just atms)
-- >   (setq atms (tms-node-atms consequence)
-- >    just (make-just :INDEX (incf (atms-just-counter atms))
-- >                    :INFORMANT informant
-- >                    :CONSEQUENCE consequence
-- >                    :ANTECEDENTS antecedents))
-- >   (push just (tms-node-justs consequence))
-- >   (dolist (node antecedents) (push just (tms-node-consequences node)))
-- >   (push just (atms-justs atms))
-- >   (debugging atms
-- >         "~%Justifying ~A in terms of ~A on ~A"
-- >         consequence
-- >         informant
-- >         (mapcar #'node-string antecedents))
-- >   (propagate just nil (list (atms-empty-env atms)))
-- >   just)
justifyNode :: Monad m => ATMS d i r s m -> i -> Node d i r s m -> [Node d i r s m] -> ATMST s m ()
justifyNode = error "< TODO unimplemented justifyNode >"

-- > ;; In atms.lisp
-- > (defun nogood-nodes (informant nodes)
-- >   (justify-node informant
-- >            (atms-contra-node (tms-node-atms (car nodes)))
-- >            nodes))
nogoodNodes :: Monad m => Node d i r s m -> [Node d i r s m] -> ATMST s m ()
nogoodNodes = error "< TODO unimplemented nogoodNodes >"

-- * Label updating

-- > ;; In atms.lisp
-- > (defun propagate (just antecedent envs &aux new-envs)
-- >   (if (setq new-envs (weave antecedent envs (just-antecedents just)))
-- >       (update new-envs (just-consequence just) just)))
propagate :: Monad m => JustRule d i r s m -> Node d i r s m -> [Env d i r s m] -> ATMST s m ()
propagate = error "< TODO unimplemented propagate >"

-- > ;; In atms.lisp
-- > (defun update (new-envs consequence just &aux atms enqueuef)
-- >   (setq atms (tms-node-atms consequence))
-- >   (when (tms-node-contradictory? consequence)
-- >     (dolist (env new-envs) (new-nogood atms env just))
-- >     (return-from update nil))
-- >   (setq new-envs (update-label consequence new-envs))
-- >   (unless new-envs (return-from update nil))
-- >   (when (setq enqueuef (atms-enqueue-procedure atms))
-- >     (dolist (rule (tms-node-rules consequence))
-- >       (funcall enqueuef rule))
-- >     (setf (tms-node-rules consequence) nil))
-- >   (dolist (supported-just (tms-node-consequences consequence))
-- >     (propagate supported-just consequence new-envs)
-- >   (do ((new-envs new-envs (cdr new-envs)))
-- >       ((null new-envs))
-- >     (unless (member (car new-envs) (tms-node-label consequence))
-- >       (rplaca new-envs nil)))
-- >   (setq new-envs (delete nil new-envs :TEST #'eq))
-- >   (unless new-envs (return-from update nil))))
update :: Monad m => [Env d i r s m] -> Node d i r s m -> JustRule d i r s m -> ATMST s m ()
update = error "< TODO unimplemented update >"

-- |Internal method to update the label of this node to include the
-- given environments.  The inclusion is not simply list extension;
-- new environments subsumed by an existing label environment will be
-- omitted, and existing label environments subsumed by a new
-- environment will be removed.
--
-- > ;; In atms.lisp
-- > (defun update-label (node new-envs &aux envs)
-- >   (setq envs (tms-node-label node))
-- >   (do ((new-envs new-envs (cdr new-envs)))
-- >       ((null new-envs))
-- >     (do ((nenvs envs (cdr nenvs)))
-- >    ((null nenvs) (push (car new-envs) envs))
-- >       (cond ((null (car nenvs)))
-- >        ((null (car new-envs)))
-- >        ((case (compare-env (car new-envs) (car nenvs))
-- >           ((:EQ :S21) (rplaca new-envs nil))
-- >           (:S12 (setf (env-nodes (car nenvs))
-- >                       (delete node (env-nodes (car nenvs))
-- >                               :COUNT 1))
-- >                 (rplaca nenvs nil)))))))
-- >   (setq new-envs (delete nil new-envs :TEST #'eq))
-- >   (dolist (new-env new-envs) (push node (env-nodes new-env)))
-- >   (setf (tms-node-label node) (delete nil envs :TEST #'eq))
-- >   new-envs)
updateLabel :: Monad m => Node d i r s m -> [Env d i r s m] -> ATMST s m [Env d i r s m]
updateLabel = error "< TODO unimplemented updateLabel >"

-- > ;; In atms.lisp
-- > (defun weave (antecedent envs antecedents &aux new-envs new-env)
-- >   (setq envs (copy-list envs))
-- >   (dolist (node antecedents)
-- >     (unless (eq node antecedent)
-- >       (setq new-envs nil)
-- >       (dolist (env envs)
-- >    (if env
-- >        (dolist (node-env (tms-node-label node))
-- >          (setq new-env (union-env env node-env))
-- >          (unless (env-nogood? new-env)
-- >            (do ((nnew-envs new-envs (cdr nnew-envs)))
-- >                ((null nnew-envs) (push new-env new-envs))
-- >              (when (car nnew-envs)
-- >                (case (compare-env new-env (car nnew-envs))
-- >                  ((:EQ :S21) (return nil))
-- >                  (:S12 (rplaca nnew-envs nil)))))))))
-- >       (setq envs (delete nil new-envs :TEST #'eq))
-- >       (unless envs (return-from weave nil))))
-- >   envs)
weave :: Monad m => Node d i r s m -> [Env d i r s m] -> [Node d i r s m] -> ATMST s m [Env d i r s m]
weave = error "< TODO unimplemented weave >"

-- > ;; In atms.lisp
-- > (defun in-antecedent? (nodes)
-- >   (or (null nodes)
-- >       (weave? (atms-empty-env (tms-node-atms (car nodes))) nodes)))
isInAntecedent :: Monad m => [Node d i r s m] -> ATMST s m Bool
isInAntecedent = error "< TODO unimplemented isInAntecedent >"

-- > ;; In atms.lisp
-- > (defun weave? (env nodes &aux new-env)
-- >   (cond ((null nodes) t)
-- >    (t (dolist (e (tms-node-label (car nodes)))
-- >         (setq new-env (union-env e env))
-- >         (unless (env-nogood? new-env)
-- >           (if (weave? new-env (cdr nodes))
-- >               (return T)))))))
isWeave :: Monad m => Env d i r s m -> [Node d i r s m] -> ATMST s m Bool
isWeave = error "< TODO unimplemented isInAntecedent >"

-- > ;; In atms.lisp
-- > (defun supporting-antecedent? (nodes env)
-- >   (dolist (node nodes t) (unless (in-node? node env) (return nil))))
isSupportingAntecedent :: Monad m => [Node d i r s m] -> Env d i r s m -> ATMST s m Bool
isSupportingAntecedent = error "< TODO unimplemented isSupportingAntecedent >"

-- > ;; In atms.lisp
-- > (defun remove-node (node &aux atms)
-- >   (if (tms-node-consequences node)
-- >       (error "Can't remove node with consequences"))
-- >   (setq atms (tms-node-atms node))
-- >   (setf (atms-nodes atms)
-- >    (delete node (atms-nodes atms) :test #'eq :count 1))
-- >   (dolist (just (tms-node-justs node))
-- >     (dolist (ant (just-antecedents just))
-- >       (setf (tms-node-consequences ant)
-- >        (delete just (tms-node-consequences ant)
-- >                :test #'eq :count 1))))
-- >   (dolist (env (tms-node-label node))
-- >     (setf (env-nodes env)
-- >      (delete node (env-nodes env) :test #'eq :count 1))))
removeNode :: Monad m => Node d i r s m -> ATMST s m ()
removeNode = error "< TODO unimplemented removeNode >"

-- * Creating and extending environments.

-- > ;; In atms.lisp
-- > (defun create-env (atms assumptions &aux e)
-- >   (setq e (make-env :INDEX (incf (atms-env-counter atms))
-- >                :ASSUMPTIONS assumptions
-- >                :COUNT (length assumptions)))
-- >   (setf (atms-env-table atms)
-- >    (insert-in-table (atms-env-table atms) e))
-- >   (set-env-contradictory atms e)
-- >   e)
createEnv :: Monad m => ATMS d i r s m -> [Node d i r s m] -> ATMST s m (Env d i r s m)
createEnv = error "< TODO unimplemented createEnv >"

-- > ;; In atms.lisp
-- > (defun union-env (e1 e2)
-- >   (when (> (env-count e1)
-- >       (env-count e2))
-- >     (psetq e1 e2 e2 e1))
-- >   (dolist (assume (env-assumptions e1))
-- >     (setq e2 (cons-env assume e2))
-- >     (if (env-nogood? e2) (return nil)))
-- >   e2)
unionEnv :: Monad m => Env d i r s m -> Env d i r s m -> ATMST s m (Env d i r s m)
unionEnv = error "< TODO unimplemented unionEnv >"

-- > ;; In atms.lisp
-- > (defun cons-env (assumption env &aux nassumes)
-- >   (setq nassumes (ordered-insert assumption
-- >                             (env-assumptions env)
-- >                             #'assumption-order))
-- >   (or (lookup-env nassumes)
-- >       (create-env (tms-node-atms assumption) nassumes)))
consEnv :: Monad m => Node d i r s m -> Env d i r s m -> ATMST s m (Env d i r s m)
consEnv = error "< TODO unimplemented consEnv >"

-- > ;; In atms.lisp
-- > (defun find-or-make-env (assumptions atms)
-- >   (unless assumptions
-- >     (return-from find-or-make-env (atms-empty-env atms)))
-- >   ;; Presumes the list of assumptions is ordered properly
-- >   (or (lookup-env assumptions)
-- >       (create-env atms assumptions)))
findOrMakeEnv :: Monad m => [Node d i r s m] -> ATMS d i r s m -> ATMST s m (Env d i r s m)
findOrMakeEnv = error "< TODO unimplemented findOrMakeEnv >"

-- * Env tables.

-- > ;; In atms.lisp
-- > (defun insert-in-table (table env &aux count entry)
-- >   (setq count (env-count env)
-- >    entry (assoc count table :TEST #'=))
-- >   (cond (entry (setf (cdr entry) (cons env (cdr entry))) table)
-- >    (t (ordered-insert
-- >         (list count env) table
-- >         #'(lambda (entry1 entry2)
-- >             (< (car entry1) (car entry2)))))))
insertInTable :: Monad m => EnvTable d i r s m -> Env d i r s m -> ATMST s m (EnvTable d i r s m)
insertInTable = error "< TODO unimplemented insertInTable >"

-- > ;; In atms.lisp
-- > (defun lookup-env (assumes)
-- >   (dolist (env (cdr (assoc (length assumes)
-- >                       (atms-env-table (tms-node-atms (car assumes)))
-- >                       :TEST #'=))
-- >           nil)
-- >     (if (equal (env-assumptions env) assumes)
-- >    (return env))))
lookupEnv :: Monad m => [Node d i r s m] -> ATMST s m (Env d i r s m)
lookupEnv = error "< TODO unimplemented lookupEnv >"

-- > ;; In atms.lisp
-- > (defun subset-env? (e1 e2)
-- >   (cond ((eq e1 e2) t)
-- >    ((> (env-count e1)
-- >        (env-count e2)) nil)
-- >    ((subsetp (env-assumptions e1)
-- >              (env-assumptions e2)))))
isSubsetEnv :: Monad m => Env d i r s m -> Env d i r s m -> Bool
isSubsetEnv = error "< TODO unimplemented isSubsetEnv >"

-- > ;; In atms.lisp
-- > (defun compare-env (e1 e2)
-- >   (cond ((eq e1 e2) :EQ)
-- >    ((< (env-count e1) (env-count e2))
-- >     (if (subsetp (env-assumptions e1)
-- >                  (env-assumptions e2))
-- >         :S12))
-- >    ((subsetp (env-assumptions e2) (env-assumptions e1))
-- >     :S21)))
compareEnv :: Monad m => Env d i r s m -> Env d i r s m -> ATMST s m (Env d i r s m)
compareEnv = error "< TODO unimplemented compareEnv >"

-- * Processing nogoods

-- > ;; In atms.lisp
-- > (defun new-nogood (atms cenv just &aux count)
-- >   (debugging atms (format nil "~%  ~A new minimal nogood." cenv))
-- >   (setf (env-nogood? cenv) just)
-- >   (remove-env-from-labels cenv atms)
-- >   (setf (atms-nogood-table atms)
-- >    (insert-in-table (atms-nogood-table atms) cenv))
-- >   (setq count (env-count cenv))
-- >   (dolist (entry (atms-nogood-table atms))
-- >     (when (> (car entry) count)
-- >       (dolist (old (cdr entry))
-- >    (if (subset-env? cenv old)
-- >        (setf (cdr entry) (delete old (cdr entry) :COUNT 1))))))
-- >   (dolist (entry (atms-env-table atms))
-- >     (when (> (car entry) count)
-- >       (dolist (old (cdr entry))
-- >    (when (and (not (env-nogood? old))
-- >               (subset-env? cenv old))
-- >      (setf (env-nogood? old) cenv)
-- >      (remove-env-from-labels old atms))))))
newNogood :: Monad m => ATMS d i r s m -> Env d i r s m -> Justification d i r s m -> ATMST s m ()
newNogood = error "< TODO unimplemented newNogood >"

-- > ;; In atms.lisp
-- > (defun set-env-contradictory (atms env &aux count)
-- >   (cond ((env-nogood? env) t)
-- >    (t (setq count (env-count env))
-- >       (dolist (entry (atms-nogood-table atms))
-- >         (cond ((> (car entry) count)
-- >                (return nil))
-- >               (t (dolist (cenv (cdr entry))
-- >                    (when (subset-env? cenv env)
-- >                      (setf (env-nogood? env)
-- >                            cenv)
-- >                      (return t)))))))))
setEnvContradictory :: Monad m => ATMS d i r s m -> Env d i r s m -> ATMST s m ()
setEnvContradictory = error "< TODO unimplemented setEnvContradictory >"

-- > ;; In atms.lisp
-- > (defun remove-env-from-labels (env atms &aux enqueuef)
-- >   (when (setq enqueuef (atms-enqueue-procedure atms))
-- >     (dolist (rule (env-rules env))
-- >       (funcall enqueuef rule))
-- >     (setf (env-rules env) nil))
-- >   (dolist (node (env-nodes env))
-- >     (setf (tms-node-label node)
-- >      (delete env (tms-node-label node) :COUNT 1))))
removeEnvFromLabels :: Monad m => Env d i r s m -> ATMS d i r s m -> ATMST s m ()
removeEnvFromLabels = error "< TODO unimplemented removeEnvFromLabels >"

-- * Interpretation construction

-- > ;; In atms.lisp
-- > (proclaim '(special *solutions*))
-- > (defun interpretations (atms choice-sets &optional defaults
-- >                    &aux solutions)
-- >   (if (atms-debugging atms)
-- >    (format *trace-output*
-- >       "~%Constructing interpretations depth-first for ~a:" choice-sets))
-- >   (format *trace-output* "~%- Refining choice sets")
-- >   (let ((*solutions* nil)
-- >    (choice-sets
-- >      (mapcar #'(lambda (alt-set)
-- >                  (format *trace-output*
-- >                      "~%  - ~a --> ???" alt-set)
-- >                  (let ((result
-- >                         (mapcan #'(lambda (alt)
-- >                                     (format *trace-output*
-- >                                         "~%    - ~a --> ~a" alt (tms-node-label alt))
-- >                                     (copy-list (tms-node-label alt)))
-- >                                 alt-set)))
-- >                    (format *trace-output*
-- >                        "~%    ~a --> ~a" alt-set result)
-- >                    result))
-- >              choice-sets)))
-- >     (format *trace-output* "~%  Refined choice sets to ~a" choice-sets)
-- >     (dolist (choice (car choice-sets))
-- >       (format *trace-output*
-- >      "~%- Calling depth-solutions with choice ~a" choice)
-- >       (format *trace-output*
-- >      "~%                               choice sets ~a" (car choice-sets))
-- >       (get-depth-solutions1 choice (cdr choice-sets))
-- >       (format *trace-output*
-- >      "~%      => solutions ~a" *solutions*))
-- >     (setq *solutions* (delete nil *solutions* :TEST #'eq))
-- >     (unless *solutions*
-- >       (if choice-sets (return-from interpretations nil)
-- >                  (setq *solutions* (list (atms-empty-env atms)))))
-- >     (when defaults
-- >       (setq solutions *solutions* *solutions* nil)
-- >       (dolist (solution solutions)
-- >    (extend-via-defaults solution defaults defaults)))
-- >     (delete nil *solutions* :TEST #'eq)))
interpretations :: Monad m => ATMS d i r s m -> [[Node d i r s m]] -> ATMST s m ()
interpretations = error "< TODO unimplemented interpretations >"

-- > ;; In atms.lisp
-- > (defun get-depth-solutions1 (solution choice-sets
-- >                                  &aux new-solution)
-- >   (cond ((null choice-sets)
-- >     (unless (do ((old-solutions *solutions* (cdr old-solutions)))
-- >                 ((null old-solutions))
-- >               (when (car old-solutions)
-- >                 (case (compare-env (car old-solutions) solution)
-- >                   ((:EQ :S12) (return t))
-- >                   (:S21 (rplaca old-solutions nil)))))
-- >       (push solution *solutions*)))
-- >    ((env-nogood? solution)) ;something died.
-- >    (t (dolist (choice (car choice-sets))
-- >         (setq new-solution (union-env solution choice))
-- >         (unless (env-nogood? new-solution)
-- >           (get-depth-solutions1 new-solution
-- >                                 (cdr choice-sets)))))))
getDepthSolutions1 :: Monad m => Env d i r s m -> [[Env d i r s m]] -> ATMST s m ()
getDepthSolutions1 = error "< TODO unimplemented getDepthSolutions1 >"

-- > ;; In atms.lisp
-- > (defun extend-via-defaults (solution remaining original)
-- >   (do ((new-solution)
-- >        (defaults remaining (cdr defaults)))
-- >       ((null defaults)
-- >        (or (member solution *solutions* :TEST #'eq)
-- >       (dolist (default original)
-- >         (or (member default (env-assumptions solution)
-- >                     :TEST #'eq)
-- >             (env-nogood? (cons-env default solution))
-- >             (return t)))
-- >       (push solution *solutions*)))
-- >     (setq new-solution (cons-env (car defaults) solution))
-- >     (unless (env-nogood? new-solution)
-- >       (extend-via-defaults new-solution (cdr defaults) original))))
extendViaDefaults :: Monad m => Env d i r s m -> [Node d i r s m] -> [Node d i r s m] -> ATMST s m ()
extendViaDefaults = error "< TODO unimplemented extendViaDefaults >"

-- * Generating explanations

-- This returns a list of justifications which form a DAG for the
-- derivation. This is quite complicated because this is really a
-- simple consequent JTMS.

-- > ;; In atms.lisp
-- > (defun explain-node (node env) (explain-node-1 env node nil nil))
explainNode :: Monad m => Node d i r s m -> Env d i r s m -> ATMST s m [Justification d i r s m]
explainNode = error "< TODO unimplemented explainNode >"

-- > ;; In atms.lisp
-- > (defun explain-node-1 (env node queued-nodes explanation)
-- >   (cond ((member node queued-nodes) nil)
-- >    ((and (tms-node-assumption? node)
-- >          (member node (env-assumptions env)))
-- >     (cons (cons 'ASSUME node) explanation))
-- >    ((dolist (just explanation)
-- >       (if (if (listp just)
-- >               (eq (cdr just) node) (eq (just-consequence just) node))
-- >           (return explanation))))
-- >    (t (setq queued-nodes (cons node queued-nodes))
-- >       (dolist (just (tms-node-justs node))
-- >         (unless (dolist (a (just-antecedents just))
-- >                   (unless (in-node? a env) (return t)))
-- >          (let ((new-explanation explanation))
-- >            (dolist (a (just-antecedents just)
-- >                       (return-from explain-node-1 (cons just new-explanation)))
-- >              (setq new-explanation
-- >                    (explain-node-1 env a queued-nodes new-explanation))
-- >              (unless new-explanation (return nil)))))))))
explainNode1 :: Monad m => Env d i r s m -> Node d i r s m -> [Node d i r s m] -> [Justification d i r s m] -> ATMST s m [Explanation d i r s m]
explainNode1 = error "< TODO unimplemented explainNode1 >"

-- > ;;; Printing
-- > (defun why-node (node &optional (stream t) (prefix ""))
-- >   (format stream "~%<~A~A,{" prefix (tms-node-datum node))
-- >   (dolist (e (tms-node-label node))
-- >     (env-string e stream))
-- >   (format stream "}>"))
whyNode :: MonadIO m => Node d i r s m -> ATMST s m (Node d i r s m)
whyNode = error "< TODO unimplemented whyNode >"

-- > ;; In atms.lisp
-- > (defun why-nodes (atms &optional (stream t))
-- >   (dolist (n (reverse (atms-nodes atms))) (why-node n stream)))
whyNodes :: MonadIO m => ATMS d i r s m -> ATMST s m ()
whyNodes = error "< TODO unimplemented whyNodes >"

-- > ;; In atms.lisp
-- > (defun node-justifications (node &optional (stream t))
-- >   (format t "~% For ~A:" (node-string node))
-- >   (dolist (j (tms-node-justs node))
-- >     (print-justification j stream)))
nodeJustifications :: Monad m => Node d i r s m -> ATMST s m ()
nodeJustifications = error "< TODO unimplemented nodeJustifications >"

-- > ;; In atms.lisp
-- > (defun print-justification (j &optional (stream t))
-- >   (format stream "~%  ~A, " (just-informant j))
-- >   (dolist (a (just-antecedents j))
-- >     (why-node a stream "     ")))
printJustification :: Monad m => Justification d i r s m -> ATMST s m ()
printJustification = error "< TODO unimplemented printJustification >"

-- > ;; In atms.lisp
-- > (defun e (atms n)
-- >   (dolist (bucket (atms-env-table atms))
-- >     (dolist (env (cdr bucket))
-- >     (if (= (env-index env) n) (return-from e env)))))
e :: Monad m => ATMS d i r s m -> Int -> ATMST s m ()
e = error "< TODO unimplemented e >"

-- > ;; In atms.lisp
-- > (defun print-env (e &optional (stream t))
-- >   (format stream "~%~A:~A"
-- >      e (if (env-nogood? e)
-- >            "* " " "))
-- >   (env-string e stream))
printEnv :: MonadIO m => Env d i r s m -> ATMST s m ()
printEnv = error "< TODO unimplemented printEnv >"

-- > ;; In atms.lisp
-- > (defun env-string (e &optional stream
-- >                      &aux assumptions strings printer)
-- >   (setq assumptions (env-assumptions e))
-- >   (when assumptions
-- >     (setq printer (atms-node-string (tms-node-atms (car assumptions)))))
-- >   (dolist (a assumptions) (push (funcall printer a) strings))
-- >   (format stream "{~{~A~^,~}}" (sort strings #'string-lessp)))
envString :: Monad m => Env d i r s m -> ATMST s m String
envString = error "< TODO unimplemented envString >"

-- * Printing global data

-- > ;; In atms.lisp
-- > (defun print-nogoods (atms &optional (stream t))
-- >   (print-env-table (atms-nogood-table atms) stream))
printNogoods :: MonadIO m => ATMS d i r s m -> ATMST s m ()
printNogoods = error "< TODO unimplemented printNogoods >"

-- > ;; In atms.lisp
-- > (defun print-envs (atms &optional (stream t))
-- >   (print-env-table (atms-env-table atms) stream))
printEnvs :: MonadIO m => ATMS d i r s m -> ATMST s m ()
printEnvs = error "< TODO unimplemented printEnvs >"

-- > ;; In atms.lisp
-- > (defun print-env-table (table stream)
-- >   (dolist (bucket table)
-- >     (dolist (env (cdr bucket))
-- >       (print-env env stream))))
printEnvTable :: MonadIO m => EnvTable d i r s m -> ATMST s m ()
printEnvTable = error "< TODO unimplemented printEnvTable >"

-- > ;; In atms.lisp
-- > (defun print-atms-statistics (atms)
-- >   (print-table "~% For env table:" (atms-env-table atms))
-- >   (print-table "~% For nogood table:" (atms-nogood-table atms)))
printAtmsStatistics :: MonadIO m => ATMS d i r s m -> ATMST s m ()
printAtmsStatistics = error "< TODO unimplemented printAtmsStatistics >"

-- > ;; In atms.lisp
-- > (defun print-table (msg table)
-- >   (format t msg)
-- >   (dolist (entry table)
-- >     (format t "~%   Length ~D, ~D" (car entry)
-- >        (length (cdr entry)))))
printTable :: MonadIO m => String -> EnvTable d i r s m -> ATMST s m ()
printTable = error "< TODO unimplemented printTable >"

