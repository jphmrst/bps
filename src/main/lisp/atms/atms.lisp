;;; -*- Mode: LISP; Syntax: Common-lisp; -*-

;;; Assumption-based truth maintenance system, version 61 of 7/21/92.

;;; Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.
 
;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;; Definitions.

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

(defun print-atms (atms stream ignore)
  (declare (ignore ignore))
  (format stream "#<ATMS: ~A>" (atms-title atms)))

(defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
  (index 0)     					;; Unique name.
  (datum nil)                   ; Pointer to IE data structures.
  (label nil)                   ; minimal envs believed under
  (justs nil)                   ; providers of support
  (consequences nil)            ; provides support for.
  (contradictory? nil)          ; flag marking it as contradictory.
  (assumption? nil)             ; flag marking it as n assumption.
  (rules nil)                   ; run when label non-empty.
  (atms nil))
			   
(defun print-tms-node (node stream ignore)
  (declare (ignore ignore))
  (if (tms-node-assumption? node)
      (format stream "A-~D" (tms-node-index node))
      (format stream "#<NODE: ~A>" (node-string node))))

(defstruct (just (:PRINT-FUNCTION print-just))
	   (index 0)
	   (informant nil)
	   (consequence nil)
	   (antecedents nil))

(defun print-just (just stream ignore)
  (declare (ignore ignore))
  (format stream "<~A ~D>" (just-informant just)
	  (just-index just)))

(defstruct (env (:PREDICATE env?)
		(:PRINT-FUNCTION print-env-structure))
	   (index 0)
	   (count 0)				; Number of assumptions.
	   (assumptions nil)
	   (nodes nil)
	   (nogood? nil)
	   (rules nil))				; Call this if becomes nogood.

(defun print-env-structure (env stream ignore)
  (declare (ignore ignore))
  (format stream "E-~D" (env-index env)))

(defun node-string (node)
  (funcall (atms-node-string (tms-node-atms node)) node))

(defmacro debugging (atms msg &optional node &rest args)
  `(when (atms-debugging ,atms)
     (trdbg:trdbg (:atms 1) ,msg (if ,node (node-string ,node)) ,@args)))

(defun default-node-string (n) (format nil "~A" (tms-node-datum n)))

(defun ordered-insert (item list test)
  (cond ((null list) (list item))
	((funcall test item (car list)) (cons item list))
	((eq item (car list)) list) 
	(t (cons (car list) (ordered-insert item (cdr list) test)))))

(defmacro ordered-push (item list test)
  `(setq ,list (ordered-insert ,item ,list ,test)))

(defun assumption-order (a1 a2)
  (< (tms-node-index a1) (tms-node-index a2)))

(defun env-order (e1 e2)
  (< (env-index e1) (env-index e2)))


;;; Basic inference engine interface.

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

(defun true-node? (node)
  (eq (car (tms-node-label node))
      (atms-empty-env (tms-node-atms node))))

(defun in-node? (n &optional env)
  (if env
      (some #'(lambda (le) (subset-env? le env))
	    (tms-node-label n))
      (not (null (tms-node-label n)))))

(defun out-node? (n env) (not (in-node? n env)))

(defun node-consistent-with? (n env)
  (some #'(lambda (le) (not (env-nogood? (union-env le env))))
	(tms-node-label n)))				    

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


(defun assume-node (node &aux atms)
  "Mark the given NODE as to be believed as an assumption by its ATMS."
  (unless (tms-node-assumption? node)
    (setq atms (tms-node-atms node))
    (debugging atms  "~%Converting ~A into an assumption" node)
    (setf (tms-node-assumption? node) t)
    (push node (atms-assumptions atms))
    (update (list (create-env atms (list node)))
	    node
	    'ASSUME-NODE)))

(defun make-contradiction
    (node &aux (atms (tms-node-atms node)) nogood)
  "Mark the given NODE as an additional contradiction node of its ATMS."
  (unless (tms-node-contradictory? node)
    (setf (tms-node-contradictory? node) t)
    (push node (atms-contradictions atms))
    (do nil (nil)
      (if (setq nogood (car (tms-node-label node)))
	  (new-nogood atms nogood 'MAKE-CONTRADICTION)
	  (return nil)))))
  
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

(defun nogood-nodes (informant nodes)
  (justify-node informant
		(atms-contra-node (tms-node-atms (car nodes)))
		nodes))

;;; Label updating

(defun propagate (just antecedent envs &aux new-envs)
  (if (setq new-envs (weave antecedent envs (just-antecedents just)))
      (update new-envs (just-consequence just) just)))

(defun update (new-envs consequence just &aux atms enqueuef)
  (setq atms (tms-node-atms consequence))

  ;; If the consequence node is a contradiction, then all we need to
  ;; do is mark all of the environments implying it as contradictory
  ;; as well.
  (when (tms-node-contradictory? consequence)
    (dolist (env new-envs) (new-nogood atms env just))
    (return-from update nil))

  ;; Otherwise we prepare to propagate further, but if this
  ;; step prunes out all `Env`s from the `newEnvs`, then we
  ;; have nothing further to do.
  (setq new-envs (update-label consequence new-envs))
  (unless new-envs (return-from update nil))

  ;; Process rules queued in the consequence.
  (when (setq enqueuef (atms-enqueue-procedure atms))
    (dolist (rule (tms-node-rules consequence))
      (funcall enqueuef rule))
    (setf (tms-node-rules consequence) nil))

  ;; Propagate to the justification rules which might depend on
  ;; this node.
  (dolist (supported-just (tms-node-consequences consequence))
    (propagate supported-just consequence new-envs)
  (do ((new-envs new-envs (cdr new-envs)))
      ((null new-envs))
    (unless (member (car new-envs) (tms-node-label consequence))
      (rplaca new-envs nil)))
  (setq new-envs (delete nil new-envs :TEST #'eq))
  (unless new-envs (return-from update nil))))

(defun update-label (node new-envs &aux envs)
  "Internal method to update the label of this node to include the given environments.  The inclusion is not simply list extension; new environments subsumed by an existing label environment will be omitted, and existing label environments subsumed by a new environment will be removed."
  (setq envs (tms-node-label node))
  (do ((new-envs new-envs (cdr new-envs)))
      ((null new-envs))
    (do ((nenvs envs (cdr nenvs)))
	((null nenvs) (push (car new-envs) envs))
      (cond ((null (car nenvs)))
	    ((null (car new-envs)))
	    ((case (compare-env (car new-envs) (car nenvs))
	       ((:EQ :S21) (rplaca new-envs nil))
	       (:S12 (setf (env-nodes (car nenvs))
			   (delete node (env-nodes (car nenvs))
				   :COUNT 1))
		     (rplaca nenvs nil)
		     ;; Note that at the exit from the inner DO-loop,
		     ;; the exit statement will push the car of the
		     ;; NEW-ENVS scanner (which may be NULL) onto
		     ;; ENVS.
		     ))))))
  (setq new-envs (delete nil new-envs :TEST #'eq))
  (dolist (new-env new-envs) (push node (env-nodes new-env)))
  (setf (tms-node-label node) (delete nil envs :TEST #'eq))
  new-envs)

(defun weave (antecedent envs antecedents &aux new-envs new-env)
  "Update the label of node ANTECEDENT to include the given ENVS environments, pruning environments which are a superset of another included enviroment.

Implements Algorithm 12.3 of /Building Problem Solvers/."

  (setq envs (copy-list envs))
  (dolist (node antecedents)
    (unless (eq node antecedent)

      ;; We will update ENVS with the list built in NEW-ENVS.
      (setq new-envs nil)
      
      ;; We look at all pairs of
      ;;  - An Env from the passed-in ENVS, plus
      ;;  - An Env from the NODE's label.      
      ;; The union of these two is NEW-ENV, and the body of the loop
      ;; considers how we should incorporate NEW-ENV into NEW-ENVS.
      (dolist (env envs)
	(if env
	  (dolist (node-env (tms-node-label node))
	    (setq new-env (union-env env node-env))
	    (unless (env-nogood? new-env)
	      
	      ;; If NEW-ENV is a superset of (or is equal to) anything
	      ;; already in NEW-ENVS, then NEW-ENV is redundant, and
	      ;; we abort the body of the inner match-searching loop
	      ;; without adding NEW-ENV to NEW-ENVS.
	      
	      ;; Otherwise if anything already in NEW-ENVS is a
	      ;; superset of NEW-ENV, then (1) NEW-ENV makes that
	      ;; element redundant, and we strip it out of NEW-ENVS;
	      ;; and (2) we add NEW-ENV to NEW-ENVS.
	      (do ((nnew-envs new-envs (cdr nnew-envs)))
		  ((null nnew-envs) (push new-env new-envs))
		(when (car nnew-envs)
		  (case (compare-env new-env (car nnew-envs))
		    ((:EQ :S21) (return nil))
		    (:S12 (rplaca nnew-envs nil))
					; Could also be NIL, for
					; mutually non-contained sets
					; --- ignored.
		    ))) ;; End of DO-macro.
	      
	      ;; Note that at this point the exit condition of the DO
	      ;; will have added NEW-ENV to the NEW-ENVS list.
	      
	      ))))

      ;; So we have nearly produced the refinement of ENVS for this
      ;; NODE in the ANTECEDENTS.  It might have spurious NILs, so we
      ;; strip those out and update ENVS.  If ever we narrow ENVS down
      ;; to nothing, then we can short- circuit returning that empty
      ;; list.
      (setq envs (delete nil new-envs :TEST #'eq))
      (unless envs (return-from weave nil))))

  ;; Finally, return the last refinement of ENVS.
  envs)

(defun in-antecedent? (nodes)
  (or (null nodes)
      (weave? (atms-empty-env (tms-node-atms (car nodes))) nodes)))
  
(defun weave? (env nodes &aux new-env)
  "Check whether any union of antecedent environments is consistent."
  (cond ((null nodes) t)			
	(t (dolist (e (tms-node-label (car nodes)))
	     (setq new-env (union-env e env))
	     (unless (env-nogood? new-env)
	       (if (weave? new-env (cdr nodes))
		 ;; Returns from the DOLIST, which then is the result
		 ;; of WEAVE?.
		 (return T)))))))

(defun supporting-antecedent? (nodes env)
  (dolist (node nodes t) (unless (in-node? node env) (return nil))))


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

(defun create-env (atms assumptions &aux e)
  (setq e (make-env :INDEX (incf (atms-env-counter atms))
		    :ASSUMPTIONS assumptions
		    :COUNT (length assumptions)))
  (setf (atms-env-table atms)
	(insert-in-table (atms-env-table atms) e))
  (set-env-contradictory atms e)
  e)

(defun union-env (e1 e2)
  (trdbg:trblock (:union-env 1
		  :trace ("Called union-env with ~a ~a; ~a ~a"
			  e1 (get-env-string e1) e2 (get-env-string e2)))
    (when (> (env-count e1) (env-count e2))
      (psetq e1 e2 e2 e1))
    (dolist (assume (env-assumptions e1))
      (trdbg:trblock (:union-env 1
		      :trace ("Processing ~a" (tms-node-datum assume)))
	(setq e2 (cons-env assume e2))
	(trdbg:trdbg (:union-env 1) "Updated e2 to ~a ~a"
	  e2 (get-env-string e2))
	(when (env-nogood? e2)
	  (trdbg:trdbg (:union-env 1) "nogood")
	  (return nil))))
    (trdbg:trdbg (:union-env 1) "Result is ~a ~a" e2 (get-env-string e2))
    e2))

(defun cons-env (assumption env &aux nassumes)
  "Derive an environment from the addition of one additional assumption to a previous ENV's assumption list."
  (trdbg:trblock (:union-env 1
		  :trace ("Called cons-env with ~a; ~a ~a"
			  (tms-node-datum assumption)
			  env (get-env-string env)))
    (setq nassumes (ordered-insert assumption
				   (env-assumptions env)
				   #'assumption-order))
    (trdbg:trdbg (:union-env 1) "List after insertion: ~a" nassumes)
    (let ((lookup (lookup-env nassumes)))
      (trdbg:trdbg (:union-env 1) "Lookup gives: ~a" lookup)
      (or lookup
	  (create-env (tms-node-atms assumption) nassumes)))))

(defun find-or-make-env (assumptions atms)
  (unless assumptions
    (return-from find-or-make-env (atms-empty-env atms)))
  ;; Presumes the list of assumptions is ordered properly
  (or (lookup-env assumptions)
      (create-env atms assumptions)))

;;; Env tables.

(defun insert-in-table (table env &aux count entry)
  (setq count (env-count env)
	entry (assoc count table :TEST #'=))
  (cond (entry (setf (cdr entry) (cons env (cdr entry))) table)
	(t (ordered-insert
	     (list count env) table
	     #'(lambda (entry1 entry2)
		 (< (car entry1) (car entry2)))))))

(defun lookup-env (assumes)
  (dolist (env (cdr (assoc (length assumes)
			   (atms-env-table (tms-node-atms (car assumes)))
			   :TEST #'=))
	       nil) ;; Result of the loop if exited
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

(defun new-nogood (atms cenv just &aux count)
  (debugging atms (format nil "~%  ~A new minimal nogood." cenv))

  ;; Record the reason for deciding that cenv is nogood.
  (setf (env-nogood? cenv) just)

  ;; Remove the cenv from the labels of any nodes which reference it.
  (remove-env-from-labels cenv atms)

  ;; Add `cenv` to the table of nogoods.
  (setf (atms-nogood-table atms)
	(insert-in-table (atms-nogood-table atms) cenv))
  (setq count (env-count cenv))

  ;; Remove any nogood table entries made redundant by `cenv`.
  (dolist (entry (atms-nogood-table atms))
    (when (> (car entry) count)
      (dolist (old (cdr entry))
	(if (subset-env? cenv old)
	  (setf (cdr entry) (delete old (cdr entry) :COUNT 1))))))

  ;; Find currently-non-nogood environments which are supersets of the
  ;; nogood.  Mark each as a nogood, and remove it from node labels.
  (dolist (entry (atms-env-table atms))
    (when (> (car entry) count)
      (dolist (old (cdr entry))
	(when (and (not (env-nogood? old))
		   (subset-env? cenv old))
	  (setf (env-nogood? old) cenv)
	  (remove-env-from-labels old atms))))))

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

(defun remove-env-from-labels (env atms &aux enqueuef)
  (when (setq enqueuef (atms-enqueue-procedure atms))
    (dolist (rule (env-rules env))
      (funcall enqueuef rule))
    (setf (env-rules env) nil))
  (dolist (node (env-nodes env))
    (setf (tms-node-label node)
	  (delete env (tms-node-label node) :COUNT 1))))

;;; Interpretation construction

(defvar *solutions*)
;; (proclaim '(special *solutions*))

(defun interpretations (atms choice-sets &optional defaults
			&aux solutions)
  (trdbg:trblock (:interpretations 1
	    :trace ("Constructing interpretations depth-first for ~a:"
		    choice-sets))
    (let ((*solutions* nil)
	  (choice-sets
	   (trdbg:trblock (:interpretations 1
			   :trace ("Refining choice sets from ~a" choice-sets))
	     (mapcar #'(lambda (alt-set)
			 (trdbg:trblock (:interpretations 1
					 :trace ("~a --> ???" alt-set))
			   (let ((result
				  ;; Like MAPCAR, but passing the
				  ;; result to NCONC.
				  (mapcan #'(lambda (alt)
					      (trdbg:trdbg (:interpretations 1)
						  "Node ~a (~a) labelled ~a"
						alt (tms-node-datum alt)
						(tms-node-label alt))
					      (copy-list (tms-node-label alt)))
					  alt-set)))
			     (trdbg:trdbg (:interpretations 1)
				 "~a --> ~a" alt-set result)
			     result)))
		   choice-sets))))
      (trdbg:trdbg (:interpretations 1)
	  "Refined choice sets to ~a" choice-sets)
      (trdbg:trblock
	  (:interpretations 1 :trace "Iterating through choice sets")
	(dolist (choice (car choice-sets))
	  (trdbg:trblock
	      (:interpretations 1 :trace ("Considering choice set ~a" choice))
	    (get-depth-solutions1 choice (cdr choice-sets))
	    (trdbg:trdbg (:interpretations 1)
		"*solutions* now ~a" *solutions*))))
      (setq *solutions* (delete nil *solutions* :TEST #'eq))
      (unless *solutions*
	(if choice-sets (return-from interpretations nil)
	    (setq *solutions* (list (atms-empty-env atms)))))
      (when defaults
	(setq solutions *solutions* *solutions* nil)
	(dolist (solution solutions)
	  (extend-via-defaults solution defaults defaults)))
      (delete nil *solutions* :TEST #'eq))))

(defun get-depth-solutions1 (solution choice-sets
				      &aux new-solution)
  (trdbg:trblock (:interpretations 1
	    :trace ("Called depth-solutions with partial solution ~a ~a"
		    solution (get-env-string solution)))
    (trdbg:trdbg (:interpretations 1) "choice sets ~a" choice-sets)
    (trdbg:trdbg (:interpretations 1) "*solutions* now ~a" *solutions*)
    (cond ((null choice-sets)
	   (unless (trdbg:trblock
		       (:interpretations 1
			:trace ("Trying filter for ~a" solution))
		     (trdbg:trdbg (:interpretations 1) 
			 "*solutions* ~a" *solutions*)
		     (do ((old-solutions *solutions* (cdr old-solutions)))
			 ((null old-solutions))
		       (when (car old-solutions)
			 (trdbg:trblock
			     (:interpretations 1
			      :trace ("Checking ~a" (car old-solutions)))
			   (case (compare-env (car old-solutions) solution)
			     ((:EQ :S12)
			      (trdbg:trdbg (:interpretations 1)
				  "Redundant by ~a" (car old-solutions))
			      (return t))
			     (:S21 (trdbg:trdbg (:interpretations 1) 
				       "Filtering ~a" (car old-solutions))
				   (rplaca old-solutions nil))
			     (otherwise
			      (trdbg:trdbg (:interpretations 1) "OK")))))))
	     (trdbg:trdbg (:interpretations 1) "Adding ~a ~a"
	       solution (get-env-string solution))
	     (push solution *solutions*)
	     (trdbg:trdbg (:interpretations 1) "*solutions* now ~a"
	       *solutions*)))
	  ((env-nogood? solution)) ;something died.
	  (t (dolist (choice (car choice-sets))
	       (trdbg:trdbg (:interpretations 1) "depth-solution for ~a"
		 (get-env-string choice))
	       (setq new-solution (union-env solution choice))
	       (if (env-nogood? new-solution)
		 (trdbg:trdbg (:interpretations 1) "nogood ~a ~a"
		     new-solution (get-env-string new-solution))
		 (get-depth-solutions1 new-solution
				       (cdr choice-sets))))))))


(defun extend-via-defaults (solution remaining original)
  "Refine one solution to add as many given defaults as possible."
  (do ((new-solution)                   ; Set at the start of the body
					; of the loop.  So the value
					; does not communicate from
					; one iteration to another,
					; and note that it is not used
					; in the result expression.
       (defaults remaining (cdr defaults)))
      ((null defaults)
       ;; This big expression is the result value from the loop, given
       ;; the final values for the above loop variables.
       (or (member solution *solutions* :TEST #'eq)
	   (dolist (default original)
	     (or (member default (env-assumptions solution)
			 :TEST #'eq)
		 (env-nogood? (cons-env default solution))
                 ;; RETURN here exits the DOLIST.
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
	((and (tms-node-assumption? node)
	      (member node (env-assumptions env)))
	 (cons (cons 'ASSUME node) explanation))
	((dolist (just explanation)
	   (if (if (listp just)
		   (eq (cdr just) node) (eq (just-consequence just) node))
	       (return explanation))))
	(t (setq queued-nodes (cons node queued-nodes))
	   (dolist (just (tms-node-justs node))
	     (unless (dolist (a (just-antecedents just))
		       (unless (in-node? a env) (return t)))
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
  (format t "~% For ~A:" (node-string node))
  (dolist (j (tms-node-justs node))
    (print-justification j stream)))

(defun print-justification (j &optional (stream t))
  (format stream "~%  ~A, " (just-informant j))
  (dolist (a (just-antecedents j))
    (why-node a stream "     ")))

(defun e (atms n)
  (dolist (bucket (atms-env-table atms))
    (dolist (env (cdr bucket))
    (if (= (env-index env) n) (return-from e env)))))

(defun print-env (e &optional (stream t))
  (format stream "~%~A:~A"
	  e (if (env-nogood? e)
		"* " " "))
  (env-string e stream))

(defun get-env-string (e &aux assumptions strings printer)
  (setq assumptions (env-assumptions e))
  (when assumptions
    (setq printer (atms-node-string (tms-node-atms (car assumptions)))))
  (dolist (a assumptions) (push (funcall printer a) strings))
  (format nil "{~{~A~^,~}}" (sort strings #'string-lessp)))

(defun get-envs-string (es)
  (format nil "[~{~A~^; ~}]" (mapcar #'get-env-string es)))

(defun env-string (e &optional stream)
  (format stream (get-env-string e)))

;;; Printing global data

(defun print-nogoods (atms &optional (stream t))
  (print-env-table (atms-nogood-table atms) stream))

(defun print-envs (atms &optional (stream t))
  (print-env-table (atms-env-table atms) stream))

(defun print-env-table (table stream)
  (dolist (bucket table)
    (dolist (env (cdr bucket))
      (print-env env stream))))

(defun print-atms-statistics (atms)
  (print-table "~% For env table:" (atms-env-table atms))
  (print-table "~% For nogood table:" (atms-nogood-table atms)))

(defun print-table (msg table)
  (format t msg)
  (dolist (entry table)
    (format t "~%   Length ~D, ~D" (car entry)
	    (length (cdr entry)))))
