;;; -*- Mode: LISP; Syntax: Common-lisp; -*-

;;;; Test code
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(proclaim '(special a b c d e f))
(defvar *atms*)

(defun atms-test1 ()
  (setq *atms* (create-atms "atms-test1" :debugging T)
	a (tms-create-node *atms* "A")
	b (tms-create-node *atms* "B")
	c (tms-create-node *atms* "C")
	d (tms-create-node *atms* "D")
	e (tms-create-node *atms* "E")
	f (tms-create-node *atms* "F"))
  (assume-node a)
  (assume-node b)
  (assume-node c)
  (justify-node 'J1 d (list a b))
  (justify-node 'J2 e (list b c))
  (justify-node 'J3 f (list d e)))

(defun atms-test2 ()
  (justify-node 'simpler d (list a)))

(defun atms-test3 ()
  (nogood-nodes 'atms-test3 (list a b)))

;;; an example from de Kleer's paper by Gitchang

(proclaim '(special a b c x=1 y=x x=z y=1 z=1))

(defun step-1 ()
  (setq *atms* (create-atms "Step-1")
	a (tms-create-node *atms* "A")
	b (tms-create-node *atms* "B")
	c (tms-create-node *atms* "C")
	x=1 (tms-create-node *atms* "x=1")
	y=x (tms-create-node *atms* "y=x")
	x=z (tms-create-node *atms* "x=z")
	y=1 (tms-create-node *atms* "y=1")
	z=1 (tms-create-node *atms* "z=1") )
  (assume-node a)
  (assume-node b)
  (assume-node c)
  (justify-node 'j1 x=1 (list a))
  (justify-node 'j2 y=x (list b))
  (justify-node 'j3 x=z (list c))
  (why-nodes *atms*)
  (print-envs *atms*)

  (format t "~2%Now register nogood{A,B}")
  (nogood-nodes 'NOGOOD (list a b))
  (why-nodes *atms*)
  (print-envs *atms*)

  (format t "~2%x=1, y=x => y=1")
  (justify-node 'j4 y=1 (list x=1 y=x))
  (why-nodes *atms*)
  (print-envs *atms*)

  (format t "~2%We have a premise z=1")
  (justify-node 'Premise z=1 nil)
  (why-nodes *atms*)
  (print-envs *atms*)

  (format t "~2%z=1, x=z => x=1")
  (justify-node 'j5 x=1 (list z=1 x=z))
  (why-nodes *atms*)
  (print-envs *atms*) )


#| The result of the above program.

The Contradiction is out.
A is in, under (E-2)
B is in, under (E-3)
C is in, under (E-4)
x=1 is in, under (E-2)
y=x is in, under (E-3)
x=z is in, under (E-4)
y=1 is out.
z=1 is out.
E-1: {}
E-2: {A}
E-3: {B}
E-4: {C}

Now register nogood{A,B}
The Contradiction is out.
A is in, under (E-2)
B is in, under (E-3)
C is in, under (E-4)
x=1 is in, under (E-2)
y=x is in, under (E-3)
x=z is in, under (E-4)
y=1 is out.
z=1 is out.
E-1: {}
E-2: {A}
E-3: {B}
E-4: {C}
E-5:* {A, B}

x=1, y=x => y=1
The Contradiction is out.
A is in, under (E-2)
B is in, under (E-3)
C is in, under (E-4)
x=1 is in, under (E-2)
y=x is in, under (E-3)
x=z is in, under (E-4)
y=1 is out.
z=1 is out.
E-1: {}
E-2: {A}
E-3: {B}
E-4: {C}
E-5:* {A, B}

We have a premise z=1
The Contradiction is out.
A is in, under (E-2)
B is in, under (E-3)
C is in, under (E-4)
x=1 is in, under (E-2)
y=x is in, under (E-3)
x=z is in, under (E-4)
y=1 is out.
z=1 is in, under (E-1)
E-1: {}
E-2: {A}
E-3: {B}
E-4: {C}
E-5:* {A, B}

z=1, x=z => x=1
The Contradiction is out.
A is in, under (E-2)
B is in, under (E-3)
C is in, under (E-4)
x=1 is in, under (E-2 E-4)
y=x is in, under (E-3)
x=z is in, under (E-4)
y=1 is in, under (E-6)
z=1 is in, under (E-1)
E-1: {}
E-2: {A}
E-3: {B}
E-4: {C}
E-5:* {A, B}
E-6: {B, C}

|#

;;; Added JPHM
(proclaim '(special e h g x))
;;; Corresponds to
;;; - Haskell function `runATMS1` in src/main/haskell/app/ATMSTrun.hs

(defun book-1 (&aux i1 i2)
  (setq *atms* (create-atms "Step-1")
        a (tms-create-node *atms* "A")
        c (tms-create-node *atms* "C")
        e (tms-create-node *atms* "E")
        h (tms-create-node *atms* "H")
	)
  (assume-node a)
  (assume-node c)
  (assume-node e)
  (justify-node 'r1 h (list c e))

  (setq g (tms-create-node *atms* "G"))
  (justify-node 'r2 g (list a c))
  
  (setq x (tms-create-node *atms* "X"))
  (make-contradiction x)
  (justify-node 'r3 x (list g))

  (setq b (tms-create-node *atms* "B" :assumptionp t))
  (justify-node 'r4 h (list b c))
  
  (why-nodes *atms*)
  (print-envs *atms*)

  (format t "~%----------------------------------------")
  (setf i1 (interpretations *atms* (list (list a c) (list h g))))
  (format t "~%Interpretations with (a, c); (h, g): ~s~%" i1)
  (mapcar #'print-env i1)

  (format t "~%----------------------------------------")
  (setf i2 (interpretations *atms* (list (list h g))))
  (format t "~%Interpretations with (h, g): ~s" i2)
  (mapcar #'print-env i2)
  
  (format t "~%----------------------------------------")
  (setf i3 (interpretations *atms* (list (list h))))
  (format t "~%Interpretations with (h): ~s" i3)
  (mapcar #'print-env i3)
  ) ; end defun book-1
