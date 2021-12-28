;;; -*- Mode: Lisp; -*-

;;; ATRE database
;; Last edited: 1/29/93, KDF

;; Copyright (c) 1992, Kenneth D. Forbus, Northwestern
;;  University, and Johan de Kleer, the Xerox Corporation
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

// (in-package :COMMON-LISP-USER)

// ; From adata.lisp
// (defun assert! (fact just &optional (*atre* *atre*)
//                   &aux datum node)
//   (setq datum (referent fact t)
//      node (datum-tms-node datum))
//   (unless (listp just) (setq just (list just)))
//   (debugging-atre "~%    Asserting ~A via ~A." fact just)
//   (justify-node (car just) node
//              (mapcar #'(lambda (f)
//                          (datum-tms-node (referent f t)))
//                      (cdr just)))
//   datum)

// ; From adata.lisp
// (defun assume! (fact reason &optional (*atre* *atre*)
//                   &aux datum node)
//   (setq datum (referent fact t)
//      node (datum-tms-node datum))
//   (cond ((not (datum-assumption? datum))
//       (setf (datum-assumption? datum) reason)
//       (debugging-atre
//        "~%    Assuming ~A via ~A." fact reason)
//       (assume-node node))
//      ((eq reason (datum-assumption? datum)))
//      (t (error
//   "Fact ~A assumed because of ~A assumed again because of ~A"
//   (show-datum datum) (datum-assumption? datum) reason)))
//   datum)

// ; From adata.lisp
// (defun already-assumed? (fact)
//   (tms-node-assumption? (get-tms-node fact)))

// ; From adata.lisp
// (defun assume-if-needed (fact reason &optional (*atre* *atre*))
//   (unless (already-assumed? fact) (assume! fact reason)))

// ; From adata.lisp
// (defmacro rassert! (fact just)
//   `(assert! ,(quotize fact) ,(quotize just)))

// ; From adata.lisp
// (defun contradiction (fact &optional (*atre* *atre*))
//   (make-contradiction (datum-tms-node (referent fact t))))

// ; From adata.lisp
// (defmacro rnogood! (informant &rest facts)
//   `(assert! 'false ,(quotize (cons informant facts))))

// ;;;; Database system

// ; From adata.lisp
// (defun get-dbclass (fact &aux dbclass)
//   (cond ((null fact) (error "~% NIL can't be a dbclass."))
//      ((listp fact) (get-dbclass (car fact)))
//      ((variable? fact)
//       (cond ((boundp fact) (get-dbclass (symbol-value fact)))
//             (t (error "~%Dbclass unbound: ~A" fact))))
//      ((symbolp fact)
//       (cond ((setq dbclass
//                    (gethash fact
//                             (atre-dbclass-table *atre*)))
//              dbclass)
//             (t (setq dbclass
//                      (make-dbclass :NAME fact :FACTS nil
//                                  :RULES nil :ATRE *atre*))
//                (setf (gethash fact
//                               (atre-dbclass-table *atre*))
//                      dbclass)
//                (push dbclass (atre-dbclasses *atre*))
//                dbclass)))
//      (t (error "Bad dbclass type: ~A" fact))))

// ; From adata.lisp
// (defun referent (fact &optional (virtual? nil))
//   (if virtual? (insert fact) (referent1 fact)))

// ; From adata.lisp
// (defun referent1 (fact) ;; Could use seperate hash table
//   (dolist (candidate (dbclass-facts (get-dbclass fact)))
//        (when (equal (datum-lisp-form candidate) fact)
//              (return-from referent1 candidate))))

// ; From adata.lisp
// (defun get-candidates (pattern)
//   (dbclass-facts (get-dbclass pattern)))

// ;;;; Interface and display of data

// ; From adata.lisp
// (defun environment-cons (fact env)
//   (cons-env (get-tms-node fact) env))

// ; From adata.lisp
// (defun view-env (env)
//   (mapcar #'view-node (env-assumptions env)))

// ;;; More interrogatives

// ; From adata.lisp
// (defun assumptions-of (fact)
//   (tms-node-label (datum-tms-node (referent fact t))))

// ;;; Extra printing routines

// ; From adata.lisp
// (defun show-datum (datum) (format nil "~A"
//                                (datum-lisp-form datum)))

// (defun show-data (&optional (*atre* *atre*)
//                          (stream *standard-output*)
//                     &aux counter)
//   (setq counter 0)
//   (format stream
//        "~%~D facts total." (atre-datum-counter *atre*))
//   (dolist (dbclass (atre-dbclasses *atre*) counter)
//     (dolist (datum (dbclass-facts dbclass))
//       (incf counter)
//       (format stream "~%~A: ~A" (show-datum datum)
//            (assumptions-of (datum-lisp-form datum)))))
//   counter)

// ; From adata.lisp
// (defun show-context (env &optional (*atre* *atre*)
//                       (stream *standard-output*)
//                     &aux counter)
//   (setq counter 0)
//   (dolist (dbclass (atre-dbclasses *atre*))
//     (dolist (datum (dbclass-facts dbclass))
//       (when (in-node? (datum-tms-node datum) env)
//      (incf counter)
//      (format stream "~%~A" (show-datum datum)))))
//   (format stream  "~%~D facts total." counter)
//   counter)

// ; From adata.lisp
// (defun show-dbclasses (&optional (*atre* *atre*)
//                               (stream *standard-output*)
//                              &aux counter)
//   ;; Handy for finding buggy assertions
//   (setq counter 0)
//   (dolist (dbclass (atre-dbclasses *atre*) counter)
//     (incf counter)
//     (format stream "~% ~A: ~D facts, ~D rules"
//          (dbclass-name dbclass)
//          (length (dbclass-facts dbclass))
//          (length (dbclass-rules dbclass)))))

