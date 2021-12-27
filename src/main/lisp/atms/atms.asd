;;; -*- Mode: common-lisp; package: yr-asd; -*-


;;; ASDF file hacked in by J Maraist

(defpackage :atms-asd (:use :common-lisp :asdf))
(in-package :atms-asd)

;;; :atms
;;; (asdf:oos 'asdf:load-op :atms)
;;; :als :atms
;;; (atms-test1)

(defsystem :atms
    :components (;; (:file "package")
		 (:file "adata" :depends-on ())
		 (:file "ainter" :depends-on ())
		 ;; (:file "aplanr" :depends-on ())
		 ;; (:file "aqueens" :depends-on ())
		 (:file "arules" :depends-on ())
		 (:file "atms" :depends-on ())
		 (:file "atre" :depends-on ())
		 (:file "atest" :depends-on ())
					; 
		 ;; (:file "atret" :depends-on ())
		 ;; (:file "bcode" :depends-on ())
		 ;; (:file "blocks" :depends-on ())
		 ;; (:file "causality-ex" :depends-on ())
		 ;; (:file "causality" :depends-on ())
		 ;; (:file "csp" :depends-on ())
		 ;; (:file "funify" :depends-on ())
		 ;; (:file "id" :depends-on ())
		 ;; (:file "interactive-ex" :depends-on ())
		 ;; (:file "my" :depends-on ())
		 ;; (:file "plan-a" :depends-on ())
		 ;; (:file "plan-e" :depends-on ())
		 ;; (:file "prob" :depends-on ())
		 ;; (:file "sudoku" :depends-on ())
		 ;; (:file "unify" :depends-on ())
		 ))
