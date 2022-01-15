// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University
// and Johan de Kleer, the Xerox Corporation.
// Copyright (C) 2022 John Maraist.
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

package org.maraist.truthmaintenancesystems.logicbased
import scala.util.control.NonLocalReturns.*
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}

/**
  *
  * **Arguments and `val` members translated from**:
  * <pre>
</pre>
  *
  * @groupname interface Interface methods
  * @groupdesc interface Top-level methods for control of the LTMS
  * from an external system.
  * @groupprio interface 1
  *
  * @groupname diagnostic Diagnostic and debugging methods
  * @groupdesc diagnostic Reporting the current LTMS state as text.
  * @groupprio diagnostic 2
  *
  * @groupname internal Internal methods
  * @groupdesc internal Implementation methods; not generally for use
  * from outside this package.
  * @groupprio internal 10
  */
class Expr[D, I, R](
) {

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defvar *ltms*)

(defun normalize (*ltms* exp) (normalize-1 exp nil))
</pre>
    *
    */
  def normalize: Expr[D, I, R] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun normalize-1 (exp negate)
  (case (and (listp exp) (car exp))
    (:IMPLIES (if negate
                  (nconc (normalize-1 (cadr exp) nil)
                         (normalize-1 (caddr exp) t))
                  (disjoin (normalize-1 (cadr exp) t)
                           (normalize-1 (caddr exp) nil))))
    (:IFF (normalize-iff exp negate))
    (:OR (if negate (normalize-conjunction exp t)
             (normalize-disjunction exp nil)))
    (:AND (if negate (normalize-disjunction exp t)
                     (normalize-conjunction exp nil)))
    (:NOT (normalize-1 (cadr exp) (not negate)))
    (:TAXONOMY (normalize-tax exp negate))
    (t (if negate `((,(tms-node-false-literal (find-node *ltms* exp))))
                  `((,(tms-node-true-literal (find-node *ltms* exp))))))))
</pre>
    *
    */
  def normalize1(negate: Boolean): Expr[D, I, R] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun normalize-tax (exp negate)
  (normalize-1 `(:AND (:OR ,@ (copy-list (cdr exp))) ;one must be true
                 ;; The list is copied above to prevent very nasty bugs, since
                 ;; the rest of normalize side effects structure continually for
                 ;; efficiency.
                 ,@ (do ((firsts (cdr exp) (cdr firsts))
                       (rests (cddr exp) (cdr rests))
                       (result nil))
                      ((null rests) result)
                    (dolist (other rests)
                      (push `(:NOT (:AND ,(car firsts) ,other))
                            result))))
               negate))
</pre>
    *
    */
  def normalizeTax(negate: Boolean): Expr[D, I, R] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun normalize-conjunction (exp negate)
  (mapcan #'(lambda (sub) (normalize-1 sub negate)) (cdr exp)))
</pre>
    *
    */
  def normalizeConjunction(negate: Boolean):
      Expr[D, I, R] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun normalize-iff (exp negate)
  (nconc (normalize-1 `(:IMPLIES ,(cadr exp) ,(caddr exp)) negate)
         (normalize-1 `(:IMPLIES ,(caddr exp) ,(cadr exp)) negate)))
</pre>
    *
    */
  def normalizeIff(negate: Boolean): Expr[D, I, R] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun normalize-disjunction (exp negate)
  (unless (cdr exp)
    (return-from normalize-disjunction (list nil)))
  (do ((result (normalize-1 (cadr exp) negate))
       (rest (cddr exp) (cdr rest)))
      ((null rest) result)
    (setq result (disjoin (normalize-1 (car rest) negate) result))))
</pre>
    *
    */
  def normalizeDisjunction(negate: Boolean):
      Expr[D, I, R] = ???

} // class Expr

object Expr {

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun disjoin (conj1 conj2)
  (unless (or conj1 conj2) (return-from disjoin nil))
  (mapcan #'(lambda (disj1)
            (mapcar #'(lambda (disj2) (append disj1 disj2))
                    conj2))
          conj1))
</pre>
    *
    */
  def disjoin[D, I, R](
    conj1: Expr[D, I, R], conj2: Expr[D, I, R]): Expr[D, I, R] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defmacro compile-formula (run-tms f &optional informant &aux ltms)
  (setq ltms (create-ltms f))
  (add-formula ltms (expand-formula f))
  (generate-code ltms run-tms (if informant `(:IMPLIED-BY ,f ,informant))))
</pre>
    *
    */

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun expand-formula (x)
  (setq x (macroexpand x))
  (cond ((not (listp x)) x)
        ((case (macroexpand (car x))
           (QUOTE (partial (cadr x)))
           (LIST (mapcar #'expand-formula (cdr x)))
           (LIST* (if (cddr x)
                      (cons (expand-formula (cadr x))
                            (expand-formula `(LIST* .,(cddr x))))
                      (expand-formula (cadr x))))
           (CONS (cons (expand-formula (cadr x))
                       (mapcar #'expand-formula (caddr x))))))
        (t x)))
</pre>
    *
    */
  def expandFormula[D, I, R](e: Expr[D, I, R]): Expr[D, I, R] = ???

  /**
    *
    *
    * **Translated from**:
    * <pre>
;; From ltms.lisp
(defun partial (x)
  (cond ((null x) x)
        ((keywordp x) x)
        ((not (listp x)) `',x)
        (t (cons (partial (car x)) (partial (cdr x))))))
</pre>
    *
    */

}
