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

package org.maraist.truthmaintenancesystems.ruleengine
import scala.util.control.NonLocalReturns.*
import scala.collection.mutable.{ListBuffer, HashSet, HashMap, Queue}

// Tiny rule engine, translated from F/dK version 61 of 7/21/92.

enum Expr {
  case Sym(s: Symbol) extends Expr

  case Num(n: Int) extends Expr

  case SExpr(subexprs: List[Expr]) extends Expr
}

type Bindings[E] = Option[Map[Symbol, E]]

/**
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
  * @groupdesc query API methods for querying the TRE and its beliefs
  * from an external system.  Note that most query-style methods are
  * on [[Node]]s.
  * @groupprio query 2
  *
  * @groupname diagnostic Diagnostic and debugging methods
  * @groupdesc diagnostic Reporting the current TRE state as text.
  * @groupprio diagnostic 3
  *
  * @groupname internal Internal methods
  * @groupdesc internal Implementation methods; not generally for use
  * from outside this package.
  * @groupprio internal 10
  */
object Expr {

  /**
    *
    * <pre>
; From unify.lisp
(defun variable? (x)
  (and (symbolp x)      ;A symbol whose first character is "?"
       (char= #\? (elt (symbol-name x) 0))))
</pre>
    */
  def isVariable(e: Expr): Boolean = e match {
    case Sym(s) => isVariable(s)
    case _ => false
  }

  /**
    *
    * <pre>
; From unify.lisp
(defun variable? (x)
  (and (symbolp x)      ;A symbol whose first character is "?"
       (char= #\? (elt (symbol-name x) 0))))
</pre>
    */
  def isVariable(s: Symbol): Boolean =
    s.name.length > 0 && s.name.charAt(0) == '?'


  /**
    *
    * <pre>
; From unify.lisp
(defun unify (a b &optional (bindings nil))
   (cond ((equal a b) bindings)
         ((variable? a) (unify-variable a b bindings))
         ((variable? b) (unify-variable b a bindings))
         ((or (not (listp a)) (not (listp b))) :FAIL)
         ((not (eq :FAIL (setq bindings
                               (unify (car a) (car b) bindings))))
          (unify (cdr a) (cdr b) bindings))
         (t :FAIL)))
</pre>
    */
  def unify(a: Expr, b: Expr, bindings: Bindings[Expr] = Some(Map.empty)):
      Bindings[Expr] = (a, b) match {
    case (Sym(sa), Sym(sb)) if sa == sb => bindings
    case (Sym(s), _) if isVariable(s) => unifyVariable(s, b, bindings)
    case (_, Sym(s)) if isVariable(s) => unifyVariable(s, a, bindings)
    case (Num(na), Num(nb)) if na == nb => bindings
    case (SExpr(Nil), SExpr(Nil)) => bindings
    case (SExpr(a1 :: as), SExpr(b1 :: bs)) => unify(a1, b1, bindings) match {
      case None => None
      case bnd2 => unify(SExpr(as), SExpr(bs), bnd2)
    }
    case _ => None
  }

  /**
    *
    * <pre>
; From unify.lisp
(defun unify-variable (var exp bindings &aux binding)
  ;; Must distinguish no value from value of nil
  (setq binding (assoc var bindings))
  (cond (binding (unify (cdr binding) exp bindings))
        ;; If safe, bind <var> to <exp>
        ((free-in? var exp bindings) (cons (cons var exp) bindings))
        (t :FAIL)))
</pre>
    */
  def unifyVariable(v: Symbol, exp: Expr, bindings: Bindings[Expr]):
      Bindings[Expr] = bindings.flatMap(_.get(v)) match {
    case None => if isFreeIn(v, exp, bindings)
      then Some(bindings.map(_ + ((v, exp))).getOrElse(Map((v, exp)))) else None
    case Some(binding) => unify(binding, exp, bindings)
  }

  /**
    *
    * <pre>
; From unify.lisp
(defun free-in? (var exp bindings)
  ;; Returns nil if <var> occurs in <exp>, assuming <bindings>.
  (cond ((null exp) t)
        ((equal var exp) nil)
        ((variable? exp)
         (let ((val (assoc exp bindings)))
           (if val
               (free-in? var (cdr val) bindings)
             t)))
        ((not (listp exp)) t)
        ((free-in? var (car exp) bindings)
         (free-in? var (cdr exp) bindings))))
</pre>
    */
  def isFreeIn(v: Symbol, exp: Expr, bindings: Bindings[Expr]): Boolean =
    exp match {
      case Sym(s) if s == v => false
      case Sym(s) =>
        bindings.flatMap(_.get(s)).map(isFreeIn(v, _, bindings)).getOrElse(true)
      case Num(_) => true
      case SExpr(es) => es.forall(isFreeIn(v, _, bindings))
    }
}
