;;; compat.ss
;;; Copyright 1984-2017 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; miscellaneous definitions to make this version compatible
;;; (where possible) with previous versions...and to a small extent with
;;; other versions of scheme and other dialects of lisp as well

;;; use only those items that you need to avoid introducing accidental
;;; dependencies on other items.

(define-syntax define!
  (syntax-rules ()
    ((_ x v) (begin (set! x v) 'x))))

(define-syntax defrec!
  (syntax-rules ()
    ((_ x v) (define! x (rec x v)))))

(define-syntax begin0
  (syntax-rules ()
    ((_ x y ...) (let ((t x)) y ... t))))

(define-syntax recur
  (syntax-rules ()
    ((_ f ((i v) ...) e1 e2 ...)
     (let f ((i v) ...) e1 e2 ...))))

(define-syntax trace-recur
  (syntax-rules ()
    ((_ f ((x v) ...) e1 e2 ...)
     (trace-let f ((x v) ...) e1 e2 ...))))

(define swap-box!
  (lambda (b v)
    (if (box? b)
        (let ((x (unbox b))) (set-box! b v) x)
        (error 'swap-box! "~s is not a box" b))))

(define cull
  (lambda (pred? ls)
    (unless (procedure? pred?)
      (error 'cull "~s is not a procedure" pred?))
    (let f ([l ls])
      (cond
         [(pair? l)
          (if (pred? (car l))
              (cons (car l) (f (cdr l)))
              (f (cdr l)))]
         [(null? l) '()]
         [else (error 'cull "~s is not a proper list" ls)]))))

(define cull! cull)

(define mem
  (lambda (pred? ls)
    (unless (procedure? pred?)
      (error 'mem "~s is not a procedure" pred?))
    (let f ([l ls])
      (cond
        [(pair? l) (if (pred? (car l)) l (f (cdr l)))]
        [(null? l) #f]
        [else (error 'mem "~s is not a proper list" ls)]))))

(define rem
  (lambda (pred? ls)
    (unless (procedure? pred?)
      (error 'rem "~s is not a procedure" pred?))
    (let f ([l ls])
      (cond
        [(pair? l)
         (if (pred? (car l))
             (f (cdr l))
             (cons (car l) (f (cdr l))))]
        [(null? l) '()]
        [else (error 'rem "~s is not a proper list" ls)]))))

(define rem!
  (lambda (pred? ls)
    (unless (procedure? pred?)
      (error 'rem! "~s is not a procedure" pred?))
    (let f ([l ls])
      (cond
        [(pair? l)
         (if (pred? (car l))
             (f (cdr l))
             (begin
               (set-cdr! l (f (cdr l)))
               l))]
        [(null? l) '()]
        [else (error 'rem! "~s is not a proper list" ls)]))))

(define ass
  (lambda (pred? alist)
    (unless (procedure? pred?)
      (error 'ass "~s is not a procedure" pred?))
    (let loop ([l alist])
      (cond
        [(and (pair? l) (pair? (car l)))
         (if (pred? (caar l))
             (car l)
             (loop (cdr l)))]
        [(null? l) #f]
        [else (error 'ass "improperly formed alist ~s" alist)]))))

(define prompt-read
  (lambda (fmt . args)
    (apply printf fmt args)
    (read)))

(define tree-copy
  (rec tree-copy
    (lambda (x)
      (if (pair? x)
          (cons (tree-copy (car x)) (tree-copy (cdr x)))
          x))))

(define ferror error)

(define *most-negative-short-integer* (most-negative-fixnum))
(define *most-positive-short-integer* (most-positive-fixnum))

(define *most-negative-fixnum* (most-negative-fixnum))
(define *most-positive-fixnum* (most-positive-fixnum))

(define *eof* (read-char (open-input-string "")))

(define short-integer? fixnum?)
(define big-integer? bignum?)
(define ratio? ratnum?)
(define float? flonum?)

(define bound? top-level-bound?)
(define global-value top-level-value)
(define set-global-value! set-top-level-value!)
(define define-global-value define-top-level-value)
(define symbol-value top-level-value)
(define set-symbol-value! set-top-level-value!)

(define put putprop)
(define get getprop)

(define copy-list list-copy)
(define copy-tree tree-copy)
(define copy-string string-copy)
(define copy-vector vector-copy)

(define intern string->symbol)
(define symbol-name symbol->string)
(define string->uninterned-symbol gensym)
(define make-temp-symbol string->uninterned-symbol)
(define uninterned-symbol? gensym?)
(define temp-symbol? uninterned-symbol?)

(define compile-eval compile)

(define closure? procedure?)

(define =? =)
(define <? <)
(define >? >)
(define <=? <=)
(define >=? >=)

(define float exact->inexact)
(define rational inexact->exact)

(define char-equal? char=?)
(define char-less? char<?)
(define string-equal? string=?)
(define string-less? string<?)

; following defn conflicts with new r6rs mod
;(define mod modulo)

(define flush-output flush-output-port)
(define clear-output clear-output-port)
(define clear-input clear-input-port)

(define mapcar map)
(define mapc for-each)
(define true #t)
(define false #f)
(define t #t)
(define nil '())

(define macro-expand expand)

;;; old macro and structure definition

;;; thanks to Michael Lenaghan (MichaelL@frogware.com) for suggesting
;;; various improvements.
(define-syntax define-macro!
  (lambda (x)
    (syntax-case x ()
      [(k (name arg1 ... . args)
          form1
          form2
          ...)
       '(k name (arg1 ... . args)
           form1
           form2
           ...)]
      [(k (name arg1 arg2 ...)
          form1
          form2
          ...)
       '(k name (arg1 arg2 ...)
           form1
           form2
           ...)]
      [(k name args . forms)
       (identifier? 'name)
       (letrec ((add-car
                 (lambda (access)
                   (case (car access)
                     ((cdr) `(cadr ,@(cdr access)))
                     ((cadr) `(caadr ,@(cdr access)))
                     ((cddr) `(caddr ,@(cdr access)))
                     ((cdddr) `(cadddr ,@(cdr access)))
                     (else `(car ,access)))))
                (add-cdr
                 (lambda (access)
                   (case (car access)
                     ((cdr) `(cddr ,@(cdr access)))
                     ((cadr) `(cdadr ,@(cdr access)))
                     ((cddr) `(cdddr ,@(cdr access)))
                     ((cdddr) `(cddddr ,@(cdr access)))
                     (else `(cdr ,access)))))
                (parse
                 (lambda (l access)
                   (cond
                     ((null? l) '())
                     ((symbol? l) `((,l ,access)))
                     ((pair? l)
                      (append!
                        (parse (car l) (add-car access))
                        (parse (cdr l) (add-cdr access))))
                     (else
                      (syntax-error 'args
                        (format "invalid ~s parameter syntax" (datum k))))))))
         (with-syntax ((proc (datum->syntax-object 'k
                               (let ((g (gensym)))
                                 `(lambda (,g)
                                    (let ,(parse (datum args) `(cdr ,g))
                                      ,@(datum forms)))))))
           '(define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   ((k1 . r)
                    (datum->syntax-object 'k1
                      (proc (syntax-object->datum x)))))))))])))

(alias define-macro define-macro!)
(alias defmacro define-macro!)

(define-macro! define-struct! (name . slots)
    `(begin
        (define ,name
            (lambda ,slots
                (vector ',name ,@slots)))
        (define ,(string->symbol (format "~a?" name))
            (lambda (x)
                (and (vector? x)
                    (= (vector-length x) (+ ,(length slots)))
                    (eq? ',name (vector-ref x 0)))))
         ,@(make-accessors name slots)
         ',name))

(define make-accessors
    (lambda (name slots)
        (recur f ((n 1) (slots slots))
            (if (not (null? slots))
                (let*
                    ((afn (string->symbol (format "~a-~a" name (car slots))))
                     (sfn (string->symbol (format "~a!" afn))))
                    `((define-macro! ,afn (x) `(vector-ref ,x ,,n))
                      (define-macro! ,sfn (x v) `(vector-set! ,x ,,n ,v))
                      ,@(f (+ n) (cdr slots))))
                '()))))
;;; def.ss
;;; Copyright (C) 1987 R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; Prototype code for definition facility that remembers definitions and
;;; allows you to pretty-print or edit them (using the structure editor
;;; defined in the file "edit.ss").

;;; def can be in place of define at top level (i.e., not within a lambda,
;;; let, let*, or letrec body).  It saves the source for the definition
;;; as well as performing the defintion.  Type (ls-def) for a list of
;;; variables defined this session, and (pp-def variable) to return the
;;; definition of a particular variable.

;;; Possible exercises/enhancements:
;;;
;;;  1) Write a "dskout" function that pretty-prints the definitions of
;;;     all or selected variables defined this session to a file.
;;;
;;;  2) In place of "def", write a modified "load" that remembers where
;;;     (that is, in which file) it saw the definition for each variable
;;;     defined in a particular session.  This would be used instead of
;;;     the "def" form.  "ls-def" would be similar to what it is now.
;;;     "pp-def" could be similar to what it is now, or it could involve
;;;     rereading the corresponding file.  "ed-def" could invoke the
;;;     structure editor and (as an option) print the modified definition
;;;     back to the corresponding file, or "ed-def" could invoke a host
;;;     editor (such as Unix "vi" or VMS "edit") on the corresponding
;;;     source file, with an option to reload.  If this tool is smart
;;;     enough, it could get around the limitation that definitions use
;;;     define at top-level, i.e., (let ([x #f]) (set! foo (lambda () x)))
;;;     could be recognized as a definition for foo.

(define-syntax def
  ;; only makes sense for "top level" definitions
  (syntax-rules ()
    [(_ (var . formals) . body)
     (begin (define (var . formals) . body)
            (insert-def! 'var '(def (var . formals) . body) var)
            'var)]
    [(_ var exp)
     (begin (define var exp)
            (insert-def! 'var '(def var exp) var)
            'var)]))

(define-syntax pp-def
  (syntax-rules (quote)
   ; allow var to be unquoted or quoted
    [(_ var) (pp-def-help 'var var)]
    [(_ 'var) (pp-def-help 'var var)]))

(define-syntax ed-def
  (syntax-rules (quote)
   ; allow var to be unquoted or quoted
    [(_ var) (ed-def-help 'var var)]
    [(_ 'var) (ed-def-help 'var var)]))


(define insert-def! #f) ; assigned within the let below
(define ls-def #f) ; assigned within the let below
(define pp-def-help #f) ; assigned within the let below
(define ed-def-help #f) ; assigned within the let below
(let ([defs '()])
  (define tree-copy
    (rec tree-copy
      (lambda (x)
        (if (pair? x)
            (cons (tree-copy (car x)) (tree-copy (cdr x)))
            x))))
   (set! insert-def!
      (lambda (var defn val)
         (unless (symbol? var)
            (error 'insert-def! "~s is not a symbol" var))
         (let ([a (assq var defs)])
            (if a
                (set-cdr! a (cons defn val))
                (set! defs (cons (cons var (cons defn val)) defs))))))
   (set! ls-def
      (lambda ()
         (map car defs)))
   (set! pp-def-help
      (lambda (var val)
         (unless (symbol? var)
            (error 'pp-def "~s is not a symbol" var))
         (let ([a (assq var defs)])
            (unless a
               (error 'pp-def
                      "~s has not been defined during this session"
                      var))
            (unless (eq? (cddr a) val)
               (printf "Warning: ~s has been reassigned since definition"
                       var))
            (cadr a))))
   (set! ed-def-help
      (lambda (var val)
         (unless (symbol? var)
            (error 'ed-def "~s is not a symbol" var))
         (let ([a (assq var defs)])
            (unless a
               (error 'ed-def
                      "~s has not been defined during this session"
                      var))
            (unless (eq? (cddr a) val)
               (printf "Warning: ~s reassigned since last definition"
                       var))
            ; edit is destructive; the copy allows the defined name to
            ; be changed without affecting the old name's definition
            (eval (edit (tree-copy (cadr a))))))))
;;; edit.ss
;;; Copyright (C) 1987 R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; This file contains an implementation of a simple interactive structure
;;; editor for Scheme.  The editor is invoked with an expression as it's
;;; single argument.  It prompts for, reads, and processes editor commands.

;;; The editor commands recognized are those documented in the Texas
;;; Instruments' PC Scheme manual.  They are summarized below.

;;; Command syntax          Action
;;;
;;;   q or <eof>            Quit the editor, returning edited expression.
;;;
;;;   p                     Write the current expression.
;;;
;;;   ?                     Write to level 2, length 10.
;;;
;;;   pp                    Pretty print the current expression.
;;;
;;;   ??                    Pretty print to level 2, length 10.
;;;
;;;   <pos>                 Move to subexpression of current expression
;;;                         <pos> = 0 is the current expression, <pos> > 0
;;;                         is the numbered subexpression (1 for first, 2
;;;                         for second, ...), <pos> < 0 is the numbered
;;;                         subexpression from the right (-1 for last, -2
;;;                         for second to last, ...), and <pos> = * is the
;;;                         "last cdr" of the current expression.  If <pos>
;;;                         is not 0, the current expression must be a list.
;;;
;;;   b                     Move back to parent expression.
;;;
;;;   t                     Move to top-level expression.
;;;
;;;   pr                    Move to expression on the left (previous).
;;;
;;;   n                     Move to expression on the right (next).
;;;
;;;   (f <obj>)             Find <obj> within or to the right of the current
;;;                         expression using equal?.
;;;
;;;   f or (f)              Find <obj> of last (f <obj>) command.
;;;
;;;   (d <pos>)             Delete the expression at position <pos>.
;;;
;;;   (r <pos> <obj>)       Replace the expression at position <pos> with
;;;                         <obj>.
;;;
;;;   (s <obj1> <obj2>)     Replace all occurrences of <obj1> by <obj2>
;;;                         within the current expression.
;;;
;;;   (dp <pos>)            Remove parens from around expression at position
;;;                         <pos>.
;;;
;;;   (ap <pos1> <pos2>)    Insert parens around expressions from position
;;;                         <pos1> through <pos2> (inclusive).  If <pos1> is
;;;                         0 or *, <pos2> is ignored and may be omitted.
;;;
;;;   (ib <pos> <obj>)      Insert <obj> before expression at position <pos>.
;;;
;;;   (ia <pos> <obj>)      Insert <obj> after expression at position <pos>.
;;;
;;;   (sb <pos> <obj>)      Splice <obj> before expression at position <pos>.
;;;
;;;   (sa <pos> <obj>)      Splice <obj> after expression at position <pos>.

;;; Possible exercises/enhancements:
;;;
;;;  1) Implement an infinite undo ("u") command in the editor.  This
;;;     can be done by creating an "inverse" function for each operation
;;;     that causes a side-effect, i.e, a closure that "remembers" the
;;;     list cells involved and knows how to put them back the way they
;;;     were.  An undo (u) variable could then be added to the editor's
;;;     main loop; it would be bound to a list containing the set of
;;;     registers at the point of the last side-effect (similarly to the
;;;     "back" (b) variable) and the undo function for the side-effect.
;;;
;;;  2) Implement an infinite redo ("r") command in the editor.  This
;;;     can be done by remembering the undo functions and registers for
;;;     the undo's since the last non-undo command.
;;;
;;;  3) Handle circular structures better in the editor.  Specifically,
;;;     modify the find ("f") command so that it always terminates, and
;;;     devise a method for printing circular structures with the "p"
;;;     and "pp" commands.  Cure the bug mentioned in the overview of
;;;     the code given later in the file.
;;;
;;;  4) Add a help ("h") command to the editor.  This could be as simple
;;;     as listing the available commands.
;;;
;;;  5) Make the editor "extensible" via user-defined macros or editor
;;;     commands written in Scheme.
;;;
;;;  6) Modify the editor to provide more descriptive error messages that
;;;     diagnose the problem and attempt to give some help.  For example,
;;;     if the editor receives "(r 1)" it might respond with:
;;;     "Two few arguments:
;;;         Type (r pos exp) to replace the expression at position pos
;;;         with the expression exp."
;;;     This should be implemented in conjunction with the help command.
;;;     Should it be possible to disable such verbose error messages?

;;; Implementation:
;;;
;;; The main editor loop and many of the help functions operate on a
;;; set of "registers".  These registers are described below:
;;;
;;; s     The current find object.  s is initially #f, and is bound to a
;;;       pair containing the find object when the first (f <obj>) command
;;;       is seen.  The identical f and (f) commands use the saved object.
;;;
;;; p     The parent of the current expression.  This is initially a list
;;;       of one element, the argument to edit.  It is updated by various
;;;       movement commands.
;;;
;;; i     The index of the current expression in the parent (p).  This is
;;;       initially 0.  It is updated by various movement commands.
;;;
;;; b     The "back" chain; actually a list containing the registers p, i
;;;       and b for the parent of the current expression.  It is initially
;;;       ().  It is updated by various movement commands.
;;;
;;; Bugs:
;;;
;;; When editing a circular structure, it is possible for the editor to
;;; get lost.  That is, when the parent node of the current expression
;;; is changed by a command operating on a subexpression of the current
;;; expression, the index for the current expression may become incorrect.
;;; This can result in abnormal termination of the editor.  It would be
;;; fairly simple to check for this (in list-ref) and reset the editor,
;;; and it may be possible to use a different set of registers to avoid
;;; the problem altogether.

(define edit #f) ; assigned within the let expression below
(let ()
   (define cmdeq?
      ;; used to check command syntax
      (lambda (cmd pat)
         (and (pair? cmd)
              (eq? (car cmd) (car pat))
              (let okargs? ([cmd (cdr cmd)] [pat (cdr pat)])
                 (if (null? pat)
                     (null? cmd)
                     (and (not (null? cmd))
                          (okargs? (cdr cmd) (cdr pat))))))))
   (define find
      ;; find expression within or to right of current expression
      (lambda (s0 p0 i0 b0)
         (define check
            (lambda (p i b)
               (if (equal? (list-ref p i) (car s0))
                   (wrlev s0 p i b)
                   (continue p i b))))
         (define continue
            (lambda (p i b)
               (let ([e (list-ref p i)])
                  (if (atom? e)
                      (let next ([p p] [i i] [b b])
                         (let ([n (maxref p)])
                            (if (or (not n) (< i n))
                                (check p (+ i 1) b)
                                (if (null? b)
                                    (search-failed s0 p0 i0 b0)
                                    (apply next b)))))
                      (check e 0 (list p i b))))))
         (continue p0 i0 b0)))
   (define maxref
      ;; use "hare and tortoise" algorithm to check for circular lists.
      ;; return maximum reference index (zero-based) for a list x.  return
      ;; -1 for atoms and #f for circular lists.
      (lambda (x)
         (let f ([hare x] [tortoise x] [n -1])
            (cond
               [(atom? hare) n]
               [(atom? (cdr hare)) (+ n 1)]
               [(eq? (cdr hare) tortoise) #f]
               [else (f (cddr hare) (cdr tortoise) (+ n 2))]))))
   (define move
      ;; move to subexpression specified by x and pass current state to k.
      (lambda (x s p i b k)
         (cond
            [(eqv? x 0) (k s p i b)]
            [(eq? x '*)
             (let ([m (maxref (list-ref p i))])
                (if m
                    (k s (list-ref p i) '* (list p i b))
                    (invalid-movement s p i b)))]
            [(> x 0)
             (let ([m (maxref (list-ref p i))] [x (- x 1)])
                (if (or (not m) (>= m x))
                    (k s (list-ref p i) x (list p i b))
                    (invalid-movement s p i b)))]
            [else
             (let ([m (maxref (list-ref p i))] [x (- -1 x)])
                (if (and m (>= m x))
                    (let ([x (- m x)])
                       (k s (list-ref p i) x (list p i b)))
                    (invalid-movement s p i b)))])))
   (define proper-list?
      ;; return #t if x is a proper list.
      (lambda (x)
         (and (maxref x)
              (or (null? x) (null? (cdr (last-pair x)))))))
   (define list-ref
      ;; reference list ls element i.  i may be *, in which case return
      ;; the last pair of ls.
      (lambda (ls i)
         (if (eq? i '*)
             (cdr (last-pair ls))
             (car (list-tail ls i)))))
   (define list-set!
      ;; change element i of ls to x.
      (lambda (ls i x)
         (if (eq? i '*)
             (set-cdr! (last-pair ls) x)
             (set-car! (list-tail ls i) x))))
   (define list-cut!
      ;; remove element i from ls.
      (lambda (ls i)
         (let ([a (cons '() ls)])
            (set-cdr! (list-tail a i) (list-tail a (+ i 2)))
            (cdr a))))
   (define list-splice!
      ;; insert ls2 into ls1 in place of element i.
      (lambda (ls1 i ls2)
         (let ([a (list-tail ls1 i)])
            (unless (null? (cdr a))
               (set-cdr! (last-pair ls2) (cdr a)))
            (set-car! a (car ls2))
            (set-cdr! a (cdr ls2)))
         ls1))
   (define list-ap*!
      ;; place parens from element i through last pair of ls.
      (lambda (ls i)
         (let ([a (list-tail ls i)])
            (let ([c (cons (car a) (cdr a))])
               (set-car! a c)
               (set-cdr! a '())))
         ls))
   (define list-ap!
      ;; place parens from element i0 through element i1.
      (lambda (ls i0 i1)
         (let ([a (list-tail ls i0)] [b (list-tail ls i1)])
            (let ([c (cons (car a) (cdr a))])
               (set-car! a c)
               (if (eq? a b)
                   (set-cdr! c '())
                   (begin (set-cdr! a (cdr b))
                          (set-cdr! b '())))))
         ls))
   (define wrlev
      ;; write current expression to level 2, length 10 and continue.
      (lambda (s p i b)
         (parameterize ([print-level 2] [print-length 10])
            (printf "~s~%" (list-ref p i)))
         (edit-loop s p i b)))
   (define wr
      ;; write current expression and continue.
      (lambda (s p i b)
         (printf "~s~%" (list-ref p i))
         (edit-loop s p i b)))
   (define pplev
      ;; pretty print current expression to level 2, length 10 and continue.
      (lambda (s p i b)
         (parameterize ([print-level 2] [print-length 10])
            (pretty-print (list-ref p i)))
         (edit-loop s p i b)))
   (define pp
      ;; pretty print current expression and continue.
      (lambda (s p i b)
         (pretty-print (list-ref p i))
         (edit-loop s p i b)))
   (define not-a-proper-list
      ;; complain and continue.
      (lambda (s p i b)
         (printf "structure is not a proper list~%")
         (edit-loop s p i b)))
   (define cannot-dp-zero
      ;; complain and continue.
      (lambda (s p i b)
         (printf "cannot remove parens from current expression~%")
         (edit-loop s p i b)))
   (define pos2-before-pos1
      ;; complain and continue.
      (lambda (s p i b)
         (printf "second position before first~%")
         (edit-loop s p i b)))
   (define invalid-movement
      ;; complain and continue.
      (lambda (s p i b)
         (printf "no such position~%")
         (edit-loop s p i b)))
   (define unrecognized-command-syntax
      ;; complain and continue.
      (lambda (s p i b)
         (printf "unrecognized command syntax~%")
         (edit-loop s p i b)))
   (define search-failed
      ;; complain and continue.
      (lambda (s p i b)
         (printf "search failed~%")
         (edit-loop s p i b)))
   (define no-previous-find
      ;; complain and continue.
      (lambda (s p i b)
         (printf "no previous find command~%")
         (edit-loop s p i b)))
   (define edit-loop
      ;; read command and process.
      (lambda (s p i b)
         (let ([x (begin (printf "edit> ") (read))])
            (cond
               [(eof-object? x) (newline)] ; need newline after eof
               [(eq? x 'q)] ; do not need newline after q
               [(eq? x 'p) (wr s p i b)]
               [(eq? x '?) (wrlev s p i b)]
               [(eq? x 'pp) (pp s p i b)]
               [(eq? x '??) (pplev s p i b)]
               [(or (integer? x) (eqv? x '*)) (move x s p i b wrlev)]
               [(eq? x 't)
                (let f ([p p] [i i] [b b])
                   (if (null? b)
                       (wrlev s p i b)
                       (apply f b)))]
               [(eq? x 'b)
                (if (pair? b)
                    (apply wrlev s b)
                    (invalid-movement s p i b))]
               [(eq? x 'n)
                (let ([n (maxref p)])
                   (if (and (not (eq? i '*)) (or (not n) (< i n)))
                       (wrlev s p (+ i 1) b)
                       (invalid-movement s p i b)))]
               [(eq? x 'pr)
                (if (and (not (eq? i '*)) (> i 0))
                    (wrlev s p (- i 1) b)
                    (invalid-movement s p i b))]
               [(or (eq? x 'f) (cmdeq? x '(f)))
                (if s
                    (find s p i b)
                    (no-previous-find s p i b))]
               [(cmdeq? x '(f x))
                (find (cons (cadr x) '()) p i b)]
               [(and (cmdeq? x '(r x x))
                     (or (integer? (cadr x)) (eq? (cadr x) '*)))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-set! p0 i0 (caddr x))))
                (wrlev s p i b)]
               [(cmdeq? x '(s x x))
                (list-set! p i (subst! (caddr x) (cadr x) (list-ref p i)))
                (wrlev s p i b)]
               [(and (cmdeq? x '(d x)) (eqv? (cadr x) 0))
                (list-set! p i '())
                (wrlev s p i b)]
               [(and (cmdeq? x '(d x)) (eq? (cadr x) '*))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (set-cdr! (last-pair p0) '())
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(d x)) (integer? (cadr x)))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-set! p i (list-cut! p0 i0))
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(dp x)) (eqv? (cadr x) 0))
                (let ([e (list-ref p i)])
                   (if (and (pair? e) (null? (cdr e)))
                       (begin (list-set! p i (car e))
                              (wrlev s p i b))
                       (cannot-dp-zero s p i b)))]
               [(and (cmdeq? x '(dp x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (let ([e0 (list-ref p0 i0)])
                         (if (or (proper-list? e0)
                                 (and (pair? e0) (eqv? i0 (maxref p0))))
                             (begin (if (null? e0)
                                        (list-set! p i (list-cut! p0 i0))
                                        (list-splice! p0 i0 e0))
                                    (wrlev s p i b))
                             (not-a-proper-list s  p i b)))))]
               [(and (or (cmdeq? x '(ap x)) (cmdeq? x '(ap x x)))
                     (memv (cadr x) '(0 *)))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-set! p0 i0 (list (list-ref p0 i0)))
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(ap x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0)))
                     (eq? (caddr x) '*))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-ap*! p0 i0)
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(ap x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0)))
                     (and (integer? (caddr x)) (not (= (caddr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (move (caddr x) s p i b
                         (lambda (s1 p1 i1 b1)
                            (if (>= i1 i0)
                                (begin (list-ap! p0 i0 i1)
                                       (wrlev s p i b))
                                (pos2-before-pos1 s p i b))))))]
               [(and (cmdeq? x '(ib x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-splice! p0 i0 (list (caddr x) (list-ref p0 i0)))
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(ia x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-splice! p0 i0 (list (list-ref p0 i0) (caddr x)))
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(sb x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-splice! p0 i0
                         (append (caddr x) (list (list-ref p0 i0))))
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(sa x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-splice! p0 i0 (cons (list-ref p0 i0) (caddr x)))
                      (wrlev s p i b)))]
               [else
                (unrecognized-command-syntax s p i b)]))))
   (set! edit
      ;; set up keyboard interrupt handler and go.
      (lambda (e)
         (let ([p (cons e '())])
            (let ([k (call/cc (lambda (k) k))]) ; return here on interrupt
               (parameterize ([keyboard-interrupt-handler
                               (lambda ()
                                  (printf "reset~%")
                                  (k k))])
                  (wrlev #f p 0 '())
                  (car p)))))))
;;; Copyright 2017 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; This file contains a sample parser defined via the ez-grammar system
;;; and a simple test of the parser.

;;; This file is organized as follows:
;;;
;;; - (streams) library providing the required exports for ez-grammar and
;;;   the parser.
;;;
;;; - (state-case) library exporting the state-case macro, copped from
;;;   cmacros.ss, for use by the lexer.
;;;
;;; - (lexer) library providing a simple lexer that reads characters
;;;   from a port and produces a corresponding stream of tokens.
;;;
;;; - (parser) library providing the sample parser.
;;;
;;; - ez-grammar-test procedure that tests the sample parser.
;;;
;;; Instructions for running the test are at the end of this file.

(library (streams)
  (export stream-cons stream-car stream-cdr stream-nil stream-null?
    stream-map stream stream-append2 stream-append-all stream-last-forced)
  (import (chezscheme))

  (define stream-cons
    (lambda (x thunk)
      (cons x thunk)))

  (define stream-car
    (lambda (x)
      (car x)))

  (define stream-cdr
    (lambda (x)
      (when (procedure? (cdr x)) (set-cdr! x ((cdr x))))
      (cdr x)))

  (define stream-nil '())

  (define stream-null?
    (lambda (x)
      (null? x)))

  (define stream-map
    (lambda (f x)
      (if (stream-null? x)
          '()
          (stream-cons (f (stream-car x))
            (lambda ()
              (stream-map f (stream-cdr x)))))))

  (define stream
    (lambda xs
      xs))

  (define stream-append2
    (lambda (xs thunk)
      (if (null? xs)
          (thunk)
          (stream-cons (stream-car xs)
            (lambda ()
              (stream-append2 (stream-cdr xs) thunk))))))

  (define stream-append-all
    (lambda (stream$) ;; stream of streams
      (if (stream-null? stream$)
          stream$
          (stream-append2 (stream-car stream$)
            (lambda () (stream-append-all (stream-cdr stream$)))))))

  (define stream-last-forced
    (lambda (x)
      (and (not (null? x))
           (let loop ([x x])
             (let ([next (cdr x)])
               (if (pair? next)
                   (loop next)
                   (car x)))))))
)

(library (state-case)
  (export state-case eof)
  (import (chezscheme))

  ;;; from Chez Scheme Version 9.5.1 cmacros.ss
  (define-syntax state-case
    (lambda (x)
      (define state-case-test
        (lambda (cvar k)
          (with-syntax ((cvar cvar))
            (syntax-case k (-)
              (char
                (char? (datum char))
                '(char=? cvar char))
              ((char1 - char2)
               (and (char? (datum char1)) (char? (datum char2)))
               '(char<=? char1 cvar char2))
              (predicate
                (identifier? 'predicate)
                '(predicate cvar))))))
      (define state-case-help
        (lambda (cvar clauses)
          (syntax-case clauses (else)
            (((else exp1 exp2 ...))
             '(begin exp1 exp2 ...))
            ((((k ...) exp1 exp2 ...) . more)
             (with-syntax (((test ...)
                            (map (lambda (k) (state-case-test cvar k))
                              '(k ...)))
                           (rest (state-case-help cvar 'more)))
               '(if (or test ...) (begin exp1 exp2 ...) rest)))
            (((k exp1 exp2 ...) . more)
             (with-syntax ((test (state-case-test cvar 'k))
                           (rest (state-case-help cvar 'more)))
               '(if test (begin exp1 exp2 ...) rest))))))
      (syntax-case x (eof)
        ((_ cvar (eof exp1 exp2 ...) more ...)
         (identifier? 'cvar)
         (with-syntax ((rest (state-case-help 'cvar '(more ...))))
           '(if (eof-object? cvar)
                 (begin exp1 exp2 ...)
                 rest))))))

  (define-syntax eof
    (lambda (x)
      (syntax-error x "misplaced aux keyword")))
)

(library (lexer)
  (export token? token-type token-value token-bfp token-efp lexer)
  (import (chezscheme) (state-case) (streams))

  (define-record-type token
    (nongenerative)
    (fields type value bfp efp))

  ;; test lexer
  (define lexer
    (lambda (fn ip)
      (define $prev-pos 0)
      (define $pos 0)
      (define ($get-char)
        (set! $pos (+ $pos 1))
        (get-char ip))
      (define ($unread-char c)
        (set! $pos (- $pos 1))
        (unread-char c ip))
      (define ($ws!) (set! $prev-pos $pos))
      (define ($make-token type value)
        (let ([tok (make-token type value $prev-pos $pos)])
          (set! $prev-pos $pos)
          tok))
      (define ($lex-error c)
        (errorf #f "unexpected ~a at character ~s of ~a"
          (if (eof-object? c)
              "eof" 
              (format "character '~c'" c))
          $pos fn))
      (define-syntax lex-error
        (syntax-rules ()
          [(_ ?c)
           (let ([c ?c])
             ($lex-error c)
             (void))]))
      (let-values ([(sp get-buf) (open-string-output-port)])
        (define (return-token type value)
          (stream-cons ($make-token type value) lex))
        (module (identifier-initial? identifier-subsequent?)
          (define identifier-initial?
            (lambda (c)
              (char-alphabetic? c)))
          (define identifier-subsequent?
            (lambda (c)
              (or (char-alphabetic? c) 
                  (char-numeric? c)))))
        (define-syntax define-state-case
          (syntax-rules ()
            [(_ ?def-id ?char-id clause ...)
             (define (?def-id)
               (let ([?char-id ($get-char)])
                 (state-case ?char-id clause ...)))]))
        (define-state-case lex c
          [eof stream-nil]
          [char-whitespace? ($ws!) (lex)]
          [char-numeric? (lex-number c)]
          [#\/ (seen-slash)]
          [identifier-initial? (put-char sp c) (lex-identifier)]
          [#\( (return-token 'lparen #\()]
          [#\) (return-token 'rparen #\))] 
          [#\! (return-token 'bang #\!)] 
          [#\+ (seen-plus)] 
          [#\- (seen-minus)] 
          [#\= (seen-equals)] 
          [#\* (return-token 'binop '*)]
          [#\, (return-token 'sep #\,)]
          [#\; (return-token 'sep #\;)]
          [else (lex-error c)])
        (module (lex-identifier)
          (define (id) (return-token 'id (string->symbol (get-buf))))
          (define-state-case next c
            [eof (id)]
            [identifier-subsequent? (put-char sp c) (next)]
            [else ($unread-char c) (id)])
          (define (lex-identifier) (next)))
        (define-state-case seen-plus c
          [eof (return-token 'binop '+)]
          [char-numeric? (lex-signed-number #\+ c)]
          [else (return-token 'binop '+)])
        (define-state-case seen-minus c
          [eof (return-token 'binop '-)]
          [char-numeric? (lex-signed-number #\- c)]
          [else (return-token 'binop '-)])
        (define-state-case seen-equals c
          [eof (return-token 'binop '=)]
          [#\> (return-token 'big-arrow #f)]
          [else (return-token 'binop '=)])
        (module (lex-number lex-signed-number)
          (define (finish-number)
            (let ([str (get-buf)])
              (let ([n (string->number str 10)])
                (unless n (errorf 'lexer "unexpected number literal ~a" str))
                (return-token 'integer n))))
          (define (num)
            (let ([c ($get-char)])
              (state-case c
                [eof (finish-number)]
                [char-numeric? (put-char sp c) (num)]
                [else ($unread-char c) (finish-number)])))
          (define (lex-signed-number s c)
            (put-char sp s)
            (lex-number c))
          (define (lex-number c)
            (state-case c
              [eof (assert #f)]
              [char-numeric? (put-char sp c) (num)]
              [else (assert #f)])))
        (define-state-case seen-slash c
          [eof (return-token 'binop '/)]
          [#\* (lex-block-comment)]
          [#\/ (lex-comment)]
          [else (return-token 'binop '/)])
        (define-state-case lex-comment c
          [eof (lex)]
          [#\newline ($ws!) (lex)]
          [else (lex-comment)])
        (define (lex-block-comment)
          (define-state-case maybe-end-comment c
            [eof (lex-error c)]
            [#\/ ($ws!) (lex)]
            [else (lex-block-comment)])
          (let ([c ($get-char)])
            (state-case c
              [eof (lex-error c)]
              [#\* (maybe-end-comment)]
              [else (lex-block-comment)])))
        (lex))))

  (record-writer (record-type-descriptor token)
    (lambda (x p wr)
      (put-char p #\[)
      (wr (token-type x) p)
      (put-char p #\,)
      (put-char p #\space)
      (wr (token-value x) p)
      (put-char p #\])
      (put-char p #\:)
      (wr (token-bfp x) p)
      (put-char p #\-)
      (wr (token-efp x) p)))
)

(module parser ()
  (export parse *sfd*)
  (import (chezscheme) (streams) (lexer))
  (define *sfd*)
  (module (define-grammar is sat parse-consumed-all? parse-result-value grammar-trace make-src)
    (define (sep->parser sep)
      (cond
        [(char? sep) (sat (lambda (x) (and (eq? (token-type x) 'sep) (eq? (token-value x) sep))))]
        [(symbol? sep) (sat (lambda (x) (eq? (token-type x) sep)))]
        [else (errorf "don't know how to parse separator: ~s" sep)]))
    (meta define (constant? x) (let ([x (syntax->datum x)]) (or (string? x) (char? x))))
    (define constant->parser
      (lambda (const)
        (define (token-sat type val)
          (sat (lambda (x)
                 (let ([ans (and (token? x) (eqv? (token-type x) type) (eqv? (token-value x) val))])
                   (when (grammar-trace) (printf "    ~s is [~s, ~a]? => ~s~%" x type val ans))
                   ans))))
        (if (string? const)
            (case const
              [else (token-sat 'id (string->symbol const))])
            (case const
              [#\( (token-sat 'lparen const)]
              [#\) (token-sat 'rparen const)]
              [#\! (token-sat 'bang const)]
              [else (errorf 'constant->parser "don't know how to construct a parser for ~a" const)]))))
    (meta define (constant->markdown k)
      (format "~a" k))
    (define binop->parser
      (lambda (binop)
        (define (binop-sat type val)
          (is val
            (where [x <- item] (and (token? x) (eq? (token-type x) type) (eq? (token-value x) val)))))
        (define (unexpected) (errorf 'binop->parser "don't know how to construct a parser for ~a" binop))
        (if (string? binop)
            (binop-sat 'binop
              (case binop
                ["=" '=]
                ["+" '+]
                ["-" '-]
                ["*" '*]
                ["/" '/]
                [else (unexpected)]))
            (unexpected))))
    (define make-src
      (lambda (bfp efp)
        (make-source-object *sfd* bfp efp)))
    (include "ez-grammar.ss"))

  (define token
    (case-lambda
      [(type)
       (is (token-value x)
         (where
           [x <- (sat (lambda (x)
                        (let ([ans (eq? (token-type x) type)])
                          (when (grammar-trace) (printf "    ~s is ~s? => ~s~%" x type ans))
                          ans)))]))]
      [(type val)
       (is (token-value x)
         (where
           [x <- (sat (lambda (x)
                        (let ([ans (and
                                     (eq? (token-type x) type)
                                     (eqv? (token-value x) val))])
                          (when (grammar-trace) (printf "    ~s is [~s, ~s]? => ~s~%" x type val ans))
                          ans)))]))]))

  (define identifier (token 'id))

  (define integer (token 'integer))

  (define-grammar expr (markdown-directory ".")
    (TERMINALS
      (identifier (x y) (DESCRIPTION ("An identifier is ...")))
      (integer (i) (DESCRIPTION ("An integer literal is ..."))))
    (expr (e)
      (BINOP src ((RIGHT "=") (LEFT "+" "-") (LEFT "*" "/")) t) =>
        (lambda (src op x y)
          (make-annotation `(,op ,x ,y) src `(,op ,(annotation-stripped x) ,(annotation-stripped y)))))
    (term (t)
      [test-SEP+ :: src "sepplus" #\( (SEP+ e #\;) #\) =>
        (lambda (src e+)
          (make-annotation `(SEP+ ,@e+) src `(SEP+ ,@(map annotation-stripped e+))))]
      [test-SEP* :: src "sepstar" #\( (SEP* e #\,) #\) =>
        (lambda (src e*)
          (make-annotation `(SEP* ,@e*) src `(SEP* ,@(map annotation-stripped e*))))]
      [test-OPT :: src "opt" #\( (OPT e #f) #\) =>
        (lambda (src maybe-e)
          (if maybe-e
              (make-annotation `(OPT ,maybe-e) src `(OPT ,(annotation-stripped maybe-e)))
              (make-annotation `(OPT) src `(OPT))))]
      [test-K+ :: src "kplus" #\( (K+ e) #\) =>
        (lambda (src e+)
          (make-annotation `(K+ ,@e+) src `(K+ ,@(map annotation-stripped e+))))]
      [test-K* :: src "kstar" #\( (K* e) #\) =>
        (lambda (src e*)
          (make-annotation `(K* ,@e*) src `(K* ,@(map annotation-stripped e*))))]
      [varref :: src x =>
        (lambda (src id)
          (make-annotation `(id ,id) src `(id ,id)))]
      [intref :: src i =>
        (lambda (src n)
          (make-annotation `(int ,n) src `(int ,n)))]
      [group :: src #\( e #\) =>
        (lambda (src e)
          `(group ,src ,e))]))

  (define parse
    (lambda (fn ip)
      (let ([token-stream (lexer fn ip)])
        (define (oops)
          (let ([last-token (stream-last-forced token-stream)])
            (if last-token
                (errorf 'parse "parse error at or before character ~s of ~a" (token-bfp last-token) fn)
                (errorf 'parse "no expressions found in ~a" fn))))
        ;;; return the first result, if any, for which the input stream was entirely consumed.
        (let loop ([res* (expr token-stream)])
          (if (null? res*)
              (oops)
              (let ([res (car res*)])
                (if (parse-consumed-all? res)
                    (parse-result-value res)
                    (loop (cdr res*))))))))))

(define run
  (lambda (fn)
    (import parser)
    (let* ([ip (open-file-input-port fn)]
           [sfd (make-source-file-descriptor fn ip #t)]
           [ip (transcoded-port ip (native-transcoder))])
      (fluid-let ([*sfd* sfd])
        (eval
          `(let ()
             (define-syntax define-ops
               (lambda (x)
                 (syntax-case x ()
                   [(_ op ...)
                    `(begin
                        (define-syntax op
                          (lambda (x)
                            (let ([src (annotation-source (syntax->annotation x))])
                              (with-syntax ([bfp (source-object-bfp src)] [efp (source-object-efp src)])
                                (syntax-case x ()
                                  [(_ e (... ...)) '`(op (bfp . efp) ,e (... ...))])))))
                        ...)])))
             (define-ops SEP+ SEP* OPT K+ K* id int group)
             (define-ops = + - * /)
             (define x 'x)
             (define y 'y)
             (define z 'z)
             ,(dynamic-wind
                void
                (lambda () (parse fn ip))
                (lambda () (close-input-port ip)))))))))

(define (ez-grammar-test)
  (define n 0)
  (define test
    (lambda (line* okay?)
      (set! n (+ n 1))
      (let ([fn (format "testfile~s" n)])
        (with-output-to-file fn
          (lambda () (for-each (lambda (line) (printf "~a\n" line)) line*))
          'replace)
        (let ([result (parameterize ([compile-profile #t] [compile-interpret-simple #f])
                        (guard (c [else c]) (run fn)))])
          (guard (c [else #f]) (profile-dump-html))
          (delete-file fn)
          (delete-file "profile.html")
          (delete-file (format "~a.html" fn))
          (unless (okay? result)
            (printf "test ~s failed\n" n)
            (printf "  test code:")
            (for-each (lambda (line) (printf "    ~a\n" line)) line*)
            (printf "  result:\n    ")
            (if (condition? result)
                (begin (display-condition result) (newline))
                (parameterize ([pretty-initial-indent 4])
                  (pretty-print result)))
            (newline))))))

  (define-syntax returns
    (syntax-rules ()
      [(_ k) (lambda (x) (equal? x 'k))]))

  (define-syntax oops
    (syntax-rules ()
      [(_ (c) e1 e2 ...)
       (lambda (c) (and (condition? c) e1 e2 ...))]))

  (test
    '(
       "1347"
       )
    (returns
      (int (0 . 4) 1347)))

  (test
    '(
       "3 /*"
       )
    (oops (c)
      (equal? (condition-message c) "unexpected ~a at character ~s of ~a")
      (equal? (condition-irritants c) '("eof" 6 "testfile2"))))

  (test
    '(
       "3 / 4 + 5 opt(6)"
       )
    (oops (c)
      (equal? (condition-message c) "parse error at or before character ~s of ~a")
      (equal? (condition-irritants c) '(10 "testfile3"))))

  (test
    '(
       "x = y = 5"
       )
    (returns
      (=
       (0 . 9)
       (id (0 . 1) x)
       (= (4 . 9) (id (4 . 5) y) (int (8 . 9) 5)))))

  (test
    '(
       "x = y = x + 5 - z * 7 + 8 / z"
       )
    (returns
      (=
       (0 . 29)
       (id (0 . 1) x)
       (=
        (4 . 29)
        (id (4 . 5) y)
        (+
         (8 . 29)
         (-
          (8 . 21)
          (+ (8 . 13) (id (8 . 9) x) (int (12 . 13) 5))
          (* (16 . 21) (id (16 . 17) z) (int (20 . 21) 7)))
         (/ (24 . 29) (int (24 . 25) 8) (id (28 . 29) z)))))))

  (test
    '(
       "opt(opt(opt()))"
       )
    (returns
      (OPT (0 . 15) (OPT (4 . 14) (OPT (8 . 13))))))

  (test
    '(
       "kstar(3 4 kplus(1 2 3 kstar()))"
       )
    (returns
      (K* (0 . 31)
        (int (6 . 7) 3)
        (int (8 . 9) 4)
        (K+ (10 . 30)
          (int (16 . 17) 1)
          (int (18 . 19) 2)
          (int (20 . 21) 3)
          (K* (22 . 29))))))

  (test
    '(
       "sepplus( opt() ; opt(5) ; sepstar(17, 34) ; sepstar())"
       )
    (returns
      (SEP+ (0 . 54)
        (OPT (9 . 14))
        (OPT (17 . 23) (int (21 . 22) 5))
        (SEP* (26 . 41) (int (34 . 36) 17) (int (38 . 40) 34))
        (SEP* (44 . 53)))))

  (delete-file "expr.md")
  (printf "~s tests ran\n" n)
  )

;;; Copyright 2017 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; See http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf for origins of 
;;; some of the monadic combinators.

;;; Authors: Jon Rossie, Kent Dybvig

;;; The define-grammar form produces a parser:
;;;
;;;   parser : token-stream -> ((Tree token-stream) ...)
;;;
;;; If the return value is the empty list, a parse error occurred.
;;; If the return value has multiple elements, the parse was ambiguous.
;;; The token-stream in each (Tree token-stream) is the tail of the
;;; input stream that begins with the last token consumed by the parse.
;;; This gives the consumer access to both the first and last token,
;;; allowing it to determine cheaply the extent of the parse, including
;;; source locations if source information is attached to the tokens.

;;; Internally, backtracking occurs whenever a parser return value
;;; has multiple elements.

;;; This code should be included into a lexical context that supplies:
;;;
;;;    token-bfp : token -> token's beginning file position
;;;    token-efp : token -> token's ending file position
;;;    meta constant? : syntax-object -> boolean
;;;    sep->parser : sep -> parser
;;;    constant->parser : constant -> parser
;;;    make-src : bfp x efp -> src.  bfp > efp => no tokens consumed.
;;;
;;; See ez-grammar-test.ss for an example.

(module (define-grammar
          is sat item peek seq ++ +++ many many+ ?
          parse-consumed-all? parse-result-value parse-result-unused
          grammar-trace
          )
  (import (streams))

  (define grammar-trace (make-parameter #f))

  (define-record-type parse-result
    (nongenerative parse-result)
    (sealed #t)
    (fields value unused))

  ;; to enable $trace-is to determine the ending file position (efp) of a parse
  ;; form, the input stream actually points to the preceding token rather than
  ;; to the current token.  the next few routines establish, maintain, and deal
  ;; with that invariant.
  (define make-top-level-parser
    (lambda (parser)
      (lambda (inp)
        (parser (stream-cons 'dummy-token inp)))))

  (define preceding-token
    (lambda (inp)
      (stream-car inp)))

  (define current-token
    (lambda (inp)
      (stream-car (stream-cdr inp))))

  (define remaining-tokens
    (lambda (inp)
      (stream-cdr inp)))

  (define no-more-tokens?
    (lambda (inp)
      (stream-null? (stream-cdr inp))))

  (define parse-consumed-all?
    (lambda (res)
      (no-more-tokens? (parse-result-unused res))))

  ;; A parser generator
  (define result
    (lambda (v)
      ;; this is a parser that ignores its input and produces v
      (lambda (inp)
        (stream (make-parse-result v inp)))))

  ;; A parse that always generates a parse error
  (define zero
    (lambda (inp)
      stream-nil))

  ;; For a non-empty stream, successfully consume the first element
  (define item
    (lambda (inp)
      (cond
        [(no-more-tokens? inp) '()]
        [else
          (stream (make-parse-result (current-token inp) (remaining-tokens inp)))])))

  (define (peek p)
    (lambda (inp)
      (stream-map (lambda (pr)
                    (make-parse-result (parse-result-value pr) inp))
        (p inp))))

  ;;------------------------------------------

  (define bind
    (lambda (parser receiver)
      (lambda (inp)
        (let ([res* (parser inp)])
          (stream-append-all
            (stream-map (lambda (res)
                          ((receiver (parse-result-value res))
                           (parse-result-unused res)))
              res*))))))

  ;; monad comprehensions
  (define-syntax is-where ; used by is and trace-is
    (lambda (x)
      (syntax-case x (where <-)
        [(_ expr (where)) 'expr]
        [(_ expr (where [x <- p] clauses ...))
         '(bind p (lambda (x) (is-where expr (where clauses ...))))]
        [(_ expr (where pred clauses ...))
         '(if pred (is-where expr (where clauses ...)) zero)]
        [(_ expr where-clause) (syntax-error 'where-clause)])))
  (indirect-export is-where bind)

  (define-syntax is
    (syntax-rules ()
      [(_ expr where-clause) (is-where (result expr) where-clause)]))
  (indirect-export is is-where)

  (module (trace-is)
    (define ($trace-is name proc head)
      (lambda (unused)
        (let ([res (proc (token-bfp (current-token head)) (token-efp (preceding-token unused)))])
          (when (and name (grammar-trace)) (printf "<<~s = ~s~%" name res))
          (stream (make-parse-result res unused)))))

    (define-syntax trace-is
      (syntax-rules ()
        [(_ name proc-expr where-clause)
         (lambda (inp) ((is-where ($trace-is 'name proc-expr inp) where-clause) inp))]))
    (indirect-export trace-is $trace-is))

  (define (seq2 p q) (is (cons x y) (where [x <- p] [y <- q])))

  (define seq
    (lambda p*
      (let loop ([p* p*])
        (cond
          [(null? p*) (result '())]
          [else (seq2 (car p*) (loop (cdr p*)))]))))

  (define (sat pred) (is x (where [x <- item] (pred x))))

  (define + ;; introduce ambiguity
    (lambda (p q)
      (lambda (inp)
        (stream-append2 (p inp)
          (lambda ()
            (q inp))))))

  (define (many+ p) (is (cons x xs) (where [x <- p] [xs <- (many p)])))

  (define (many p) (++ (many+ p) (result '())))

  (define (? p) (++ (sat p) (result #f)))

  (define (sepby1 p sep)
    (is (cons x xs)
      (where
        [x <- p]
        [xs <- (many (is y (where [_ <- sep] [y <- p])))])))

  (define (sepby p sep) (++ (sepby1 p sep) (result '())))

  (define (bracket open p close) (is x (where [_ <- open] [x <- p] [_ <- close])))

  (define (optional p default)
    (lambda (inp)
      (let ([res (p inp)])
        (if (stream-null? res)
            (stream (make-parse-result default inp))
            res))))

  (define (first p)
    (lambda (inp)
      (let ([res (p inp)])
        (if (stream-null? res)
            res
            (stream (stream-car res))))))

  (define-syntax infix-expression-parser
    (lambda (x)
      (syntax-case x ()
        [(_ ((L/R ?op-parser) ...) ?term-parser ?receiver)
         (with-syntax ([(op-parser ...) (generate-temporaries '(?op-parser ...))])
           `(let ([op-parser ?op-parser] ... [term-parser (lambda (inp) (?term-parser inp))] [receiver ?receiver])
               ,(let f ([ls '((L/R op-parser) ...)])
                   (if (null? ls)
                       'term-parser
                       `(let ([next ,(f (cdr ls))])
                           ,(syntax-case (car ls) (LEFT RIGHT)
                               [(LEFT op-parser)
                                '(let ()
                                    (define-record-type frob (nongenerative) (sealed #t) (fields op y efp))
                                    (trace-is binop-left (lambda (bfp ignore-this-efp)
                                                           (fold-left
                                                             (lambda (x f) (receiver bfp (frob-efp f) (frob-op f) x (frob-y f)))
                                                             x f*))
                                      (where
                                        [x <- next]
                                        [f* <- (rec this
                                                 (optional
                                                   (is (cons f f*)
                                                     (where
                                                       [f <- (trace-is binop-left-tail (lambda (bfp efp) (make-frob op y efp))
                                                               (where
                                                                 [op <- op-parser]
                                                                 [y <- next]))]
                                                       [f* <- this]))
                                                   '()))])))]
                               [(RIGHT op-parser)
                                '(rec this
                                    (+++
                                      (trace-is binop-right (lambda (bfp efp) (receiver bfp efp op x y))
                                        (where
                                          [x <- next]
                                          [op <- op-parser]
                                          [y <- this]))
                                      next))]))))))])))

  (define (format-inp inp)
    (if (no-more-tokens? inp)
        "#<null-stream>"
        (format "(~s ...)" (current-token inp))))

  (define-syntax define-grammar
    (lambda (x)
      (define-record-type grammar
        (nongenerative)
        (sealed #t)
        (fields title paragraph* section*))
      (define-record-type section
        (nongenerative)
        (sealed #t)
        (fields title paragraph* suppressed? clause*))
      (define-record-type clause
        (nongenerative)
        (fields id alias* before-paragraph* after-paragraph*))
      (define-record-type regular-clause
        (nongenerative)
        (sealed #t)
        (parent clause)
        (fields prod*))
      (define-record-type binop-clause
        (nongenerative)
        (sealed #t)
        (parent clause)
        (fields level* term receiver)
        (protocol
          (lambda (pargs->new)
            (lambda (nt alias* before-paragraph* after-paragraph* level* term src? receiver)
              ((pargs->new nt alias* before-paragraph* after-paragraph*) level* term
                `(lambda (bfp efp op x y)
                    ,(if src?
                          `(,receiver (make-src bfp efp) op x y)
                          `(,receiver op x y))))))))
      (define-record-type terminal-clause
        (nongenerative)
        (sealed #t)
        (fields term*))
      (define-record-type terminal
        (nongenerative)
        (sealed #t)
        (fields parser alias* paragraph*))
      (define-record-type production
        (nongenerative)
        (sealed #t)
        (fields name paragraph* elt* receiver)
        (protocol
          (let ()
            (define (check-elts elt*)
              (for-each (lambda (elt) (unless (elt? elt) (errorf 'make-production "~s is not an elt" elt))) elt*))
            (lambda (new)
              (case-lambda
                [(name elt* receiver)
                 (check-elts elt*)
                 (new name #f elt* receiver)]
                [(name paragraph* elt* receiver)
                 (check-elts elt*)
                 (new name paragraph* elt* receiver)])))))
      (define-record-type elt
        (nongenerative))
      (define-record-type sep-elt
        (nongenerative)
        (sealed #t)
        (parent elt)
        (fields +? elt sep))
      (define-record-type opt-elt
        (nongenerative)
        (sealed #t)
        (parent elt)
        (fields elt default))
      (define-record-type kleene-elt
        (nongenerative)
        (sealed #t)
        (parent elt)
        (fields +? elt))
      (define-record-type constant-elt
        (nongenerative)
        (sealed #t)
        (parent elt)
        (fields k))
      (define-record-type id-elt
        (nongenerative)
        (sealed #t)
        (parent elt)
        (fields id))
      (define paragraph?
        (lambda (x)
          (syntax-case x (include)
            [(include filename) (string? (datum filename))]
            [(str  ...) (andmap string? (datum (str ...)))])))
      (define (gentemp) (datum->syntax '* (gensym)))
      (define (elt-temps elt*)
        (for-each (lambda (elt) (unless (elt? elt) (errorf 'elt-temps "~s is not an elt" elt))) elt*)
        (fold-left
          (lambda (t* elt)
            (if (constant-elt? elt) t* (cons (gentemp) t*)))
          '()
          elt*))
      (define (left-factor clause*)
        (define syntax-equal?
          (lambda (x y)
            (equal? (syntax->datum x) (syntax->datum y))))
        (define (elt-equal? x y)
          (cond
            [(sep-elt? x) 
             (and (sep-elt? y)
                  (eq? (sep-elt-+? x) (sep-elt-+? y))
                  (elt-equal? (sep-elt-elt x) (sep-elt-elt y))
                  (syntax-equal? (sep-elt-sep x) (sep-elt-sep y)))]
            [(opt-elt? x) 
             (and (opt-elt? y)
                  (elt-equal? (opt-elt-elt x) (opt-elt-elt y))
                  (syntax-equal? (opt-elt-default x) (opt-elt-default y)))]
            [(kleene-elt? x) 
             (and (kleene-elt? y)
                  (eq? (kleene-elt-+? x) (kleene-elt-+? y))
                  (elt-equal? (kleene-elt-elt x) (kleene-elt-elt y)))]
            [(constant-elt? x)
             (and (constant-elt? y)
                  (syntax-equal? (constant-elt-k x) (constant-elt-k y)))]
            [(id-elt? x)
             (and (id-elt? y)
                  (syntax-equal? (id-elt-id x) (id-elt-id y)))]
            [else #f]))
        (let lp1 ([clause* clause*] [new-clause* '()])
          (if (null? clause*)
              (reverse new-clause*)
              (let ([clause (car clause*)])
                (let lp2 ([prod* (regular-clause-prod* clause)] [new-prod* '()] [clause* (cdr clause*)])
                  (if (null? prod*)
                      (lp1 clause* (cons (make-regular-clause (clause-id clause) (clause-alias* clause) '() '() (reverse new-prod*)) new-clause*))
                      (let ([prod (car prod*)] [prod* (cdr prod*)])
                        (let ([elt* (production-elt* prod)])
                          (if (null? elt*)
                              (lp2 prod* (cons prod new-prod*) clause*)
                              (let ([elt (car elt*)])
                                (let-values ([(haves have-nots) (partition
                                                                  (lambda (prod)
                                                                    (let ([elt* (production-elt* prod)])
                                                                      (and (not (null? elt*))
                                                                           (elt-equal? (car elt*) elt))))
                                                                  prod*)])
                                  (if (null? haves)
                                      (lp2 prod* (cons prod new-prod*) clause*)
                                      (let ([haves (cons prod haves)])
                                        ; "haves" start with the same elt.  to cut down on the number of new
                                        ; nonterminals and receiver overhead, find the largest common prefix
                                        (let ([prefix (cons elt
                                                        (let f ([elt** (map production-elt* haves)])
                                                          (let ([elt** (map cdr elt**)])
                                                            (if (ormap null? elt**)
                                                                '()
                                                                (let ([elt (caar elt**)])
                                                                  (if (andmap (lambda (elt*) (elt-equal? (car elt*) elt)) (cdr elt**))
                                                                      (cons elt (f elt**))
                                                                      '()))))))])
                                          (let ([t (gentemp)] [n (length prefix)] [t* (elt-temps prefix)])
                                            (lp2 have-nots
                                              (cons (make-production #f (append prefix (list (make-id-elt t)))
                                                      `(lambda (bfp efp ,@t* p) (p bfp ,@t*)))
                                                new-prod*)
                                              (cons (make-regular-clause t '() '() '()
                                                      (map (lambda (prod)
                                                             (let ([elt* (list-tail (production-elt* prod) n)])
                                                               (make-production (production-name prod) elt*
                                                                 (let ([u* (elt-temps elt*)])
                                                                   `(lambda (bfp efp ,@u*)
                                                                       (lambda (bfp ,@t*)
                                                                         (,(production-receiver prod) bfp efp ,@t* ,@u*)))))))
                                                        haves))
                                                clause*)))))))))))))))))
      (define (make-env tclause* clause*)
        (let ([env (make-hashtable (lambda (x) (symbol-hash (syntax->datum x))) free-identifier=?)])
          (define (insert parser)
            (lambda (name)
              (let ([a (hashtable-cell env name #f)])
                (when (cdr a) (syntax-error name "duplicate terminal/non-terminal name"))
                (set-cdr! a parser))))
          (for-each
            (lambda (tclause)
              (for-each
                (lambda (term)
                  (let ([parser (terminal-parser term)])
                    (for-each (insert parser) (cons parser (terminal-alias* term)))))
                (terminal-clause-term* tclause)))
            tclause*)
          (for-each
            (lambda (clause)
              (let ([id (clause-id clause)])
                (for-each (insert id) (cons id (clause-alias* clause)))))
            clause*)
          env))
      (define (lookup id env)
        (or (hashtable-ref env id #f)
            (syntax-error id "unrecognized terminal or nonterminal")))
      (define (render-markdown name grammar mdfn env)
        (define (separators sep ls)
          (if (null? ls)
              ""
              (apply string-append
                (cons (car ls)
                  (map (lambda (s) (format "~a~a" sep s)) (cdr ls)))))) 
        (define (render-paragraph hard-leading-newline?)
          (lambda (paragraph)
            (define (md-text s)
              (list->string
                (fold-right
                  (lambda (c ls)
                    (case c
                      [(#\\) (cons* c c ls)]
                      [else (cons c ls)]))
                  '()
                  (string->list s))))
            (syntax-case paragraph (include)
              [(include filename)
               (string? (datum filename))
               (let ([text (call-with-port (open-input-file (datum filename)) get-string-all)])
                 (unless (equal? text "")
                   (if hard-leading-newline? (printf "\\\n") (newline))
                   (display-string text)))]
              [(sentence ...)
               (andmap string? (datum (sentence ...)))
               (let ([sentence* (datum (sentence ...))])
                 (unless (null? sentence*)
                   (if hard-leading-newline? (printf "\\\n") (newline))
                   (printf "~a\n" (separators " " (map md-text sentence*)))))])))
        (define (format-elt x)
          (cond
            [(sep-elt? x)
             (let* ([one (format-elt (sep-elt-elt x))]
                    [sep (constant->markdown (syntax->datum (sep-elt-sep x)))]
                    [seq (format "~a&nbsp;&nbsp;~a&nbsp;&nbsp;`...`" one sep)])
               (if (sep-elt-+? x)
                   seq
                   (format "OPT(~a)" seq)))]
            [(opt-elt? x)
             (format "~a~~opt~~" (format-elt (opt-elt-elt x)))]
            [(kleene-elt? x)
             (let ([one (format-elt (kleene-elt-elt x))])
               (if (kleene-elt-+? x)
                   (format "~a&nbsp;&nbsp;`...`" one)
                   (format "OPT(~a)" one)))]
            [(constant-elt? x) (constant->markdown (syntax->datum (constant-elt-k x)))]
            [(id-elt? x) (format "[*~s*](#~s)"
                           (syntax->datum (id-elt-id x))
                           (syntax->datum (lookup (id-elt-id x) env)))]
            [else (errorf 'format-elt "unexpected elt ~s" x)]))
        (define (render-elt x)
          (printf "&nbsp;&nbsp;~a" (format-elt x)))
        (define (render-production prod)
          (unless (null? (production-elt* prod))
            (printf "  : ")
            (for-each render-elt (production-elt* prod))
            (printf "\n"))
          (when (and (null? (production-elt* prod))
                     (not (null? (production-paragraph* prod))))
            (errorf 'render-production "empty production must not have description: ~a" (production-paragraph* prod)))
          (for-each (render-paragraph #t) (production-paragraph* prod)))
        (define (render-clause clause)
          (define (render-aliases alias*)
            (unless (null? alias*)
              (printf "  \naliases: ~{*~a*~^, ~}\n" (map syntax->datum alias*))))
          (if (terminal-clause? clause)
              (for-each
                (lambda (term)
                  (printf "\n#### *~a* {#~:*~a}\n" (syntax->datum (terminal-parser term)))
                  (render-aliases (terminal-alias* term))
                  (for-each (render-paragraph #f) (terminal-paragraph* term)))
                (terminal-clause-term* clause))
              (let ([id (syntax->datum (clause-id clause))])
                (printf "\n#### *~a* {#~:*~a}\n" id)
                (render-aliases (clause-alias* clause))
                (for-each (render-paragraph #f) (clause-before-paragraph* clause))
                (printf "\nsyntax:\n")
                (if (binop-clause? clause)
                    (let ([level* (binop-clause-level* clause)])
                      (let loop ([level* level*] [first? #t])
                        (unless (null? level*)
                          (let ([level (syntax->datum (car level*))] [level* (cdr level*)])
                            (let ([L/R (car level)] [op* (cdr level)])
                              (printf "  : _~(~a~)-associative" L/R)
                              (if first?
                                  (if (null? level*)
                                      (printf ":_\n")
                                      (printf ", highest precedence:_\n"))
                                  (if (null? level*)
                                      (printf ", lowest precedence:_\n")
                                      (printf ":_\n")))
                              (for-each
                                (lambda (op) (printf "    : ~s ~a ~s\n" id (constant->markdown op) id))
                                op*))
                            (loop level* #f))))
                      (printf "  : _leaves:_\n")
                      (printf "    : ")
                      (render-elt (binop-clause-term clause))
                      (printf "\n"))
                    (for-each render-production (or (regular-clause-prod* clause) '())))
                (for-each (render-paragraph #f) (clause-after-paragraph* clause)))))
        (define (render-section section)
          (unless (section-suppressed? section)
            (printf "\n## ~a\n" (or (section-title section) "The section"))
            (for-each (render-paragraph #f) (section-paragraph* section))
            (for-each render-clause (section-clause* section))))
        (with-output-to-file mdfn
          (lambda ()
            (printf "# ~a\n" (or (grammar-title grammar) "The grammar"))
            (for-each (render-paragraph #f) (grammar-paragraph* grammar))
            (for-each render-section (grammar-section* grammar)))
          'replace))
      (module (parse-grammar)
        (define parse-elt
          (lambda (elt)
            (syntax-case elt (SEP+ SEP* OPT K* K+)
              [(SEP+ p sep) (make-sep-elt #t (parse-elt 'p) 'sep)]
              [(SEP* p sep) (make-sep-elt #f (parse-elt 'p) 'sep)]
              [(OPT p default) (make-opt-elt (parse-elt 'p) 'default)]
              [(K+ p) (make-kleene-elt #t (parse-elt 'p))]
              [(K* p) (make-kleene-elt #f (parse-elt 'p))]
              [k (constant? 'k) (make-constant-elt 'k)]
              [id (identifier? 'id) (make-id-elt 'id)]
              [_ (syntax-error elt "invalid production element")])))
        (define parse-production
          (lambda (prod)
            (define (finish name src? paragraph* elt* receiver)
              (let ([elt* (map parse-elt elt*)])
                (make-production name paragraph* elt*
                  (with-syntax ([(t ...) (elt-temps elt*)])
                    `(lambda (bfp efp t ...)
                        ,(if src?
                              `(,receiver (make-src bfp efp) t ...)
                              `(,receiver t ...)))))))
            (syntax-case prod (:: src =>)
              [[name :: src elt ... => receiver]
               (finish 'name #t '() '(elt ...) 'receiver)]
              [[name :: elt ... => receiver]
               (finish 'name #f '() '(elt ...) 'receiver)])))
        (define (parse-terminal term)
          (syntax-case term (DESCRIPTION)
            [(parser (alias ...) (DESCRIPTION paragraph ...))
             (and (identifier? 'parser) (andmap identifier? '(alias ...)) (andmap paragraph? '(paragraph ...)))
             (make-terminal 'parser '(alias ...) '(paragraph ...))]
            [(parser (alias ...))
             (and (identifier? 'parser) (andmap identifier? '(alias ...)))
             (make-terminal 'parser '(alias ...) '())]))
        (define (parse-clause clause nt alias* before-paragraph* after-paragraph* stuff*)
          (syntax-case stuff* (BINOP :: src =>)
            [((BINOP src (level ...) term) => receiver)
             (make-binop-clause nt alias* before-paragraph* after-paragraph* '(level ...) (parse-elt 'term) #t 'receiver)]
            [((BINOP (level ...) term) => receiver)
             (make-binop-clause nt alias* before-paragraph* after-paragraph* '(level ...) (parse-elt 'term) #f 'receiver)]
            [(prod prods ...)
             (make-regular-clause nt alias* before-paragraph* after-paragraph* (map parse-production '(prod prods ...)))]
            [else (syntax-error clause)]))
        (define (parse-top top* knull kgrammar ksection kclause)
          (if (null? top*)
              (knull)
              (let ([top (car top*)] [top* (cdr top*)])
                (syntax-case top (GRAMMAR SECTION SUPPRESSED DESCRIPTION BINOP TERMINALS src =>)
                  [(GRAMMAR title paragraph ...)
                   (andmap paragraph? '(paragraph ...))
                   (kgrammar top* (datum title) '(paragraph ...))]
                  [(SECTION SUPPRESSED title paragraph ...)
                   (andmap paragraph? '(paragraph ...))
                   (ksection top* (datum title) '(paragraph ...) #t)]
                  [(SECTION title paragraph ...)
                   (andmap paragraph? '(paragraph ...))
                   (ksection top* (datum title) '(paragraph ...) #f)]
                  [(TERMINALS term ...)
                   (kclause top* (make-terminal-clause (map parse-terminal '(term ...))))]
                  [(TERMINALS term ...)
                   (kclause top* (make-terminal-clause (map parse-terminal '(term ...))))]
                  [(nt (alias ...) (DESCRIPTION paragraph1 ...) stuff ... (DESCRIPTION paragraph2 ...))
                   (and (identifier? 'nt) (andmap identifier? '(alias ...)) (andmap paragraph? '(paragraph1 ...)) (andmap paragraph? '(paragraph2 ...)))
                   (kclause top* (parse-clause top 'nt '(alias ...) '(paragraph1 ...) '(paragraph2 ...) '(stuff ...)))]
                  [(nt (alias ...) (DESCRIPTION paragraph ...) stuff ...)
                   (and (identifier? 'nt) (andmap identifier? '(alias ...)) (andmap paragraph? '(paragraph ...)))
                   (kclause top* (parse-clause top 'nt '(alias ...) '(paragraph ...) '() '(stuff ...)))]
                  [(nt (alias ...) stuff ... (DESCRIPTION paragraph ...))
                   (and (identifier? 'nt) (andmap identifier? '(alias ...)) (andmap paragraph? '(paragraph ...)))
                   (kclause top* (parse-clause top 'nt '(alias ...) '() '(paragraph ...) '(stuff ...)))]
                  [(nt (alias ...) stuff ...)
                   (and (identifier? 'nt) (andmap identifier? '(alias ...)))
                   (kclause top* (parse-clause top 'nt '(alias ...) '() '() '(stuff ...)))]))))
        (define (parse-grammar top*)
          (define (misplaced-grammar-error top)
            (syntax-error top "unexpected GRAMMAR element after other elements"))
          (define (s1 top*) ; looking for GRAMMAR form, first SECTION form, or clause
            (parse-top top*
              (lambda () (make-grammar #f '() '()))
              (lambda (top* title paragraph*)
                (make-grammar title paragraph* (s2 top*)))
              (lambda (top* title paragraph* suppressed?)
                (make-grammar #f '()
                  (s3 top* title paragraph* suppressed? '() '())))
              (lambda (top* clause)
                (make-grammar #f '()
                  (s3 top* #f '() #f (list clause) '())))))
          (define (s2 top*) ; looking for first SECTION form or clause
            (parse-top top*
              (lambda () '())
              (lambda (title paragraph*) (misplaced-grammar-error (car top*)))
              (lambda (top* title paragraph* suppressed?)
                (s3 top* title paragraph* suppressed? '() '()))
              (lambda (top* clause)
                (s3 top* #f '() #f (list clause) '()))))
          (define (s3 top* title paragraph* suppressed? rclause* rsection*) ; steady state: looking for remaining SECTION forms and clauses
            (define (finish-section)
              (cons (make-section title paragraph* suppressed? (reverse rclause*)) rsection*))
            (parse-top top*
              (lambda () (reverse (finish-section)))
              (lambda (title paragraph*) (misplaced-grammar-error (car top*)))
              (lambda (top* title paragraph* suppressed?)
                (s3 top* title paragraph* suppressed? '() (finish-section)))
              (lambda (top* clause)
                (s3 top* title paragraph* suppressed? (cons clause rclause*) rsection*))))
          (s1 top*)))
      (define (go init-nts top* mddir)
        (let ([grammar (parse-grammar top*)])
          (let* ([clause* (apply append (map section-clause* (grammar-section* grammar)))]
                 [terminal-clause* (filter terminal-clause? clause*)]
                 [binop-clause* (filter binop-clause? clause*)]
                 [regular-clause* (left-factor (filter regular-clause? clause*))]
                 [env (make-env terminal-clause* (append binop-clause* regular-clause*))])
            (define (elt-helper x)
              (cond
                [(sep-elt? x) `(,(if (sep-elt-+? x) 'sepby1 'sepby) ,(elt-helper (sep-elt-elt x)) (sep->parser ,(sep-elt-sep x)))]
                [(opt-elt? x) `(optional ,(elt-helper (opt-elt-elt x)) ,(opt-elt-default x))]
                [(kleene-elt? x) `(,(if (kleene-elt-+? x) 'many+ 'many) ,(elt-helper (kleene-elt-elt x)))]
                [(constant-elt? x) `(constant->parser ',(constant-elt-k x))]
                [(id-elt? x) (lookup (id-elt-id x) env)]
                [else (errorf 'elt-helper "unhandled elt ~s\n" x)]))
            (define (binop-helper clause)
              `[,(clause-id clause)
                  (infix-expression-parser
                    ,(map (lambda (level)
                             (syntax-case level ()
                               [(L/R op1 ... op2)
                                (or (free-identifier=? 'L/R 'LEFT) (free-identifier=? 'L/R 'RIGHT))
                                `(L/R ,(fold-right (lambda (op next)) '(binop->parser 'op2) '(op1 ...)))]))
                        (binop-clause-level* clause))
                    ,(elt-helper (binop-clause-term clause))
                    ,(binop-clause-receiver clause))])
            (define (nt-helper clause)
              `[,(clause-id clause)
                  ,(let f ([prod* (regular-clause-prod* clause)])
                      (if (null? prod*)
                          'zero
                          (let ([elt* (production-elt* (car prod*))])
                            (with-syntax ([name (production-name (car prod*))]
                                          [(elt ...) elt*]
                                          [receiver (production-receiver (car prod*))])
                              (with-syntax ([(x ...) (generate-temporaries elt*)])
                                (with-syntax ([([y _] ...) (filter (lambda (pr) (not (constant-elt? (cadr pr)))) '([x elt] ...))])
                                  (with-syntax ([(where-nt ...) (map elt-helper elt*)]))))))))])
            (with-syntax ([(init-nt ...)
                           (syntax-case init-nts ()
                             [(id1 id2 ...) (andmap identifier? '(id1 id2 ...)) '(id1 id2 ...)]
                             [id (identifier? 'id) (list 'id)])])
              (when mddir
                (for-each
                  (lambda (init-nt)
                    (let ([mdfn (format "~a/~a.md" mddir (syntax->datum init-nt))])
                      (render-markdown init-nt grammar mdfn env)))
                  '(init-nt ...)))
              (with-syntax ([((lhs rhs) ...)
                             (append
                               (map binop-helper binop-clause*)
                               (map nt-helper regular-clause*))])
                '(module (init-nt ...)
                    (module M (init-nt ...) (define lhs rhs) ...)
                    (define init-nt
                      (let ()
                        (import M)
                        (make-top-level-parser init-nt)))
                    ...))))))
      (syntax-case x (markdown-directory)
        [(_ init-nts (markdown-directory mddir) top ...)
         (string? (datum mddir))
         (go 'init-nts '(top ...) (datum mddir))]
        [(_ init-nts top ...) (go 'init-nts '(top ...) #f)])))

  (indirect-export define-grammar
    result
    zero
    is
    trace-is
    sepby1
    sepby
    optional
    many
    many+
    +++
    infix-expression-parser

    grammar-trace
    format-inp
    trace-is

    make-top-level-parser
  )
)
;;; simple factorial function

;;; it is interesting to change the 'lambda' into 'trace-lambda'
;;; or simply type (trace fact) before running fact to observe
;;; the nesting of recursive calls.

(define fact
   (lambda (x)
      (if (zero? x)
          1
          (* x (fact (1- x))))))
;;; fat fibonacci function

;;; this is "fat" because it uses only increments and decrements
;;; for addition and subtraction (i.e., peano arithmetic).

;;; note that fat+ is tail-recursive; this is how all looping is
;;; performed in Scheme.

(define fat+
   (lambda (x y)
      (if (zero? y)
          x
          (fat+ (+ x) (1- y)))))

(define fatfib
   (lambda (x)
      (if (< x 2)
          1
          (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
;;; fft.ss
;;; Copyright (C) 1996 R. Kent Dybvig
;;; from "The Scheme Programming Language, 2ed" by R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

(define (dft x)
  (define (w-powers n)
    (let ((pi (* (acos 0.0) 2)))
      (let ((delta (/ (* -2.0i pi) n)))
        (let f ((n n) (x 0.0))
          (if (= n 0)
              '()
              (cons (exp x) (f (- n 2) (+ x delta))))))))
  (define (evens w)
    (if (null? w)
        '()
        (cons (car w) (evens (cddr w)))))
  (define (interlace x y)
    (if (null? x)
        '()
        (cons (car x) (cons (car y) (interlace (cdr x) (cdr y))))))
  (define (split ls)
    (let split ((fast ls) (slow ls))
      (if (null? fast)
          (values '() slow)
          (call-with-values
            (lambda () (split (cddr fast) (cdr slow)))
            (lambda (front back)
              (values (cons (car slow) front) back))))))
  (define (butterfly x w)
    (call-with-values
      (lambda () (split x))
      (lambda (front back)
        (values
          (map + front back)
          (map * (map - front back) w)))))
  (define (rfft x w)
    (if (null? (cddr x))
      (let ((x0 (car x)) (x1 (cadr x)))
        (list (+ x0 x1) (- x0 x1)))
      (call-with-values
        (lambda () (butterfly x w))
        (lambda (front back)
          (let ((w (evens w)))
            (interlace (rfft front w) (rfft back w)))))))
  (rfft x (w-powers (length x))))
;;; simple fibonacci function

;;; uses trace-lambda to show the nesting

(define fib
   (trace-lambda fib (x)
      (if (<= x 1)
          1
          (+ (fib (- x 1)) (fib (- x 2))))))
;;; foreign.ss
;;; Copyright (c) 1997 R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; Prototype code for converting ``foreign-callable'' declarations into
;;; C interface routines to support C calls to Scheme procedures with
;;; automatic datatype conversion analogous to that provided for Scheme
;;; calls to C procedures via foreign-procedure.

;;; Todo
;;;   - support for foreign-pointer and foreign-object
;;;   - similar support for foreign-procedure declarations

(define spec->decl
  (lambda (spec)
    (case spec
      [(integer-32 boolean) "int"]
      [(unsigned-32) "unsigned int"]
      [(char) "char"]
      [(string) "char *"]
      [(fixnum) "int"]
      [(double-float) "double"]
      [(single-float) "float"]
      [(void) "void"]
      [(scheme-object) "ptr"]
      [else
       (record-case spec
         [(foreign-pointer foreign-object) ()
          (error 'spec->decl "unsupported specifier ~s" spec)]
         [else (error 'spec->decl "unexpected specifier ~s" spec)])])))

(define C->Scheme
  (lambda (spec id)
    (case spec
      [(boolean) (format "Sboolean(~a)" id)]
      [(char) (format "Schar(~a)" id)]
      [(fixnum) (format "Sfixnum(~a)" id)]
      [(integer-32) (format "Sinteger(~a)" id)]
      [(unsigned-32) (format "Sunsigned(~a)" id)]
      [(single-float) (format "Sflonum((double)~a)" id)]
      [(double-float) (format "Sflonum(~a)" id)]
      [(scheme-object) id]
      [(string) (format "Sstring(~a)" id)]
      [else
       (record-case spec
         [(foreign-pointer foreign-object) ()
          (error 'C->Scheme "unsupported specifier ~s" spec)]
         [else (error 'C->Scheme "unexpected specifier ~s" spec)])])))

(define Scheme->C
  (lambda (op spec src)
    (case spec
      [(boolean) (fprintf op "Sboolean_value(~a)" src)]
      [(char) (fprintf op "Schar_value(~a)" src)]
      [(fixnum) (fprintf op "Sfixnum_value(~a)" src)]
      [(integer-32) (fprintf op "Sinteger_value(~a)" src)]
      [(unsigned-32) (fprintf op "Sunsigned_value(~a)" src)]
      [(single-float) (fprintf op "(float)Sflonum_value(~a)" src)]
      [(double-float) (fprintf op "Sflonum_value(~a)" src)]
      [(scheme-object) (display src op)]
      [(string) (fprintf op "Sstring_value(~a)" src)]
      [else
       (record-case spec
         [(foreign-pointer foreign-object) ()
          (error 'Scheme->C "unsupported specifier ~s" spec)]
         [else (error 'Scheme->C "unexpected specifier ~s" spec)])])))

(define gen-fcallable
  (case-lambda
    [(cname arg-specs res-spec)
     (gen-fcallable (current-output-port) cname arg-specs res-spec)]
    [(op cname arg-specs res-spec)
     (let ((names (let loop ((ls arg-specs) (i 1))
                    (if (null? ls)
                        '()
                        (cons (format "x~d" i) (loop (cdr ls) (+ i 1))))))
           (count (length arg-specs)))
       (newline op)
       (fprintf op "~a ~a(ptr proc" (spec->decl res-spec) cname) ;)
       (let loop ((arg-specs arg-specs) (names names))
         (unless (null? arg-specs)
           (fprintf op ", ~a ~a" (spec->decl (car arg-specs)) (car names))
           (loop (cdr arg-specs) (cdr names)))) ;(
       (fprintf op ") {~%")
       (if (<= 0 count 3)
           (begin
             (display "    return " op)
             (Scheme->C op res-spec
               (let ((op (open-output-string)))
                 (fprintf op "Scall~d(proc" count) ;)
                 (let loop ((arg-specs arg-specs) (names names))
                   (unless (null? arg-specs)
                     (display ", " op)
                     (display (C->Scheme (car arg-specs) (car names)) op)
                     (loop (cdr arg-specs) (cdr names)))) ;(
                 (fprintf op ")")
                 (get-output-string op))))
             (begin
               (fprintf op "    Sinitframe(~d);~%" count)
               (let loop ([arg-specs arg-specs] [names names] [num 1])
                 (unless (null? arg-specs)
                   (fprintf op "    Sput_arg(~d, ~a);~%"
                      num (C->Scheme (car arg-specs) (car names)))
                   (loop (cdr arg-specs) (cdr names) (+ num 1))))
               (fprintf op "    return ")
               (Scheme->C op res-spec
                 (format "Scall(proc, ~d)" count))))
       (fprintf op ";~%}~%"))]))

(define-syntax foreign-callable
  (syntax-rules ()
    ((_ n args res)
     (gen-fcallable n 'args 'res))))

(define gen-file
  (lambda (fnroot)
    (let ((ifn (format "~a.ss" fnroot))
          (ofn (format "~a.xx" fnroot)))
      (with-output-to-file ofn
        (lambda () (load ifn))
        'replace))))

;;; Copyright (C) 1996 R. Kent Dybvig
;;; from "The Scheme Programming Language, 2ed" by R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; If the next character on p is a letter, get-word reads a word
;;; from p and returns it in a string.  If the character is not a
;;; letter, get-word returns the character (on eof, the eof-object).
(define get-word
  (lambda (p)
    (let ((c (read-char p)))
      (if (eq? (char-type c) 'letter)
          (list->string
            (let loop ((c c))
              (cons c
                (if (memq (char-type (peek-char p)) '(letter digit))
                    (loop (read-char p))
                    '()))))
          c))))

;;; char-type tests for the eof-object first, since the eof-object
;;; may not be a valid argument to char-alphabetic? or char-numeric?
;;; It returns the eof-object, the symbol letter, the symbol digit,
;;; or the argument itself if it is not a letter or digit.
(define char-type
  (lambda (c)
    (cond
      ((eof-object? c) c)
      ((char-alphabetic? c) 'letter)
      ((char-numeric? c) 'digit)
      (else c))))

;;; Trees are represented as vectors with four fields: word, left,
;;; right, and count.  Only one field, word, is initialized by an
;;; argument to the constructor procedure make-tree.  The remaining
;;; fields are explicitly initialized and changed by subsequent
;;; operations.  Most Scheme systems provide structure definition
;;; facilities that automate creation of structure manipulation
;;; procedures, but we simply define the procedures by hand here.
(define make-tree
  (lambda (word)
    (vector word '() '() 1)))

(define tree-word (lambda (tree) (vector-ref tree 0)))

(define tree-left (lambda (tree) (vector-ref tree 1)))
(define set-tree-left!
  (lambda (tree new-left)
    (vector-set! tree 1 new-left)))

(define tree-right (lambda (tree) (vector-ref tree 2)))
(define set-tree-right!
  (lambda (tree new-right)
    (vector-set! tree 2 new-right)))

(define tree-count (lambda (tree) (vector-ref tree 3)))
(define set-tree-count!
  (lambda (tree new-count)
    (vector-set! tree 3 new-count)))

;;; If the word already exists in the tree, tree increments its
;;; count.  Otherwise, a new tree node is created and put into the
;;; tree.  In any case, the new or modified tree is returned.
(define tree
  (lambda (node word)
    (cond
      ((null? node) (make-tree word))
      ((string=? word (tree-word node))
       (set-tree-count! node (+ (tree-count node) 1))
       node)
      ((string<? word (tree-word node))
       (set-tree-left! node (tree (tree-left node) word))
       node)
      (else
       (set-tree-right! node (tree (tree-right node) word))
       node))))

;;; tree-print prints the tree in "in-order," i.e., left subtree,
;;; then node, then right subtree.  For each word, the count and the
;;; word are printed on a single line.
(define tree-print
  (lambda (node p)
    (if (not (null? node))
        (begin
          (tree-print (tree-left node) p)
          (write (tree-count node) p)
          (write-char #\space p)
          (display (tree-word node) p)
          (newline p)
          (tree-print (tree-right node) p)))))

;;; frequency is the driver routine.  It opens the files, reads the
;;; words, and enters them into the tree.  When the input port
;;; reaches end-of-file, it prints the tree and closes the ports.
(define frequency
  (lambda (infn outfn)
    (let ((ip (open-input-file infn))
          (op (open-output-file outfn)))
      (let loop ((root '()))
        (let ((w (get-word ip)))
          (cond
             ((eof-object? w) (tree-print root op))
             ((string? w) (loop (tree root w)))
             (else (loop root)))))
       (close-input-port ip)
       (close-output-port op))))
;;; interpret.ss
;;; Copyright (C) 1996 R. Kent Dybvig
;;; from "The Scheme Programming Language, 2ed" by R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

(define interpret #f)
(let ()
  ;; primitive-environment is an environment containing a small
  ;; number of primitive procedures; it can be extended easily
  ;; to include additional primitives.
  (define primitive-environment
    (list (cons 'apply apply)
          (cons 'assq assq)
          (cons 'call/cc call/cc)
          (cons 'car car)
          (cons 'cadr cadr)
          (cons 'caddr caddr)
          (cons 'cadddr cadddr)
          (cons 'cddr cddr)
          (cons 'cdr cdr)
          (cons 'cons cons)
          (cons 'eq? eq?)
          (cons 'list list)
          (cons 'map map)
          (cons 'memv memv)
          (cons 'null? null?)
          (cons 'pair? pair?)
          (cons 'read read)
          (cons 'set-car! set-car!)
          (cons 'set-cdr! set-cdr!)
          (cons 'symbol? symbol?)))

  ;; new-env returns a new environment from a formal parameter
  ;; specification, a list of actual parameters, and an outer
  ;; environment.  The symbol? test identifies "improper"
  ;; argument lists.  Environments are association lists,
  ;; associating variables with values.
  (define new-env
    (lambda (formals actuals env)
      (cond
        ((null? formals) env)
        ((symbol? formals) (cons (cons formals actuals) env))
        (else
         (cons (cons (car formals) (car actuals))
               (new-env (cdr formals) (cdr actuals) env))))))

  ;; lookup finds the value of the variable var in the environment
  ;; env, using assq.  Assumes var is bound in env.
  (define lookup
    (lambda (var env)
      (cdr (assq var env))))

  ;; assign is similar to lookup but alters the binding of the
  ;; variable var in the environment env by changing the cdr of
  ;; association pair
  (define assign
    (lambda (var val env)
      (set-cdr! (assq var env) val)))

  ;; exec evaluates the expression, recognizing all core forms.
  (define exec
    (lambda (exp env)
      (cond
        ((symbol? exp) (lookup exp env))
        ((pair? exp)
         (case (car exp)
           ((quote) (cadr exp))
           ((lambda)
            (lambda vals
              (let ((env (new-env (cadr exp) vals env)))
                (let loop ((exps (cddr exp)))
                   (if (null? (cdr exps))
                       (exec (car exps) env)
                       (begin
                          (exec (car exps) env)
                          (loop (cdr exps))))))))
           ((if)
            (if (exec (cadr exp) env)
                (exec (caddr exp) env)
                (exec (cadddr exp) env)))
           ((set!)
            (assign (cadr exp)
                    (exec (caddr exp) env)
                    env))
           (else
            (apply (exec (car exp) env)
                   (map (lambda (x) (exec x env))
                        (cdr exp))))))
        (else exp))))

  ;; interpret starts execution with the primitive environment.
  (set! interpret
    (lambda (exp)
      (exec exp  primitive-environment))))
;;; m4.ss
;;; Copyright (C) 1988 R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; speed improvement ideas:
;;;    use hash table rather than assoc for macro lookup
;;;    use extensible string buffer in place of lists
;;;    collect multiple characters when scanning text, arguments
;;;    use fixnum arithmetic where appropriate
 
(eval-when (compile) (optimize-level 3))

(define lexeme-type car)
(define lexeme-value cdr)

(define-structure (ibuf ip) ([pb '()]))

(define oparen #\()
(define cparen #\))

(define m4-get-char
   (lambda (ib)
      (let ([pb (ibuf-pb ib)])
         (if (null? pb)
             (read-char (ibuf-ip ib))
             (begin (set-ibuf-pb! ib (cdr pb))
                    (car pb))))))

(define m4-unget-char
   (lambda (c ib)
      (set-ibuf-pb! ib (cons c (ibuf-pb ib)))))

(define unget-string
   (lambda (s ib)
      (set-ibuf-pb! ib (append (string->list s) (ibuf-pb ib)))))

(define int->str
   (lambda (num)
      (format "~s" num)))

(define char->digit
   (let ([zero (char->integer #\0)])
      (lambda (c)
         (- (char->integer c) zero))))

(define str->int
   (let ([ustr->int
          (lambda (s i n)
             (let f ([a 0] [i i])
                (if (= i n)
                    a
                    (f (+ (* a 10) (char->digit (string-ref s i)))
                       (+ i 1)))))])
      (lambda (s)
         (let ([n (string-length s)])
            (if (= n 0)
                0
                (if (char=? (string-ref s 0) #\-)
                    (- (ustr->int s 1 n))
                    (ustr->int s 0 n)))))))

(define eval-string
   (let ([str #f] [port #f] [token #f] [value #f])
      (define eval-error
         (lambda ()
            (error 'm4 "invalid arithmetic expression ~s" str)))
      (define next-token!
         (lambda ()
            (let ([c (read-char port)])
               (cond
                  [(eof-object? c) (set! token 'eof)]
                  [(char-whitespace? c) (next-token!)]
                  [(char-numeric? c)
                   (let loop ([a (char->digit c)])
                      (let ([c (read-char port)])
                         (cond
                            [(eof-object? c)
                             (set! token 'integer)
                             (set! value a)]
                            [(char-numeric? c)
                             (loop (+ (* a 10) (char->digit c)))]
                            [else
                             (unread-char c port)
                             (set! token 'integer)
                             (set! value a)])))]
                  [(char=? c oparen) (set! token 'oparen)]
                  [(char=? c cparen) (set! token 'cparen)]
                  [(char=? c #\-) (set! token '-)]
                  [(char=? c #\*)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (set! token '*)]
                         [(char=? c #\*) (set! token '**)]
                         [else (unread-char c port) (set! token '*)]))]
                  [(char=? c #\+) (set! token '+)]
                  [(char=? c #\-) (set! token '+)]
                  [(char=? c #\/) (set! token '/)]
                  [(char=? c #\%) (set! token '%)]
                  [(char=? c #\!)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (set! token '!)]
                         [(char=? c #\=) (set! token '!=)]
                         [else (unread-char c port) (set! token '!)]))]
                  [(char=? c #\|)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (eval-error)]
                         [(char=? c #\|) (set! token 'or)]
                         [else (unread-char c port) (eval-error)]))]
                  [(char=? c #\&)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (eval-error)]
                         [(char=? c #\&) (set! token 'and)]
                         [else (unread-char c port) (eval-error)]))]
                  [(char=? c #\=)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (eval-error)]
                         [(char=? c #\=) (set! token '==)]
                         [else (unread-char c port) (eval-error)]))]
                  [(char=? c #\<)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (set! token '<)]
                         [(char=? c #\=) (set! token '<=)]
                         [else (unread-char c port) (set! token '<)]))]
                  [(char=? c #\>)
                   (let ([c (read-char port)])
                      (cond
                         [(eof-object? c) (set! token '>)]
                         [(char=? c #\=) (set! token '>=)]
                         [else (unread-char c port) (set! token '>)]))]))))
      (define E0 ; or
         (lambda ()
            (E0* (E1))))
      (define E0*
         (lambda (v)
            (case token
               [or (next-token!) (E0* (if (= (+ v (E1)) 0) 0 1))]
               [else v])))
      (define E1 ; and
         (lambda ()
            (E1* (E2))))
      (define E1*
         (lambda (v)
            (case token
               [and (next-token!) (E1* (if (= (* v (E2)) 0) 0 1))]
               [else v])))
      (define E2 ; ==, !=
         (lambda ()
            (E2* (E3))))
      (define E2*
         (lambda (v)
            (case token
               [== (next-token!) (E2* (if (= v (E3)) 1 0))]
               [!= (next-token!) (E2* (if (= v (E3)) 0 1))]
               [else v])))
      (define E3 ; <, <=, >, >=
         (lambda ()
            (E3* (E4))))
      (define E3*
         (lambda (v)
            (case token
               [< (next-token!) (E3* (if (< v (E4)) 1 0))]
               [<= (next-token!) (E3* (if (<= v (E4)) 1 0))]
               [> (next-token!) (E3* (if (> v (E4)) 1 0))]
               [>= (next-token!) (E3* (if (>= v (E4)) 1 0))]
               [else v])))
      (define E4 ; +, -
         (lambda ()
            (E4* (E5))))
      (define E4*
         (lambda (v)
            (case token
               [+ (next-token!) (E4* (+ v (E5)))]
               [- (next-token!) (E4* (- v (E5)))]
               [else v])))
      (define E5 ; *, /, %
         (lambda ()
            (E5* (E6))))
      (define E5*
         (lambda (v)
            (case token
               [* (next-token!) (E5* (* v (E6)))]
               [/ (next-token!) (E5* (quotient v (E6)))]
               [% (next-token!) (E5* (modulo v (E6)))]
               [else v])))
      (define E6 ; **
         (lambda ()
            (E6* (E7))))
      (define E6*
         (lambda (v)
            (case token
               [** (next-token!) (E6* (expt v (E7)))]
               [else v])))
      (define E7 ; -, integer, paren
         (lambda ()
            (case token
               [- (next-token!) (- (E7))]
               [! (next-token!) (if (= (E7) 0) 1 0)]
               [oparen
                (next-token!)
                (let ([v (E0)])
                   (unless (eq? token 'cparen) (eval-error))
                   (next-token!)
                   v)]
               [integer (next-token!) value]
               [else (eval-error)])))
      (lambda (s)
         (fluid-let ([str s] [port (open-input-string s)] [token #f] [value #f])
            (next-token!)
            (let ([v (E0)])
               (unless (eq? token 'eof) (eval-error))
               v)))))

(define *divnum* #f)
(define *diversions* #f)

(define m4-put-string
   (lambda (s)
      (unless (= *divnum* -1)
         (display s (vector-ref *diversions* *divnum*)))))

(define *open-quote* #f)
(define *close-quote* #f)

(define *macros* #f)
(define builtin-macros '())

(define *translit-table* #f)

(define define-builtin-macro
   (lambda (name proc)
      (set! builtin-macros (cons (cons name proc) builtin-macros))))

(define m4
   (lambda (ofn ifn . rest)
      (let ([op (open-output-file ofn 'replace)])
         (fluid-let ([*macros* builtin-macros]
                     [*open-quote* #\`]
                     [*close-quote* #\']
                     [*translit-table* #f]
                     [*divnum* 0]
                     [*diversions* (vector op #f #f #f #f #f #f #f #f #f)])
            (let loop ([ip (open-input-file ifn)] [rest rest])
               (m4-process (make-ibuf ip) op)
               (close-input-port ip)
               (unless (null? rest)
                  (loop (open-input-file (car rest)) (cdr rest))))
            (for-each undivert '(1 2 3 4 5 6 7 8 9)))
         (close-output-port op))))

(define m4-process
   (lambda (ib op)
      (let ([lexeme (read-lexeme ib)])
         (case (lexeme-type lexeme)
            [(comment literal)
             (m4-put-string (lexeme-value lexeme))
             (m4-process ib op)]
            [macro
             ((cdr (lexeme-value lexeme)) (read-args ib) ib)
             (m4-process ib op)]
            [eof #t]
            [else (error 'm4-internal "unexpected lexeme ~s" lexeme)]))))

(define name-start-char?
   (lambda (c)
      (or (char-alphabetic? c)
          (char=? c #\_))))

(define name-char?
   (lambda (c)
      (or (name-start-char? c)
          (char-numeric? c))))

(define read-lexeme
   (lambda (ib)
      (let ([c (m4-get-char ib)])
         (cond
            [(eof-object? c) (cons 'eof c)]
            [(char=? c #\#) (cons 'comment (read-comment ib))]
            [(char=? c *open-quote*) (cons 'literal (read-quoted ib))]
            [(name-start-char? c) (lookup-macro (cons c (read-alpha ib)))]
            [else (cons 'literal (string c))]))))

(define read-comment
   (lambda (ib)
      (let loop ([ls '(#\#)])
         (let ([c (m4-get-char ib)])
            (cond
               [(eof-object? c) (list->string (reverse ls))]
               [(char=? c #\newline) (list->string (reverse (cons c ls)))]
               [else (loop (cons c ls))])))))

(define read-quoted
   (lambda (ib)
      (let loop ([ls '()] [n 0])
         (let ([c (m4-get-char ib)])
            (cond
               [(eof-object? c)
                (error 'm4 "end-of-file detected at quote level ~s" n)]
               [(char=? c *close-quote*)
                (if (= n 0)
                    (list->string (reverse ls))
                    (loop (cons c ls) (- n 1)))]
               [(char=? c *open-quote*) (loop (cons c ls) (+ n 1))]
               [else (loop (cons c ls) n)])))))

(define read-alpha
   (lambda (ib)
      (let ([c (m4-get-char ib)])
         (cond
            [(eof-object? c) '()]
            [(name-char? c) (cons c (read-alpha ib))]
            [else (m4-unget-char c ib) '()]))))

(define lookup-macro
   (lambda (ls)
      (let ([s (list->string ls)])
         (let ([a (assoc s *macros*)])
            (if a
                (cons 'macro a)
                (cons 'literal s))))))

(define read-args
   (lambda (ib)
      (let ([c (m4-get-char ib)])
         (cond
            [(eof-object? c) '()]
            [(char=? c oparen)
             (let next-arg ()
                (let skip-white ()
                   (let ([c (m4-get-char ib)])
                      (cond
                         [(eof-object? c) '()]
                         [(char-whitespace? c) (skip-white)]
                         [else (m4-unget-char c ib)])))
                (let this-arg ([strings '()])
                   (let ([c (m4-get-char ib)])
                      (cond
                         [(or (eof-object? c) (char=? c cparen))
                          (if (null? strings)
                              '()
                              (cons (apply string-append (reverse strings))
                                    '()))]
                         [(char=? c oparen)
                          (let nest ([strings (cons (string oparen)
                                                    strings)]
                                     [k this-arg])
                             (let ([c (m4-get-char ib)])
                                (cond
                                   [(eof-object? c) (this-arg strings)]
                                   [(char=? c cparen)
                                    (k (cons (string cparen) strings))]
                                   [(char=? c oparen)
                                    (nest (cons (string oparen) strings)
                                          (lambda (strings)
                                             (nest strings k)))]
                                   [else
                                    (m4-unget-char c ib)
                                    (let ([lexeme (read-lexeme ib)])
                                       (case (lexeme-type lexeme)
                                          [comment (nest strings k)]
                                          [literal
                                           (nest (cons (lexeme-value lexeme)
                                                       strings)
                                                 k)]
                                          [macro
                                           ((cdr (lexeme-value lexeme))
                                            (read-args ib)
                                            ib)
                                           (nest strings k)]
                                          [else
                                           (error 'm4-internal
                                                  "unexpected lexeme ~s"
                                                  lexeme)]))])))]
                         [(char=? c #\,)
                          (cons (apply string-append (reverse strings))
                                (next-arg))]

                         [else
                          (m4-unget-char c ib)
                          (let ([lexeme (read-lexeme ib)])
                             (case (lexeme-type lexeme)
                                [comment (this-arg strings)]
                                [literal
                                 (this-arg
                                     (cons (lexeme-value lexeme) strings))]
                                [macro
                                 ((cdr (lexeme-value lexeme)) (read-args ib) ib)
                                 (this-arg strings)]
                                [else
                                 (error 'm4-internal
                                        "unexpected lexeme ~s"
                                        lexeme)]))]))))]
            [else (m4-unget-char c ib) '()]))))

;;; builtin macros

(define $$ (lambda (ls) (if (null? ls) ls (cdr ls))))
(define $1 (lambda (ls) (if (null? ls) "" (car ls))))
(define $2 (lambda (ls) ($1 ($$ ls))))
(define $3 (lambda (ls) ($2 ($$ ls))))
(define $4 (lambda (ls) ($3 ($$ ls))))
(define $5 (lambda (ls) ($4 ($$ ls))))
(define $6 (lambda (ls) ($5 ($$ ls))))
(define $7 (lambda (ls) ($6 ($$ ls))))
(define $8 (lambda (ls) ($7 ($$ ls))))
(define $9 (lambda (ls) ($8 ($$ ls))))

(define-builtin-macro "changequote"
   (lambda (args ib)
      (set! *open-quote*
            (if (string=? ($1 args) "") #\` (string-ref ($1 args) 0)))
      (set! *close-quote*
            (if (string=? ($2 args) "") #\' (string-ref ($2 args) 0)))))

(define-builtin-macro "define"
   (lambda (args ib)
      (let ([name ($1 args)])
         (unless (let ([n (string-length name)])
                    (and (fx> n 0)
                         (name-start-char? (string-ref name 0))
                         (let ok? ([i 1])
                            (or (fx= i n)
                                (and (name-char? (string-ref name i))
                                     (ok? (fx+ i 1)))))))
            (error 'm4-define "invalid macro name ~s" name))
         (let ([proc (make-macro ($2 args))])
            (let ([a (assoc name *macros*)])
               (if a
                   (set-cdr! a proc)
                   (set! *macros* (cons (cons name proc) *macros*))))))))

(define make-macro
   (lambda (s)
      (let ([ls (string->list s)])
         (lambda (args ib)
            (let loop ([ls ls])
               (unless (null? ls)
                  (case (and (char=? (car ls) #\$)
                             (not (null? (cdr ls)))
                             (cadr ls))
                     [#\1 (loop (cddr ls)) (unget-string ($1 args) ib)]
                     [#\2 (loop (cddr ls)) (unget-string ($2 args) ib)]
                     [#\3 (loop (cddr ls)) (unget-string ($3 args) ib)]
                     [#\4 (loop (cddr ls)) (unget-string ($4 args) ib)]
                     [#\5 (loop (cddr ls)) (unget-string ($5 args) ib)]
                     [#\6 (loop (cddr ls)) (unget-string ($6 args) ib)]
                     [#\7 (loop (cddr ls)) (unget-string ($7 args) ib)]
                     [#\8 (loop (cddr ls)) (unget-string ($8 args) ib)]
                     [#\9 (loop (cddr ls)) (unget-string ($9 args) ib)]
                     [else (loop (cdr ls)) (m4-unget-char (car ls) ib)])))))))

(define-builtin-macro "divert"
   (lambda (args ib)
      (set! *divnum*
         (if (string=? ($1 args) "")
             0
             (case (string-ref ($1 args) 0)
                [#\0 0]
                [#\1 1]
                [#\2 2]
                [#\3 3]
                [#\4 4]
                [#\5 5]
                [#\6 6]
                [#\7 7]
                [#\8 8]
                [#\9 9]
                [else -1])))
      (when (and (<= 1 *divnum* 9) (not (vector-ref *diversions* *divnum*)))
         (vector-set! *diversions* *divnum* (open-output-string)))))

(define-builtin-macro "divnum"
   (lambda (args ib)
      (unget-string (format "~a" *divnum*) ib)))

(define-builtin-macro "dnl"
   (lambda (args ib)
      (let loop ()
         (let ([c (m4-get-char ib)])
            (cond
               [(eof-object? c) '()]
               [(char=? c #\newline) '()]
               [else (loop)])))))

(define-builtin-macro "dumpdef"
   (lambda (args ib)
      (printf "m4 warning: no dumpdef yet~%")))

(define-builtin-macro "errprint"
   (lambda (args ib)
      (display ($1 args) *error-output*)
      (newline *error-output*)))

(define-builtin-macro "eval"
   (lambda (args ib)
      (unget-string (int->str (eval-string ($1 args))) ib)))

(define-builtin-macro "ifdef"
   (lambda (args ib)
      (unget-string ((if (assoc ($1 args) *macros*) $2 $3) args) ib)))

(define-builtin-macro "ifelse"
   (rec ifelse
      (lambda (args ib)
         (if (string=? ($1 args) ($2 args))
             (unget-string ($3 args) ib)
             (if (> (length args) 4)
                 (ifelse ($$ ($$ ($$ args))) ib)
                 (unget-string ($4 args) ib))))))

(define-builtin-macro "include"
   (lambda (args ib)
      (printf "m4 warning: no include yet~%")))

(define-builtin-macro "incr"
   (lambda (args ib)
      (unget-string (int->str (+ (str->int ($1 args)) 1)) ib)))

(define-builtin-macro "index"
   (lambda (args ib)
      (let ([s1 ($1 args)] [s2 ($2 args)])
         (let ([n1 (string-length s1)] [n2 (string-length s2)])
            (let find ([i 0])
               (if (fx> n2 (fx- n1 i))
                   (unget-string "-1" ib)
                   (let try ([i1 i] [i2 0])
                      (if (fx= i2 n2)
                          (unget-string (int->str i) ib)
                          (if (char=? (string-ref s1 i1) (string-ref s2 i2))
                              (try (fx+ i1 1) (fx+ i2 1))
                              (find (fx+ i 1)))))))))))

(define-builtin-macro "len"
   (lambda (args ib)
      (unget-string (int->str (string-length ($1 args))) ib)))

(define-builtin-macro "maketemp"
   (lambda (args ib)
      (printf "m4 warning: no maketemp yet~%")))

(define-builtin-macro "shift"
   (lambda (args ib)
      (printf "m4 warning: no shift yet~%")))

(define-builtin-macro "sinclude"
   (lambda (args ib)
      (printf "m4 warning: no sinclude yet~%")))

(define-builtin-macro "substr"
   (lambda (args ib)
      (let ([s ($1 args)] [start ($2 args)] [count ($3 args)])
         (let ([n (string-length s)])
            (let ([start (min (max (str->int start) 0) n)])
               (let ([end (if (string=? count "")
                              n
                              (min (max (+ (str->int count) start) start) n))])
                  (unget-string (substring s start end) ib)))))))

(define-builtin-macro "syscmd"
   ;;; cannot be written in Scheme---needs something more powerful than
   ;;; "system" or "process"
   (lambda (args ib)
      (printf "m4 warning: no syscmd yet~%")))

(define-builtin-macro "translit"
   (lambda (args ib)
      (let ([s1 ($1 args)] [s2 ($2 args)] [s3 ($3 args)])
         (let ([n1 (string-length s1)] [n2 (string-length s2)])
            (unless (= n2 (string-length s3))
               (error 'm4 "translit arguments ~s and ~s are not of same length"
                      s2 s3))
            (when (null? *translit-table*)
               (set! *translit-table* (make-string 256)))
            (do ([i 0 (fx+ i 1)])
                ((fx= i 256))
                (string-set! *translit-table* i (integer->char i)))
            (do ([i 0 (fx+ i 1)])
                ((fx= i n2))
                (string-set! *translit-table*
                             (char->integer (string-ref s2 i))
                             (string-ref s3 i)))
            (let ([s4 (make-string n1)])
               (do ([i 0 (fx+ i 1)])
                   ((fx= i n1))
                   (string-set! s4 i
                      (string-ref *translit-table*
                                  (char->integer (string-ref s1 i)))))
               (unget-string s4 ib))))))

(define-builtin-macro "undefine"
   (lambda (args ib)
      (let ([a (assoc ($1 args) *macros*)])
         (unless a (error 'm4 "cannot undefine ~s (not defined)" ($1 args)))
         (set-car! a #f))))

(define-builtin-macro "undivert"
   (rec myself
      (lambda (args ib)
         (if (null? args)
             (myself '("1" "2" "3" "4" "5" "6" "7" "8" "9") ib)
             (for-each
                (lambda (x)
                   (case (and (not (string=? x "")) (string-ref x 0))
                      [#\1 (undivert 1)]
                      [#\2 (undivert 2)]
                      [#\3 (undivert 3)]
                      [#\4 (undivert 4)]
                      [#\5 (undivert 5)]
                      [#\6 (undivert 6)]
                      [#\7 (undivert 7)]
                      [#\8 (undivert 8)]
                      [#\9 (undivert 9)]))
                args)))))

(define undivert
   (lambda (n)
      (let ([op (vector-ref *diversions* n)])
         (when op
            (display (get-output-string op) (vector-ref *diversions* 0))))))
;;; Copyright (C) 1989 R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; PCScheme/MacScheme "macro" defined in terms of "syntax-case":

(define-syntax macro
  (lambda (x)
    (syntax-case x ()
      ((_ name fcn)
       '(define-syntax name
           (lambda (x)
             (syntax-case x ()
               ((k . stuff)
                (datum->syntax-object 'k
                  (fcn (syntax-object->datum x)))))))))))


;;; PCScheme/MacScheme "macro" defined in terms of "extend-syntax":
;;; requires (current-expand eps-expand)

;(extend-syntax (macro)
;   [(macro name fcn)
;    (eval-when (compile load eval)
;       (let ([f fcn])
;          (extend-syntax (name)
;             [anything
;              ((with ([w 'with]) w)
;               ([v (f 'anything)]) v)])))])

;;; The strange expression "(with ([w 'with]) w)" is used to insert the
;;; keyword "with" into the expansion.  The "eval-when" in the expansion is
;;; necessary to allow macros defined in a file to be used later in the
;;; file, even if the file is compiled with "compile-file".  If it were
;;; left out, the implicit "eval-when" wrapped around the "extend-syntax"
;;; would cause it to be evaluated, but without the enclosing "let"
;;; expression.  The enclosing "let" expression is necessary to cause the
;;; function to be evaluated once, which may be important if the function
;;; something other than a simple lambda expression.


;;; PCScheme/MacScheme "macro" defined in terms of "define-syntax-expander":
;;; requires (current-expand eps-expand)

;(extend-syntax (macro)
;   [(macro name fcn)
;    (define-syntax-expander name
;       (let ([f fcn])
;          (lambda (x e) (e (f x) e))))])

;;; The "eval-when" is not necessary because "define-syntax-expander"
;;; expands into an "eval-when" expression, and the "let" expression is
;;; tucked inside the "define-syntax-expander" expression.

;;; If you want to see the expander generated by either of the above
;;; "extend-syntax" definitions looks like, use "extend-syntax/code" in
;;; place of "extend-syntax":

;;; > (extend-syntax/code (macro)
;;;      [(macro name fcn)
;;;       (define-syntax-expander name
;;;          (let ([f fcn])
;;;             (lambda (x e) (e (f x) e))))])
;;;
;;; (lambda (x e)
;;;    (unless (procedure? e)
;;;       (error 'macro "~s is not a procedure" e))
;;;    (e (cond
;;;          [(syntax-match? '(macro * *) x)
;;;           `(define-syntax-expander ,(cadr x)
;;;               (let ([f ,@(cddr x)]) (lambda (x e) (e (f x) e))))]
;;;          [else (error 'macro "invalid syntax ~s" x)])
;;;       e))
;;; matrix.ss
;;; Copyright (C) 1996 R. Kent Dybvig
;;; from "The Scheme Programming Language, 2ed" by R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; make-matrix creates a matrix (a vector of vectors).
(define make-matrix
  (lambda (rows columns)
    (do ((m (make-vector rows))
         (i 0 (+ i 1)))
        ((= i rows) m)
        (vector-set! m i (make-vector columns)))))

;;; matrix? checks to see if its argument is a matrix.
;;; It isn't foolproof, but it's generally good enough.
(define matrix?
  (lambda (x)
    (and (vector? x)
         (> (vector-length x) 0)
         (vector? (vector-ref x 0)))))

;;; matrix-ref returns the jth element of the ith row.
(define matrix-ref
  (lambda (m i j)
    (vector-ref (vector-ref m i) j)))

;;; matrix-set! changes the jth element of the ith row.
(define matrix-set!
  (lambda (m i j x)
    (vector-set! (vector-ref m i) j x)))

;;; mul is the generic matrix/scalar multiplication procedure
(define mul
  (lambda (x y)
   ;; type-error is called to complain when mul receives an invalid
   ;; type of argument.
    (define type-error
       (lambda (what)
          (error 'mul
             "~s is not a number or matrix"
             what)))

    ;; match-error is called to complain when mul receives a pair of
    ;; incompatible arguments.
    (define match-error
       (lambda (what1 what2)
          (error 'mul
             "~s and ~s are incompatible operands"
             what1
             what2)))

    ;; matrix-rows returns the number of rows in a matrix.
    (define matrix-rows
       (lambda (x)
          (vector-length x)))

    ;; matrix-columns returns the number of columns in a matrix.
    (define matrix-columns
       (lambda (x)
          (vector-length (vector-ref x 0))))

    ;; mat-sca-mul multiplies a matrix by a scalar.
    (define mat-sca-mul
       (lambda (m x)
          (let* ((nr (matrix-rows m))
                 (nc (matrix-columns m))
                 (r  (make-matrix nr nc)))
             (do ((i 0 (+ i 1)))
                 ((= i nr) r)
                 (do ((j 0 (+ j 1)))
                     ((= j nc))
                     (matrix-set! r i j
                        (* x (matrix-ref m i j))))))))

    ;; mat-mat-mul multiplies one matrix by another, after verifying
    ;; that the first matrix has as many columns as the second
    ;; matrix has rows.
    (define mat-mat-mul
       (lambda (m1 m2)
          (let* ((nr1 (matrix-rows m1))
                 (nr2 (matrix-rows m2))
                 (nc2 (matrix-columns m2))
                 (r   (make-matrix nr1 nc2)))
             (if (not (= (matrix-columns m1) nr2))
                 (match-error m1 m2))
             (do ((i 0 (+ i 1)))
                 ((= i nr1) r)
                 (do ((j 0 (+ j 1)))
                     ((= j nc2))
                     (do ((k 0 (+ k 1))
                          (a 0
                             (+ a
                                (* (matrix-ref m1 i k)
                                   (matrix-ref m2 k j)))))
                         ((= k nr2)
                          (matrix-set! r i j a))))))))

    ;; body of mul; dispatch based on input types
    (cond
      ((number? x)
       (cond
         ((number? y) (* x y))
         ((matrix? y) (mat-sca-mul y x))
         (else (type-error y))))
      ((matrix? x)
       (cond
         ((number? y) (mat-sca-mul x y))
         ((matrix? y) (mat-mat-mul x y))
         (else (type-error y))))
      (else (type-error x)))))
;;; object.ss
;;; Copyright (C) 1996 R. Kent Dybvig
;;; from "The Scheme Programming Language, 2ed" by R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; define-object creates an object constructor that uses let* to bind
;;; local fields and letrec to define the exported procedures.  An
;;; object is itself a procedure that accepts messages corresponding
;;; to the names of the exported procedures.  The second pattern is
;;; used to allow the set of local fields to be omitted.
(define-syntax define-object
  (syntax-rules ()
    ((_ (name . varlist)
        ((var1 val1) ...)
        ((var2 val2) ...))
     (define name
       (lambda varlist
         (let* ((var1 val1) ...)
           (letrec ((var2 val2) ...)
             (lambda (msg . args)
               (case msg
                 ((var2) (apply var2 args)) ...
                 (else
                  (error 'name "invalid message ~s"
                     (cons msg args))))))))))
    ((_ (name . varlist)
        ((var2 val2) ...))
     (define-object (name . varlist)
       ()
       ((var2 val2) ...)))))

;;; send-message abstracts the act of sending a message from the act
;;; of applying a procedure and allows the message to be unquoted.
(define-syntax send-message
  (syntax-rules ()
    ((_ obj msg arg ...)
     (obj 'msg arg ...))))
;;; doubly recursive power (expt) function

;;; try using trace-lambda to see the nesting.

(define power
   (lambda (x n)
      (cond
         [(= n 0) 1]
         [(= n 1) x]
         [else
          (let ([q (quotient n 2)])
             (* (power x q) (power x (- n q))))])))
;;; queue
;;; an abstract datatype

;;; operations:
;;;    (queue)           ;create a queue object

;;;    if 'q' is a queue object:

;;;    (q 'type?)        ;return the type (queue), useful if there are other
;;;                      ;abstract datatypes floating around.
;;;    (q 'empty?)       ;returns true iff q is empty
;;;    (q 'put val)      ;adds val to end of q; returns val
;;;    (q 'get)          ;removes first element of q and returns it

;;; Examples

;;;    (define! q (queue))
;;;    (q 'type?)             => queue
;;;    (q 'empty?)            => #!true
;;;    (q 'put 3)
;;;    (q 'put 4)
;;;    (q 'put 5)
;;;    (q 'empty?)            => ()
;;;    (q 'get)               => 3
;;;    (q 'get)               => 4
;;;    (q 'put 7)
;;;    (q 'get)               => 5
;;;    (q 'get)               => 7
;;;    (q 'empty?)            => #!true

(define queue
   (lambda ()
      (let ([head '()] [tail '()])
         (lambda (request . args)
            (case request
               [type? 'queue]
               [empty? (null? head)]
               [put
                (let ([v (car args)])
                   (if (null? head)
                       (let ([p (cons v '())])
                          (set! tail p)
                          (set! head p))
                       (let ([quebit (cons v '())])
                          (set-cdr! tail quebit)
                          (set! tail quebit)))
                   v)]
               [get
                (if (null? head)
                    (error 'queue "queue is empty")
                    (let ([v (car head)])
                       (set! head (cdr head))
                       (when (null? head) (set! tail '()))
                       v))]
               [else
                (error 'queue "~s is not a valid request" request)])))))
;;; rabbit

;;; The rabbit program highlights the use of continuations and
;;; timer interrupts to perform thread scheduling.  The scheduler
;;; maintains a thread queue and operating system primitives for
;;; dispatching and thread creation.  The queue is only visible
;;; to the operating system kernel and all accesses are performed
;;; with the timer off to prevent corruption.

;;; (thread exp) will create a thread out of exp and place it in
;;; the thread queue.  you may do this for as many threads as
;;; you like.  (dispatch) starts the threads going.  If the
;;; thread queue ever becomes empty, dispatch exits.  Threads
;;; may create other threads.

;;; The rabbit function creates a thread that spawns two offspring
;;; and dies.  Each thread has a generation number associated with
;;; it.  The generation number of each rabbit is one lower than that
;;; of it's parent; rabbits in generation 0 are sterile.

;;; load the queue datatype -- might need a fuller pathname
(load "queue.ss")

;;; swap-time determines the number of timer ticks in a time slice
(define swap-time
   (make-parameter
      100
      (lambda (x)
         (unless (and (integer? x) (positive? x))
            (error 'swap-time "~s is not a positive integer" x))
         x)))

(define dispatch #f)
(define thread #f)

(let ([pq (queue)])
   (set! dispatch
      (lambda ()
         (unless (pq 'empty?)
             ; the thread queue holds continuations---grab one and invoke it
             (let ([next (pq 'get)])
                (set-timer (swap-time))
                (next #f)))))
    (set! thread
       (lambda (thunk)
          (call/cc
             (lambda (return)
                (call/cc
                   (lambda (k)
                      ; turn off the timer while accessing the queue
                      (let ([time-left (set-timer 0)])
                         ; put the thread on the queue
                         (pq 'put k)
                         (set-timer time-left)
                         ; get out of here
                         (return #f))))
                ; the first time through we will return before getting
                ; here.  the second time is when a thread is first
                ; dispatched from the thread queue.
                (thunk)
                (set-timer 0)
                (dispatch)))))
    (timer-interrupt-handler
       (lambda ()
          (printf "swapping~%")
          (call/cc
             (lambda (l)
                ; place the continuation of the interrupt on the queue
                (pq 'put l)
                (dispatch))))))


;;; *delay-max* gives the maximum random delay before a rabbit
;;; reaches child-bearing age.
(define *delay-max* 10000)

(define rabbit
   (lambda (n)
      (thread
         (lambda ()
            (printf "~s~%" n)
            (unless (zero? n)
               (do ([i (random *delay-max*) (1- i)]) ((zero? i)))
               (rabbit (1- n))
               (rabbit (1- n)))))))
;;; Implementation:

(module ((make-user user) show-center encrypt decrypt send)

;;; (make-user name) creates a user with the chosen name.  When it
;;; creates the user, it tells him what his name is.  He will use
;;; this when registering with the center.

(define-syntax make-user
  (syntax-rules ()
    [(_ uid)
     (begin (define uid (user 'uid)) (uid 'register))]))

;;; (encrypt mesg u1 u2) causes user 1 to encrypt mesg using the public
;;; keys for user 2.

(define-syntax encrypt
  (syntax-rules ()
    [(_ mesg u1 u2) ((u1 'send) mesg 'u2)]))

;;; (decrypt number-list u) causes the user to decrypt the list of
;;; numbers using his private key.

(define-syntax decrypt
  (syntax-rules ()
    [(_ numbers u) ((u 'receive) numbers)]))

;;; (send mesg u1 u2) this combines the functions 'encrypt' and 'decrypt',
;;; calling on user 1 to encrypt the message for user 2 and calling on
;;; user 2 to decrypt the message.

(define-syntax send
  (syntax-rules ()
    [(_ mesg u1 u2) (decrypt (encrypt mesg u1 u2) u2)]))

;;; A user is capable of the following:
;;;   -        choosing public and private keys and registering with the center
;;;   - revealing his public and private keys
;;;   -        retrieving user's private keys from the center and encrypting a
;;;        message for that user
;;;   -        decrypting a message with his private key

(define user
  (lambda (name)
    (let* ([low (expt 2 63)]     ; low, high = bounds on p and q
           [high (* 2 low)]
           [p 0]                 ; p,q = two large, probable primes
           [q 0]
           [n 0]                 ; n = p * q, base for modulo arithmetic
           [phi 0]               ; phi = lcm(p-1,q-1), not quite the Euler phi function,
                                 ;        but it will serve for our purposes
           [e 0]                 ; e = exponent for encryption
           [d 0])                ; d = exponent for decryption
      (lambda (request)
        (case request
          ;; choose keys and register with the center
          [register
           (set! p (find-prime low high))
           (set! q
             (let loop ([q1 (find-prime low high)])
               (if (= 1 (gcd p q1))
                   q1
                   (loop (find-prime low high)))))
           (set! n (* p q))
           (set! phi
              (/ (* (1- p) (1- q))
                 (gcd (1- p) (1- q))))
           (set! e
             (do ([i 3 (+ 2 i)])
                 ((= 1 (gcd i phi)) i)))
           (set! d (mod-inverse e phi))
           (register-center (cons name (list n e)))
           (printf "Registered with Center~%")
           (printf "User: ~s~%" name)
           (printf "Base: ~d~%" n)
           (printf "Encryption exponent: ~d~%" e)]

          ;; divulge your keys-- you should resist doing this...
          [show-all
           (printf "p = ~d ; q = ~d~%" p q)
           (printf "n = ~d~%" n)
           (printf "phi = ~d~%" (* (1- p) (1- q)))
           (printf "e = ~d ; d = ~d~%" e d)]

          ;; get u's public key from the center and encode
          ;; a message for him
          [send
           (lambda (mesg u)
             (let* ([public (request-center u)]
                    [base (car public)]
                    [exponent (cadr public)]
                    [mesg-list (string->numbers mesg base)])
               (map (lambda (x) (expt-mod x exponent base))
                    mesg-list)))]

          ;; decrypt a message with your private key
          [receive
           (lambda (crypt-mesg)
             (let ([mesg-list (map (lambda (x) (expt-mod x d n)) crypt-mesg)])
               (numbers->string mesg-list)))])))))

;;; The center maintains the list of public keys.  It can register
;;; new users, provide the public keys for any particular user, or
;;; display the whole public file.

(module (register-center request-center show-center)
  (define public-keys '())
  (define register-center
    (lambda (entry)
      (set! public-keys
        (cons entry
              (remq (assq (car entry) public-keys) public-keys)))))
  (define request-center
    (lambda (u)
      (let ([a (assoc u public-keys)])
        (when (null? a)
          (error 'request-center
            "User ~s not registered in center"
            u))
        (cdr a))))
  (define show-center
    (lambda ()
      (for-each
        (lambda (entry)
          (printf "~%User: ~s~%" (car entry))
          (printf "Base: ~s~%" (cadr entry))
          (printf "Encryption exponent: ~s~%" (caddr entry)))
        public-keys)))
)

;;; string->numbers encodes a string as a list of numbers
;;; numbers->string decodes a string from a list of numbers

;;; string->numbers and numbers->string are defined with respect to
;;; an alphabet.  Any characters in the alphabet are translated into
;;; integers---their regular ascii codes.  Any characters outside
;;; the alphabet cause an error during encoding.  An invalid code
;;; during decoding is translated to a space.

(module (string->numbers numbers->string)
  (define first-code 32)
  (define last-code 126)
  (define alphabet
    ; printed form of the characters, indexed by their ascii codes
    (let ([alpha (make-string 128 #\space)])
      (do ([i first-code (+ i)])
          ((= i last-code) alpha)
        (string-set! alpha i (integer->char i)))))

  (define string->integer
    (lambda (str)
      (let ([ln (string-length str)])
        (let loop ([i 0] [m 0])
          (if (= i ln)
              m
              (let* ([c (string-ref str i)] [code (char->integer c)])
                (when (or (< code first-code) (>= code last-code))
                  (error 'rsa "Illegal character ~s" c))
                (loop (+ i) (+ code (* m 128)))))))))

  (define integer->string
    (lambda (n)
      (list->string
        (map (lambda (n) (string-ref alphabet n))
             (let loop ([m n] [lst '()])
               (if (zero? m)
                   lst
                   (loop (quotient m 128)
                         (cons (remainder m 128) lst))))))))

  ; turn a string into a list of numbers, each no larger than base
  (define string->numbers
    (lambda (str base)
      (letrec ([block-size
                (do ([i -1 (+ i)] [m 1 (* m 128)]) ((>= m base) i))]
               [substring-list
                (lambda (str)
                  (let ([ln (string-length str)])
                    (if (>= block-size ln)
                        (list str)
                        (cons (substring str 0 block-size)
                              (substring-list
                                (substring str block-size ln))))))])
        (map string->integer (substring-list str)))))

  ; turn a list of numbers into a string
  (define numbers->string
    (lambda (lst)
      (letrec ([reduce
                (lambda (f l)
                  (if (null? (cdr l))
                      (car l)
                      (f (car l) (reduce f (cdr l)))))])
        (reduce
          string-append
          (map (lambda (x) (integer->string x)) lst)))))
)

;;; find-prime finds a probable prime between two given arguments.
;;; find-prime uses a cheap but fairly dependable test for primality
;;; for large numbers, by first weeding out multiples of first 200
;;; primes, then applies Fermat's theorem with base 2.

(module (find-prime)
  (define product-of-primes
    ; compute product of first n primes, n > 0
    (lambda (n)
      (let loop ([n (1- n)] [p 2] [i 3])
        (cond
          [(zero? n) p]
          [(= 1 (gcd i p)) (loop (1- n) (* p i) (+ i 2))]
          [else (loop n p (+ i 2))]))))
  (define prod-first-200-primes (product-of-primes 200))
  (define probable-prime
    ; first check is quick, and weeds out most non-primes
    ; second check is slower, but weeds out almost all non-primes
    (lambda (p)
      (and (= 1 (gcd p prod-first-200-primes))
           (= 1 (expt-mod 2 (1- p) p)))))
  (define find-prime
    ; find probable prime in range low to high (inclusive)
    (lambda (low high)
      (let ([guess
             (lambda (low high)
               (let ([g (+ low (random (+ (- high low))))])
                 (if (odd? g) g (+ g))))])
        (let loop ([g (guess low high)])
          (cond
            ; start over if already too high
            [(> g high) (loop (guess low high))]
            ; if guess is probably prime, return
            [(probable-prime g) g]
            ; don't bother with even guesses
            [else (loop (+ 2 g))])))))
)

;;; mod-inverse finds the multiplicative inverse of x mod b, if it exists

(module (mod-inverse)
  (define gcdx
    ; extended Euclid's gcd algorithm, x <= y
    (lambda (x y)
      (let loop ([x x] [y y] [u1 1] [u2 0] [v1 0] [v2 1])
        (if (zero? y)
            (list x u1 v1)
            (let ([q (quotient x y)] [r (remainder x y)])
              (loop y r u2 (- u1 (* q u2)) v2 (- v1 (* q v2))))))))

  (define mod-inverse
    (lambda (x b)
      (let* ([x1 (modulo x b)] [g (gcdx x1 b)])
        (unless (= (car g) 1)
          (error 'mod-inverse "~d and ~d not relatively prime" x b))
        (modulo (cadr g) b))))
)
)
;;; scons.ss
;;; a stream-construction facility

;;; The scons special form performs a cons, suspending the cdr field
;;; by enclosing it in a procedure of no arguments.  scdr tests to see
;;; if the cdr is a procedure, and if so, invokes it.  scar is provided
;;; for symmetry; it is just car.

;;; The function stream-ref is simply list-ref defined in terms of
;;; scdr and scar.

;;; factlist and fiblist are two infinite streams.
;;; Try (stream-ref factlist 10) or (stream-ref fiblist 20).

;;; scons could easily suspend the car field as well.  This would
;;; implement the lazy cons of Friedman & Wise.

(define-syntax scons
  (syntax-rules ()
    ((_ car cdr) (cons car (lambda () cdr)))))

(define scar car)

(define scdr
   (lambda (x)
      (when (procedure? (cdr x)) (set-cdr! x ((cdr x))))
      (cdr x)))

(define stream-ref
   (lambda (x n)
      (if (zero? n)
          (scar x)
          (stream-ref (scdr x) (1- n)))))

(define factlist
   (let fact ([a 1] [n 1])
      (scons a (fact (* a n) (+ n)))))

(define fiblist
   (let fib ([fib-2 0] [fib-1 1])
      (scons fib-1 (fib fib-1 (+ fib-2 fib-1)))))
;;; setof.ss
;;; Copyright (C) 1996 R. Kent Dybvig
;;; from "The Scheme Programming Language, 2ed" by R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; set-of uses helper syntactic extension set-of-help, passing it
;;; an initial base expression of '()
(define-syntax set-of
  (syntax-rules ()
    ((_ e m ...)
     (set-of-help e '() m ...))))

;;; set-of-help recognizes in, is, and predicate expressions and
;;; changes them into nested named let, let, and if expressions.
(define-syntax set-of-help
  (syntax-rules (in is)
    ((_ e base)
     (set-cons e base))
    ((_ e base (x in s) m ...)
     (let loop ((set s))
       (if (null? set)
           base
           (let ((x (car set)))
             (set-of-help e (loop (cdr set)) m ...)))))
    ((_ e base (x is y) m ...)
     (let ((x y)) (set-of-help e base m ...)))
    ((_ e base p m ...)
     (if p (set-of-help e base m ...) base))))

;;; set-cons returns the original set y if x is already in y.
(define set-cons
  (lambda (x y)
    (if (memv x y)
        y
        (cons x y))))
;;; socket.ss
;;; R. Kent Dybvig May 1998
;;; Updated November 2005
;;; Updated by Jamie Taylor, Sept 2016
;;; Public Domain
;;;
;;; bindings for socket operations and other items useful for writing
;;; programs that use sockets.

;;; Requires csocket.so, built from csocket.c.
;;; Example compilation command line from macOS:
;;;  cc -c csocket.c -o csocket.o
;;;  cc csocket.o -dynamic -dynamiclib -current_version 1.0 -compatibility_version 1.0 -o csocket.so
(load-shared-object "./csocket.so")

;;; Requires from C library:
;;;   close, dup, execl, fork, kill, listen, tmpnam, unlink
(case (machine-type)
  [(i3le ti3le) (load-shared-object "libc.so.6")]
  [(i3osx ti3osx a6osx ta6osx) (load-shared-object "libc.dylib")]
  [else (load-shared-object "libc.so")])

;;; basic C-library stuff

(define close
  (foreign-procedure "close" (int)
    int))

(define dup
  (foreign-procedure "dup" (int)
    int))

(define execl4
  (let ((execl-help
         (foreign-procedure "execl"
           (string string string string void*)
           int)))
    (lambda (s1 s2 s3 s4)
      (execl-help s1 s2 s3 s4 0))))

(define fork
  (foreign-procedure "fork" ()
    int))

(define kill
  (foreign-procedure "kill" (int int)
    int))

(define listen
  (foreign-procedure "listen" (int int)
    int))

(define tmpnam
  (foreign-procedure "tmpnam" (void*)
    string))

(define unlink
  (foreign-procedure "unlink" (string)
    int))

;;; routines defined in csocket.c

(define accept
  (foreign-procedure "do_accept" (int)
    int))

(define bytes-ready?
  (foreign-procedure "bytes_ready" (int)
    boolean))

(define bind
  (foreign-procedure "do_bind" (int string)
    int))

(define c-error
  (foreign-procedure "get_error" ()
    string))

(define c-read
  (foreign-procedure "c_read" (int u8* size_t size_t)
    ssize_t))

(define c-write
  (foreign-procedure "c_write" (int u8* size_t ssize_t)
    ssize_t))

(define connect
  (foreign-procedure "do_connect" (int string)
    int))

(define socket
  (foreign-procedure "do_socket" ()
    int))

;;; higher-level routines

(define dodup
 ; (dodup old new) closes old and dups new, then checks to
 ; make sure that resulting fd is the same as old
  (lambda (old new)
    (check 'close (close old))
    (unless (= (dup new) old)
      (error 'dodup
        "couldn't set up child process io for fd ~s" old))))

(define dofork
 ; (dofork child parent) forks a child process and invokes child
 ; without arguments and parent with the child's pid
  (lambda (child parent)
    (let ([pid (fork)])
      (cond
        [(= pid 0) (child)]
        [(> pid 0) (parent pid)]
        [else (error 'fork (c-error))]))))

(define setup-server-socket
 ; create a socket, bind it to name, and listen for connections
  (lambda (name)
    (let ([sock (check 'socket (socket))])
      (unlink name)
      (check 'bind (bind sock name))
      (check 'listen (listen sock 1))
      sock)))

(define setup-client-socket
 ; create a socket and attempt to connect to server
  (lambda (name)
    (let ([sock (check 'socket (socket))])
      (check 'connect (connect sock name))
      sock)))

(define accept-socket
 ; accept a connection
  (lambda (sock)
    (check 'accept (accept sock))))

(define check
 ; signal an error if status x is negative, using c-error to
 ; obtain the operating-system's error message
  (lambda (who x)
    (if (< x 0)
        (error who (c-error))
        x)))

(define terminate-process
 ; kill the process identified by pid
  (lambda (pid)
    (define sigterm 15)
    (kill pid sigterm)
    (void)))

(define open-process
  (lambda (command)
    (define (make-r! socket)
      (lambda (bv start n)
        (check 'r! (c-read socket bv start n))))
    (define (make-w! socket)
      (lambda (bv start n)
        (check 'w! (c-write socket bv start n))))
    (define (make-close pid socket)
      (lambda ()
        (check 'close (close socket))
        (terminate-process pid)))
    (let* ([server-socket-name (tmpnam 0)]
           [server-socket (setup-server-socket server-socket-name)])
      (dofork 
        (lambda () ; child
          (check 'close (close server-socket))
          (let ([sock (setup-client-socket server-socket-name)])
            (dodup 0 sock)
            (dodup 1 sock))
          (check 'execl (execl4 "/bin/sh" "/bin/sh" "-c" command))
          (error 'open-process "subprocess exec failed"))
        (lambda (pid) ; parent
          (let ([sock (accept-socket server-socket)])
            (check 'close (close server-socket))
            (make-custom-binary-input/output-port command
              (make-r! sock) (make-w! sock) #f #f (make-close pid sock))))))))
;;; Copyright 1984-2017 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;; include-template: a simple yet powerful extensible templating mechanism
;;;
;;; Authors: Andrew W. Keep and R. Kent Dybvig
;;;
;;; The syntax (include-template <filename>) expands into an expression whose
;;; value is a string containing the contents of the file named by <filename>,
;;; except each occurrence of @@ within the file is replaced by @, and each
;;; occurrence of @(<scheme expression>) is replaced with the value of
;;; <scheme expression>.  <filename> must be a string literal, and the value
;;; of each <scheme expression> must be a string.  The file named by <filename>
;;; must be present at expand time and need not be present at run time.
;;;
;;; The template system can also be extended using the syntax:
;;;
;;; (define-template-extension <template-pattern> <output-pattern>)
;;;
;;; where:
;;;
;;; <template-pattern>     -> (<initial-pattern> <additional-pattern>*)
;;;                         | <non-template-pattern>
;;; <initial-pattern>      -> <keyword> (<syntax-pattern>*) <template-id>
;;;                         | <keyword> <template-id>
;;; <additional-pattern>   -> <initial-pattern>
;;;                         | (<keyword> (<syntax-pattern>*) <template-id>) ...
;;;                         | (<keyword> <tempalte-id>) ...
;;;                         | (optional <keyword> (<syntax-pattern>*)
;;;                             <template-id>)
;;;                         | (optional <keyword> <template-id>)
;;; <non-template-pattern> -> <keyword> (<syntax-pattern>*)
;;; <keyword>              -> <initial-kw-character> <kw-character>
;;; <initial-kw-character> -> a - z | A - Z | ! | $ | % | & | * | / | : | <
;;;                         | = | > | ? | ^ | _ | ~
;;; <kw-character>         -> <initial-kw-character> | 0 - 9 | - | + | .
;;;
;;; <output-pattern> is treated as a quasisyntax template,
;;; <syntax-pattern> is a syntax-rules pattern and,
;;; <template-id> is any valid scheme identifier.
;;;
;;; For <template-patterns> using a <non-template-pattern> a new @<keyword>
;;; form is created and when the @<keyword>(<syntax-pattern>*) is encountered
;;; in a template, it is immediately replaced with the <output-pattern>.
;;;
;;; For instance an extension that converts numbers to strings can be
;;; implemented as:
;;;
;;; (define-template-extension (num (e)) (number->string e))
;;;
;;; When @num(10) is encountered in a template, the string "10" is generated.
;;;
;;; For <template-patterns> with an <inital-pattern> each <keyword> in the
;;; <template-pattern> the define-template-extension will create a new syntax
;;; form @<keyword> along with an @end<keyword> for the <keyword> from the
;;; <initial-pattern>.  When include-template encounters an
;;; @<initial-keyword> ... @end<initial-keyword> pattern it will match the
;;; <syntax-pattern>* (when supplied) against the following scheme expression
;;; and match the template expressions found between the @<keywords> to the
;;; matching <template-id> bindings.
;;;
;;; For example, we can add a "for" loop extension as:
;;;
;;; (define-template-extension (for ([x e] [xs es] ...) tmpl)
;;;   (let ([t e])
;;;     (apply string-append (map (lambda (x xs ...) tmpl) t es ...))))
;;;
;;;     (for (exprs ...) tmpl)
;;;     =>
;;;     @for (lambda (...)
;;;            (let ([t (read-scheme k...)])
;;;              (cons (incomplete-node @for t) ...)))
;;;     @endfor (lambda (...)
;;;               ---
;;;               (let loop ([t '()] [tmpl '()] ...)
;;;                 (cond
;;;                   [(and (incomplete-node? (car stack)) (eq? (incomplete-node-kw (car stack)) '@for))
;;;                    (with-syntax (['([x e] [xs es] ...) (incomplete-node-stx (car stack))])
;;;                      '(let ([t e])
;;;                         (apply string-append (map (lambda (x xs ...) tmpl) t es ..))))
;;;
;;; In a template if: @for([x '("a" "b" "c")]) got @(t). @endfor
;;; is encountered, it will print the string " got a.  got b.  got c. " for
;;; this expression (which will be produced by the code:
;;; (apply string-append
;;;   (map (lambda (x) (string-append " got " x ". ") '("a" "b" "c"))))
;;;
;;; For a more complex example see the @if/@elif/@else/@endif example at
;;; the end of the library.
;;;
;;; Additional examples are embedded within the tests below #!eof.

;;; The (template-helpers) library supplies scheme procedures that are used at
;;; macro-expansion time by both the include-template and
;;; define-template-extension macros.
(library (template-helpers)
  (export
    incomplete-node? make-incomplete-node
    incomplete-node-type incomplete-node-e* incomplete-node-bfp
    source-string source-error
    read-scheme initial-id-char? id-char?
    make-string-buffer extend-string-buffer! extract-string-and-reset!
    open-positional-string-output-port
    strip-blanks)
  (import (chezscheme))

  (define (source-string sfd bfp)
    (call-with-values
      (lambda () (locate-source sfd bfp))
      (case-lambda
        [() (format "at char position ~s of ~a" bfp
              (source-file-descriptor-path sfd))]
        [(path lp cp) (format "at line ~s, char ~s of ~a" lp cp
                        (source-file-descriptor-path sfd))])))

  (define (source-error sfd bfp msg . args)
    (errorf 'include-template "~? ~a" msg args (source-string sfd bfp)))

  (define (read-scheme k ip sfd bfp)
    (let-values ([(x new-bfp) (get-datum/annotations ip sfd bfp)])
      (let ([x (if (annotation? x) (annotation-expression x) x)])
        (values (datum->syntax k x) new-bfp))))

  (define-record-type incomplete-node (nongenerative) (fields type e* bfp))

  (define (initial-id-char? c)
    (or (char<=? #\a c #\z) (char<=? #\A c #\Z)
        (char=? c #\!) (char<=? #\$ c #\&)
        (char=? c #\*) (char=? c #\/)
        (char=? c #\:) (char<=? #\< c #\?)
        (char=? c #\^) (char=? c #\_) (char=? c #\~)))

  (define (id-char? c)
    (or (initial-id-char? c) (char<=? #\0 c #\9)
        (char=? c #\-) (char=? c #\+) (char=? c #\.)))

  (define-record-type string-buffer (nongenerative)
    (fields (mutable n) (mutable str))
    (protocol (lambda (new) (lambda () (new 0 (make-string 16))))))
  (define (get-buffer tb n required-capacity)
    (let* ([str (string-buffer-str tb)] [len (string-length str)])
      (if (fx< (fx- len n) required-capacity)
          (let ([new-str (make-string (fx* 2 (fx+ len required-capacity)))])
            (string-copy! str 0 new-str 0 n)
            (string-buffer-str-set! tb new-str)
            new-str)
          str)))
  (define (extend-string-buffer! tb c)
    (let ([n (string-buffer-n tb)])
      (string-set! (get-buffer tb n 1) n c)
      (string-buffer-n-set! tb (fx+ n 1))))
  (define (append-to-string-buffer! tb str)
    (let ([n (string-buffer-n tb)] [len (string-length str)])
      (string-copy! (get-buffer tb n len) n str 0 len)
      (string-buffer-n-set! tb (fx+ n len))))
  (define (extract-string-and-reset! tb)
    (let ([str (substring (string-buffer-str tb) 0 (string-buffer-n tb))])
      (string-buffer-n-set! tb 0)
      str))
  (define (open-positional-string-output-port)
    (define-record-type position (nongenerative)
      (fields (mutable line) (mutable column))
      (protocol (lambda (new) (lambda () (new 1 1)))))
    (let ([tb (make-string-buffer)] [pos (make-position)])
      (define (w! str start cnt)
        (let* ([n (string-buffer-n tb)]
               [buf (get-buffer tb n cnt)]
               [end (fx+ start cnt)])
          (let loop! ([i start] [n n] [line (position-line pos)] [column (position-column pos)])
            (if (fx= i end)
                (begin
                  (position-line-set! pos line)
                  (position-column-set! pos column)
                  (string-buffer-n-set! tb n))
                (let ([c (string-ref str i)])
                  (string-set! buf n c)
                  (if (char=? c #\newline)
                      (loop! (fx+ i 1) (fx+ n 1) (fx+ line 1) 1)
                      (loop! (fx+ i 1) (fx+ n 1) line (fx+ column 1)))))))
        cnt)
      (define (gp) (string-buffer-n tb))
      (let ([op (make-custom-textual-output-port "positional-string-output-port" w! gp #f #f)])
        (define (line) (flush-output-port op) (position-line pos))
        (define (column) (flush-output-port op) (position-column pos))
        (define (p) (flush-output-port op) (extract-string-and-reset! tb))
        (values op p line column))))
  
  ;; scan foward for blanks, and if it leads you to a new-line, strip
  ;; the previous blanks back to the new line.
  (define (finish-strip ip stack bfp start-bfp)
    (let ([node-to-strip (car stack)])
      (if (string? node-to-strip)
          (let loop ([i (string-length node-to-strip)])
            (if (fx= i 0)
                (values (cdr stack) bfp)
                (let ([i (fx- i 1)])
                  (let ([c (string-ref node-to-strip i)])
                    (cond
                      [(char=? c #\newline)
                       (values (cons (substring node-to-strip 0 (fx+ i 1)) (cdr stack)) bfp)]
                      [(char-whitespace? c) (loop i)]
                      [else (set-port-position! ip start-bfp) (values stack start-bfp)])))))
          (begin (set-port-position! ip start-bfp) (values stack start-bfp)))))
  (define (strip-blanks ip stack start-bfp)
    (let gather-blanks ([bfp start-bfp])
      (let ([c (read-char ip)])
        (cond
          [(eof-object? c) (finish-strip ip stack bfp start-bfp)]
          [(char=? c #\newline) (finish-strip ip stack (fx+ bfp 1) start-bfp)]
          [(char-whitespace? c) (gather-blanks (fx+ bfp 1))]
          [else (set-port-position! ip start-bfp) (values stack start-bfp)])))))

(library (template)
  (export include-template define-template-extension optional @if @elif @else @endif @for @endfor @num)
  (import (chezscheme) (template-helpers))

  (define-syntax optional (lambda (x) (syntax-violation #f "misplaced aux keyword" x)))

  (define check-string-and-indent
    (lambda (s at indent)
      (unless (string? s)
        (errorf 'include-template "unexpected non-string value ~s of expression ~s" s at))
      (if (= indent 0)
          s
          (let ([ip (open-string-input-port s)])
            (let ([first-line (get-line ip)])
              (if (eof-object? first-line)
                  s
                  (let-values ([(op p) (open-string-output-port)])
                    (display first-line op)
                    (let ([indent (make-string indent #\space)])
                      (let loop ()
                        (let ([line (get-line ip)])
                          (if (eof-object? line)
                              (begin
                                (when (char=? (string-ref s (fx- (string-length s) 1)) #\newline) (newline op))
                                (p))
                              (begin
                                (newline op)
                                (display indent op)
                                (display line op)
                                (loop)))))))))))))

  (define-syntax include-template
    (lambda (x)
      (define (process-template-file r fn k)
        (let* ([bip (open-file-input-port fn)]
               [sfd (make-source-file-descriptor fn bip #t)]
               [ip (transcoded-port bip (native-transcoder))]
               [tb (make-string-buffer)])
          (define (s0 a bfp)
            (let ([c (read-char ip)])
              (cond
                [(eof-object? c)
                 (close-input-port ip)
                 (reverse (cons (extract-string-and-reset! tb) a))]
                [(char=? c #\@) (s1 a (+ bfp 1))]
                [else (extend-string-buffer! tb c) (s0 a (+ bfp 1))])))
          (define (s1 a bfp)
            (let ([c (read-char ip)])
              (cond
                [(eof-object? c) (source-error sfd bfp "expected open paren or @ following @")]
                [(eqv? c #\@) (extend-string-buffer! tb c) (s0 a (+ bfp 1))]
                [(eqv? c #\()
                 (unread-char c ip)
                 (let-values ([(e* new-bfp) (read-scheme k ip sfd bfp)])
                   (syntax-case e* ()
                     [(e)
                      (s0
                        (cons*
                          `(check-string-and-indent e ,(source-string sfd bfp) (fx- (column) 1))
                          (extract-string-and-reset! tb)
                          a)
                        new-bfp)]
                     [else (source-error sfd bfp "found more than one expression within @(---)")]))]
                [(initial-id-char? c)
                 (let ([str (extract-string-and-reset! tb)])
                   (extend-string-buffer! tb #\@)
                   (extend-string-buffer! tb c)
                   (s2 (cons str a) (+ bfp 1) bfp))]
                [else (source-error sfd bfp "expected open paren or @ following @")])))
          (define (s2 a bfp token-start-bfp)
            (let ([c (read-char ip)])
              (cond
                [(eof-object? c) (close-input-port ip) (finish-identifier a bfp token-start-bfp)]
                [(id-char? c) (extend-string-buffer! tb c) (s2 a (+ bfp 1) token-start-bfp)]
                [else (unread-char c ip) (finish-identifier a bfp token-start-bfp)])))
          (define (finish-identifier a bfp token-bfp)
            (let* ([token (extract-string-and-reset! tb)]
                   [@kw (datum->syntax k (string->symbol token))]
                   [p (r @kw)])
              (unless p (source-error sfd token-bfp "unrecognized token ~a" token))
              (call-with-values (lambda () (p k ip sfd a bfp token-bfp)) s0)))
          (s0 '() 0)))
      (syntax-case x ()
        [(k fn)
         (string? (datum fn))
         (lambda (r)
           (with-syntax ([(e ...) (process-template-file r (datum fn) 'k)])
             '(let ([filename fn])
                 (let-values ([(op p line column) (open-positional-string-output-port)])
                   (display e op) ...
                   (p)))))])))

  (define-syntax define-template-extension
    (lambda (x)
      (define who 'define-template-extension)
      (define (make-prefix-id prefix kw)
        (datum->syntax kw
          (string->symbol
            (string-append prefix (symbol->string (syntax->datum kw))))))
      (define build-matcher
        (case-lambda
          [(kw)
           (with-syntax ([kw kw] [@kw (make-prefix-id "@" kw)])
             '[@kw (lambda (k ip sfd stack bfp token-bfp)
                      (let-values ([(stack bfp) (strip-blanks ip stack bfp)])
                        (values (cons (make-incomplete-node 'kw #f token-bfp) stack) bfp)))])]
          [(kw expr)
           (with-syntax ([kw kw] [@kw (make-prefix-id "@" kw)] [(expr ...) expr])
             '[@kw (lambda (k ip sfd stack bfp token-bfp)
                      (let-values ([(e* new-bfp) (read-scheme k ip sfd bfp)])
                        (syntax-case e* ()
                          [(expr ...)
                           (let-values ([(stack new-bfp) (strip-blanks ip stack new-bfp)])
                             (values (cons (make-incomplete-node 'kw e* token-bfp) stack) new-bfp))]
                          [_ (source-error sfd token-bfp "expected @~s~s syntax, but got @~s~s"
                               'kw '(expr ...) 'kw (syntax->datum e*))])))])]))
      (define (check-id id)
        (let* ([str (symbol->string (syntax->datum id))]
               [len (string-length str)])
          (unless (and (> len 0) (initial-id-char? (string-ref str 0))
                       (let loop ([len len])
                         (or (= len 0)
                             (let ([len (- len 1)])
                               (and (id-char? (string-ref str len)) (loop len))))))
            (syntax-violation who "invalid template keyword" id))))
      (define (check-unique! type ids)
        (let loop ([ids ids])
          (syntax-case ids ()
            [(id rest ...)
             (if (memq (datum id) (datum (rest ...)))
                 (syntax-violation who (format "one or more ~a used more than once" type) 'id '(rest ...))
                 (loop '(rest ...)))]
            [() (void)])))
      (define (check-syntax-unique! type maybe-expr*)
        (check-unique! type
          (let f ([stx maybe-expr*] [ids '()])
            (syntax-case stx ()
              [id (and (identifier? 'id) (not (memq (datum id) '(... unquote quote)))) (cons 'id ids)]
              [(a . d) (f 'a (f 'd ids))]
              [_ ids]))))
      (define (build-check kw tmpl x)
        `(unless ,(if x `(and ,x ,tmpl) tmpl)
            (source-error sfd token-bfp "found ~s without required ~s" token ',kw)))
      (define (build-initial-values bindings list?*)
        (fold-right (lambda (binding list? init-val**)
                      (cons
                        (if list?
                            (make-list (length binding) ''())
                            (make-list (length binding) '#f))
                        init-val**))
          '() bindings list?*))
      (define (build-bodies list?* tmpls updates bindings)
        (let f ([list?* list?*] [tmpls tmpls] [updates updates] [bindings bindings] [rbindings '()])
          (if (null? list?*)
              '()
              (with-syntax ([(checks ...)
                             (if (car list?*)
                                 '()
                                 `((when ,(car tmpls)
                                      (source-error token-bfp "found more @~s than expected" type))))]
                            [((args ...) ...) (fold-left (lambda (args binding) (cons binding args))
                                                (cons (car updates) (cdr bindings)) rbindings)])
                (cons '(begin checks ... (loop (cdr stack) '() args ... ...))
                  (f (cdr list?*) (cdr tmpls) (cdr updates) (cdr bindings) (cons (car bindings) rbindings)))))))
      (define (process-template output pat)
        (define (squawk type)
          (syntax-violation who (format "extension cannot start with ~s keyword" type) pat))
        (syntax-case pat (optional)
          [((optional kw (expr ...) tmpl) . rest)
           (and (identifier? 'kw) (identifier? 'tmpl))
           (squawk 'optional)]
          [((optional kw tmpl) . rest)
           (and (identifier? 'kw) (identifier? 'tmpl))
           (squawk 'optional)]
          [((kw (expr ...) tmpl) dots . rest)
           (and (eq? (datum dots) '...) (identifier? 'kw) (identifier? 'tmpl))
           (squawk 'list)]
          [((kw tmpl) dots . rest)
           (and (eq? (datum dots) '...) (identifier? 'kw) (identifier? 'tmpl))
           (squawk 'optional)]
          [(kw (expr ...) tmpl . rest)
           (and (identifier? 'kw) (identifier? 'tmpl))
           (process-rest output 'kw 'rest
             (list (build-matcher 'kw '(expr ...)))
             '([tmpl `(string-append ,@rstack)]
                [(expr ...) (incomplete-node-e* item)]))]
          [(kw tmpl . rest)
           (and (identifier? 'kw) (identifier? 'tmpl))
           (process-rest output 'kw 'rest (list (build-matcher 'kw))
             '([tmpl `(string-append ,@rstack)]))]
          [(kw (expr ...))
           (with-syntax ([@kw (make-prefix-id "@" 'kw)] [output output])
             '([@kw (lambda (k ip sfd stack bfp token-bfp)
                       (let-values ([(e* new-bfp) (read-scheme k ip sfd bfp)])
                         (syntax-case e* ()
                           [(expr ...) (values (cons `output stack) new-bfp)]
                           [_ (source-error sfd token-bfp "expected @~s~s syntax, but got @~s~s"
                                'kw '(expr ...) 'kw (syntax->datum e*))])))]))]
         [(kw)
          (with-syntax ([@kw (make-prefix-id "@" 'kw)] [output output])
            '([@kw (lambda (k ip sfd stack bfp indent token-bfp)
                      (values (cons `output stack) bfp indent))]))]))
      (define (process-rest output first-kw rest as* matches)
        (let f ([pat rest]
                [as* as*]
                [checks '()]
                [kws '()]
                [tmpls '()]
                [list?* '()]
                [bindings '()]
                [updates '()]
                [exprs '()]
                [matches matches])
          (syntax-case pat (optional)
            [((optional kw (expr ...) tmpl) . rest)
             (and (identifier? 'kw) (identifier? 'tmpl))
             (with-syntax ([(t) (generate-temporaries '(t))])
               (f 'rest
                  (cons (build-matcher 'kw '(expr ...)) as*) checks
                  (cons 'kw kws) (cons 'tmpl tmpls) (cons #f list?*)
                  (cons (list 'tmpl 't) bindings)
                  (cons (list '`(string-append ,@rstack) '(incomplete-node-e* item)) updates)
                  (cons '(expr ...) exprs) (cons* '[tmpl tmpl] '[(expr ...) 't] matches)))]
            [((optional kw tmpl) . rest)
             (and (identifier? 'kw) (identifier? 'tmpl))
             (f 'rest
                (cons (build-matcher 'kw) as*) checks
                (cons 'kw kws) (cons 'tmpl tmpls) (cons #f list?*) (cons (list 'tmpl) bindings)
                (cons (list '`(string-append ,@rstack)) updates)
                (cons  #f exprs) (cons '[tmpl tmpl] matches))]
            [((kw (expr ...) tmpl) dots . rest)
             (and (eq? (datum dots) '...) (identifier? 'kw) (identifier? 'tmpl))
             (with-syntax ([(t*) (generate-temporaries '(t*))])
               (f 'rest
                  (cons (build-matcher 'kw '(expr ...)) as*) checks
                  (cons 'kw kws) (cons 'tmpl tmpls) (cons #t list?*) (cons (list 'tmpl 't*) bindings)
                  (cons (list '(cons `(string-append ,@rstack) tmpl) '(cons (incomplete-node-e* item) t*)) updates)
                  (cons '(expr ...) exprs) (cons* '[(tmpl (... ...)) tmpl] '[((expr ...) (... ...)) t*] matches)))]
            [((kw tmpl) dots . rest)
             (and (eq? (datum dots) '...) (identifier? 'kw) (identifier? 'tmpl))
             (f 'rest
                (cons (build-matcher 'kw) as*) checks
                (cons 'kw kws) (cons 'tmpl tmpls) (cons #t list?*) (cons (list 'tmpl) bindings)
                (cons (list '(cons `(string-append ,@rstack) tmpl)) updates)
                (cons #f exprs) (cons* '[(tmpl (... ...)) tmpl] matches))]
            [(kw (expr ...) tmpl . rest)
             (and (identifier? 'kw) (identifier? 'tmpl))
             (with-syntax ([(t) (generate-temporaries '(t))])
               (f 'rest
                  (cons (build-matcher 'kw '(expr ...)) as*)
                  (cons (build-check 'kw 'tmpl 't) checks)
                  (cons 'kw kws) (cons 'tmpl tmpls) (cons #f list?*) (cons (list 'tmpl 't) bindings)
                  (cons (list '`(string-append ,@rstack) '(incomplete-node-e* item)) updates)
                  (cons '(expr ...) exprs) (cons* '[tmpl tmpl] '[(expr ...) 't] matches)))]
            [(kw tmpl . rest)
             (and (identifier? 'kw) (identifier? 'tmpl))
             (f 'rest
                (cons (build-matcher 'kw) as*) 
                (cons (build-check 'kw 'tmpl #f) checks)
                (cons 'kw kws) (cons 'tmpl tmpls) (cons #f list?*) (cons (list 'tmpl) bindings)
                (cons (list '`(string-append ,@rstack)) updates)
                (cons  #f exprs) (cons '[tmpl tmpl] matches))]
            [() 
             (begin
               (for-each check-id kws)
               (check-unique! "keyword" kws)
               (check-unique! "template bindings" tmpls)
               (check-syntax-unique! "scheme syntax matching expressions" exprs)
               (cons
                 (with-syntax ([startkw first-kw]
                               [endkw (make-prefix-id "end" first-kw)]
                               [@endkw (make-prefix-id "@end" first-kw)]
                               [output output]
                               [(matches ...) matches]
                               [(checks ...) checks]
                               [((x ...) ...) bindings]
                               [((init-val ...) ...) (build-initial-values bindings list?*)]
                               [(kw ...) kws]
                               [(body ...) (build-bodies list?* tmpls updates bindings)])
                   '[@endkw (lambda (k ip sfd stack bfp token-bfp)
                               (let-values ([(stack bfp) (strip-blanks ip stack bfp)])
                                 (let loop ([stack stack] [rstack '()] [x init-val] ... ...)
                                   (if (null? stack)
                                       (source-error sfd token-bfp "found @~s with no initial @~s" 'endkw 'startkw)
                                       (let ([item (car stack)])
                                         (if (incomplete-node? item)
                                             (let ([type (incomplete-node-type item)])
                                               (case type
                                                 [(startkw) checks ...
                                                   (with-syntax (matches ...)
                                                     (values (cons `output (cdr stack)) bfp))]
                                                 [(kw) body] ...
                                                 [else (source-error sfd token-bfp
                                                         "found unexpected @~s (~a) instead of expected @~s before @~s"
                                                         type (source-string sfd (incomplete-node-bfp item)) 'startkw 'endkw)]))
                                             (loop (cdr stack) (cons item rstack) x ... ...)))))))])
                 as*))]
            [_ (syntax-violation who "unrecognized pattern" pat)])))
      (syntax-case x ()
        [(_ pat output)
         (with-syntax ([([@kw proc] ...) (process-template 'output 'pat)])
           '(begin (define-syntax @kw (make-compile-time-value proc)) ...))])))

  (define-template-extension (num (e)) (number->string e))

  (define-template-extension (for ([binding e] [bindings es] ...) tmpl)
    (with-output-to-string
      (lambda ()
        (for-each (lambda (binding bindings ...) (display tmpl)) e es ...))))

  (define-template-extension (if (expr) tmpl (elif (exprs) tmpls) ... (optional else else-tmpl))
    (if expr
        tmpl
        ,(let f ([exprs '(exprs ...)] [tmpls '(tmpls ...)])
            (if (null? exprs)
                (or 'else-tmpl '"")
                (with-syntax ([expr (car exprs)] [tmpl (car tmpls)] [else (f (cdr exprs) (cdr tmpls))])
                  '(if expr
                        tmpl
                        else)))))))
;;; unify.ss
;;; Copyright (C) 1996 R. Kent Dybvig
;;; from "The Scheme Programming Language, 2ed" by R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

(define unify #f)
(let ()
  ;; occurs? returns true if and only if u occurs in v
  (define occurs?
    (lambda (u v)
      (and (pair? v)
           (let f ((l (cdr v)))
             (and (pair? l)
                  (or (eq? u (car l))
                      (occurs? u (car l))
                      (f (cdr l))))))))

  ;; sigma returns a new substitution procedure extending s by
  ;; the substitution of u with v
  (define sigma
    (lambda (u v s)
      (lambda (x)
        (let f ((x (s x)))
          (if (symbol? x)
              (if (eq? x u) v x)
              (cons (car x) (map f (cdr x))))))))

  ;; try-subst tries to substitute u for v but may require a
  ;; full unification if (s u) is not a variable, and it may
  ;; fail if it sees that u occurs in v.
  (define try-subst
    (lambda (u v s ks kf)
      (let ((u (s u)))
        (if (not (symbol? u))
            (uni u v s ks kf)
            (let ((v (s v)))
              (cond
                ((eq? u v) (ks s))
                ((occurs? u v) (kf "cycle"))
                (else (ks (sigma u v s)))))))))

  ;; uni attempts to unify u and v with a continuation-passing
  ;; style that returns a substitution to the success argument
  ;; ks or an error message to the failure argument kf.  The
  ;; substitution itself is represented by a procedure from
  ;; variables to terms.
  (define uni
    (lambda (u v s ks kf)
      (cond
        ((symbol? u) (try-subst u v s ks kf))
        ((symbol? v) (try-subst v u s ks kf))
        ((and (eq? (car u) (car v))
              (= (length u) (length v)))
         (let f ((u (cdr u)) (v (cdr v)) (s s))
           (if (null? u)
               (ks s)
               (uni (car u)
                    (car v)
                    s
                    (lambda (s) (f (cdr u) (cdr v) s))
                    kf))))
        (else (kf "clash")))))

  ;; unify shows one possible interface to uni, where the initial
  ;; substitution is the identity procedure, the initial success
  ;; continuation returns the unified term, and the initial failure
  ;; continuation returns the error message.
  (set! unify
    (lambda (u v)
      (uni u
           v
           (lambda (x) x)
           (lambda (s) (s u))
           (lambda (msg) msg)))))
