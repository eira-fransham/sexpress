;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


;; see http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node157.html


(define-test test-basic-array-stuff
    " the first block of code defines an 8x8 array, then fills
      the elements with a checkerboard pattern"
  (let ((chess-board))
    (setf chess-board (make-array '(8 8)))
    "this dotimes is an iterator which loops x over integers 0 to 7"
    (dotimes (x 8)
      (dotimes (y 8)
        (if (evenp (+ x y))
            (setf (aref chess-board x y) :black)
            (setf (aref chess-board x y) :white)
            )))
    (assert-true (typep chess-board 'array))
    (assert-equal (aref chess-board 0 0) ___)
    (assert-equal (aref chess-board 2 3) ___)
    "array-rank returns the number of dimensions of the array"
    (assert-equal ___ (array-rank chess-board))
    "array-dimensions returns a list of the cardinality of the array dims"
    (assert-equal ___ (array-dimensions chess-board))
    (assert-equal ___ (array-total-size chess-board))))

(define-test test-make-your-own-array
    "make your own array that meets the specifications below."
  (let ((color-cube nil))
    "you may need to modify your array after you make it"
    (if (typep color-cube '(simple-array T (3 3 3)))
        (progn
          (assert-equal 3 (array-rank color-cube))
          (assert-equal '(3 3 3) (array-dimensions color-cube))
          (assert-equal 27 (array-total-size color-cube))
          (assert-equal (aref color-cube 0 1 2) :red)
          (assert-equal (aref color-cube 2 1 0) :white))
        (assert-true nil))))


(define-test test-adjustable-array
    "one may build arrays that can change size"
  (let ((x (make-array '(2 2) :initial-element 5 :adjustable t)))
    (assert-equal (aref x 1 0) ____)
    (assert-equal (array-dimensions x) ____)
    (adjust-array x '(3 4))
    (assert-equal (array-dimensions x) ____)))


(define-test test-make-array-from-list
  (let ((x))
    (setf x (make-array '(4) :initial-contents '(:one :two :three :four)))
    (assert-equal (array-dimensions x) ____)
    (assert-equal ____ (aref x 0))))


(define-test test-row-major-index
    "row major indexing is a way to access elements with a single integer,
     rather than a list of integers"
  (let ((my-array nil))
    (setf my-array (make-array '(2 2 2 2)))
    (dotimes (i (* 2 2 2 2))
      (setf (row-major-aref my-array i) i))
    (assert-equal (aref my-array 0 0 0 0) ____)
    (assert-equal (aref my-array 1 1 1 1) ____)))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


; Concept: What do you do to go through the lisp koans?  You fill in
; the blanks, or otherwise fix the lisp code so that the
; code within the 'define-test' blocks passes.


; In common lisp, "True" and "False" are represented by "t" and "nil".
; More in a future lesson, but for now, consider t to be true,
; and nil to be false.


(define-test assert-true
    "t is true.  Replace the blank with a t"
    (assert-true ___))

(define-test assert-false
    "nil is false"
    (assert-false ___))

(define-test fill-in-the-blank
    "sometimes you will need to fill the blank to complete"
    (assert-equal 2 ___))

(define-test fill-in-the-blank-string
    (assert-equal ___ "hello world"))

(define-test test-true-or-false
    "sometimes you will be asked to evaluate whether statements
     are true (t) or false (nil)"
    (true-or-false? ___ (equal 34 34))
    (true-or-false? ___ (equal 19 78)))


;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


(define-test test-list-or-atom
    "Lists in lisp are forms beginning and ending with rounded parentheses.
     Atoms are symbols, numbers, or other forms usually separated by
     white-space or parentheses.  The function 'listp' will return true if
     the input is a list.  The function 'atom' will return true if the
     input is an atom."
  (true-or-false? ___ (listp '(1 2 3)))
  (true-or-false? ___ (atom '(1 2 3)))

  (true-or-false? ___ (listp '("heres" "some" "strings")))
  (true-or-false? ___ (atom '("heres" "some" "strings")))

  (true-or-false? ___ (listp "a string"))
  (true-or-false? ___ (atom "a string"))

  (true-or-false? ___ (listp 2))
  (true-or-false? ___ (atom 2))

  (true-or-false? ___ (listp '(("first" "list") ("second" "list"))))
  (true-or-false? ___ (atom '(("first" "list") ("second" "list")))))


(define-test test-empty-list-is-both-list-and-atom
    "the empty list, nil, is unique in that it is both a list and an atom"
  (true-or-false? ___ (listp nil))
  (true-or-false? ___ (atom nil)))


(define-test test-keywords
    "symbols like :hello or :like-this are treated differently in lisp.
     Called keywords, they are symbols that evaluate to themselves."
  (true-or-false? ___ (equal :this-is-a-keyword :this-is-a-keyword))
  (true-or-false? ___ (equal :this-is-a-keyword ':this-is-a-keyword)))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


;; CLOS stands for Common Lisp Object System.
;; CLOS is common lisps' object oriented framework.

(defclass racecar () (color speed))

(define-test test-defclass
    (let ((car-1 (make-instance 'racecar))
          (car-2 (make-instance 'racecar)))
      (setf (slot-value car-1 'color) :red)
      (setf (slot-value car-1 'speed) 220)
      (setf (slot-value car-2 'color) :blue)
      (setf (slot-value car-2 'speed) 240)
      (assert-equal ____ (slot-value car-1 'color))
      (assert-equal ____ (slot-value car-2 'speed))))

;; CLOS provides functionality for creating getters / setters
;; for defined objects

(defclass spaceship ()
  ((color :reader get-color :writer set-color)
   (speed :reader get-speed :writer set-speed)))

(define-test test-clos-getters-and-setters
    (let ((ship-1 (make-instance 'spaceship)))
      (set-color :orange ship-1)
      (assert-equal ____ (get-color ship-1))
      (set-speed 1000 ship-1)
      (assert-equal ____ (get-speed ship-1))))

;; CLOS also provides functionality to create accessors
;; to object data.

;; stores a value, and a counter which tallies accesses, read or write,
;; to that value
(defclass value-with-access-counter ()
  ((value :reader get-value :writer set-value :initform 0)
   (access-count :reader how-many-value-queries :initform 0)))

(defmethod get-value ((object value-with-access-counter))
           (incf (slot-value object 'access-count))
           (slot-value object 'value))

(defmethod set-value (new-value (object value-with-access-counter))
           (incf (slot-value object 'access-count))
           (setf (slot-value object 'value) new-value))

(define-test test-access-counter
    (let ((x (make-instance 'value-with-access-counter)))
      ; check that no one has ever looked at the x value yet.
      (assert-equal ____ (how-many-value-queries x))
      ; check that the default value is zero.
      (assert-equal ___ (get-value x))
      ; now that we've looked at it, there is a single access.
      (assert-equal ___ (how-many-value-queries x))
      ; check that we can set and read the value
      (set-value 33 x)
      (assert-equal 33 (get-value x))
      (assert-equal ___ (how-many-value-queries x))))


; countdowner has a value which goes down every time you look at it
; and returns "bang" when it hits zero.
(defclass countdowner ()
  ((value :initform 4)))

;; Write the get-value for the countdowner
;; to satisfy the test-countdowner tests.
;; you may be interested in the 'decf function.
(defmethod get-value ((object countdowner))
  :WRITE-ME)


(define-test test-countdowner
    (let ((c (make-instance 'countdowner)))
      (assert-equal 3 (get-value c))
      (assert-equal 2 (get-value c))
      (assert-equal 1 (get-value c))
      (assert-equal "bang" (get-value c))
      (assert-equal "bang" (get-value c))))


;; Classes can inherit data and methods from other classes.
;; Here, the specific CIRCLE class extends the generic SHAPE class
(defclass shape ()
  ((kind :reader get-kind :writer set-kind :initform :default-shape-kind)
   (pos :reader get-pos :writer set-pos :initform '(0 0))))

(defclass circle (shape)
  ((radius :reader get-radius :writer set-radius :initform 0)))

(define-test test-inheritance
    (let ((circle-1 (make-instance 'circle))
          (shape-1 (make-instance 'shape)))
      (assert-equal ____ (type-of shape-1))
      (assert-equal ____ (type-of circle-1))
      (true-or-false? ____ (typep circle-1 'circle))
      (true-or-false? ____ (typep circle-1 'shape))
      (set-kind :circle circle-1)
      (set-pos '(3 4) circle-1)
      (set-radius 5 circle-1)
      (assert-equal ____ (get-pos circle-1))
      (assert-equal ____ (get-radius circle-1))))

;; Classes may also inherit from more than one base class.
;; This is known as multiple inheritance.

;; Color holds an rgb triplet and a transparency alpha value.
;; The RGB stands for the amount of red, green, and blue.
;; the alpha (transparency) value is 0 for completely opaque.
;; Note that color also has a kind, like shape.

(defclass color ()
  ((rgb :reader get-rgb :writer set-rgb :initform '(0 0 0))
   (alpha :reader get-alpha :writer set-alpha :initform 0)
   (kind :reader get-kind :writer set-kind :initform :default-color-kind)))

;; The COLORED-CIRCLE class extends both CIRCLE and COLOR.
;; Of particular interest is which "kind" slot will COLORED-CIRCLE get,
;; since both CIRCLE and COLOR provide the "kind" slot.

(defclass colored-circle (color circle) ())
(defclass circled-color (circle color) ())

(define-test test-multiple-inheritance
    (let ((my-colored-circle (make-instance 'colored-circle))
          (my-circled-color (make-instance 'circled-color)))
      (assert-equal ____ (get-kind my-colored-circle))
      (assert-equal ____ (get-kind my-circled-color))))


(defvar *last-kind-accessor* nil)

(defmethod get-kind ((object shape))
           (setf *last-kind-accessor* :shape)
           (slot-value object 'kind))

(defmethod get-kind ((object circle))
           (setf *last-kind-accessor* :circle)
           (slot-value object 'kind))

(defmethod get-kind ((object color))
           (setf *last-kind-accessor* :color)
           (slot-value object 'kind))

;; Precedence order is similarly a depth first search for methods.

(define-test test-multiple-inheritance-method-order
    (let ((my-colored-circle (make-instance 'colored-circle))
          (my-circled-color (make-instance 'circled-color))
          (my-shape (make-instance 'shape))
          (my-circle (make-instance 'circle))
          (my-color (make-instance 'color)))
      (get-kind my-shape)
      (assert-equal ____ *last-kind-accessor*)
      (get-kind my-circle)
      (assert-equal ____ *last-kind-accessor*)
      (get-kind my-color)
      (assert-equal ____ *last-kind-accessor*)
      (get-kind my-colored-circle)
      (assert-equal ____ *last-kind-accessor*)
      (get-kind my-circled-color)
      (assert-equal ____ *last-kind-accessor*)))


;; Todo: consider adding :before and :after method control instructions.

;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


"Common lisp conditions are much like CLOS classes.
They are used to handle exceptional situations, and separate
error handling code from normal operational code."

(define-condition minimal-error-cond (error) ())
(define-condition minimal-warning-cond (warning) ())


(define-test test-conditions-derive-from-types
    "conditions inherit from base types"
  (true-or-false? ___ (typep (make-condition 'minimal-error-cond)
                             'minimal-error-cond))

  (true-or-false? ___ (typep (make-condition 'minimal-error-cond)
                             'error))

  (true-or-false? ___ (typep (make-condition 'minimal-error-cond)
                             'warning))

  (true-or-false? ___ (typep (make-condition 'minimal-warning-cond)
                             'minimal-warning-cond))

  (true-or-false? ___ (typep (make-condition 'minimal-warning-cond)
                             'error))

  (true-or-false? ___ (typep (make-condition 'minimal-warning-cond)
                             'warning)))


;; ----


(define-condition my-div-by-zero-error (error) ())
(define-condition my-non-number-args-error (error) ())

(defun my-divide (num denom)
  (if (or (not (numberp num))
          (not (numberp denom)))
      (error 'my-non-number-args-error))
  (if (= 0 denom)
      (error 'my-div-by-zero-error)
      (/ num denom)))

(define-test assert-error-thrown
    "assert-error checks that the right error is thrown"
  (assert-equal 3 (my-divide 6 2))
  (assert-error 'my-div-by-zero-error (my-divide 6 0))
  (assert-error ____ (my-divide 6 "zero")))


(define-test test-handle-errors
    "the handler case is like a case statement which can capture errors
     and warnings, and execute appropriate forms in those conditions."
  (assert-equal ___
                (handler-case (my-divide 6 2)
                  (my-div-by-zero-error (condition) :zero-div-error)
                  (my-non-number-args-error (condition) :bad-args)))
  (assert-equal ___
                (handler-case (my-divide 6 0)
                  (my-div-by-zero-error (condition) :zero-div-error)
                  (my-non-number-args-error (condition) :bad-args)))
  (assert-equal ___
                (handler-case (my-divide 6 "woops")
                  (my-div-by-zero-error (condition) :zero-div-error)
                  (my-non-number-args-error (condition) :bad-args))))


;; ----

"conditions, as CLOS objects, can have slots, some of which have special
meanings.  Common Lisp the Language Chapter 29 for more details.
http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node312.html"

; This error condition is more than a signal.  It carries data in two slots.
; the "original-line" slot and the "reason" slot.  Both slots have a defined
; :initarg, which they will use to set themselves, if available.  If not,
; they have a default form (:initform).  They also both provide reader functions

(define-condition logline-parse-error (error)
  ((original-line :initarg :original-line :initform "line not given" :reader original-line)
   (reason :initarg :reason :initform "no-reason" :reader reason)))


;; This function is designed to take loglines, and report what type they are.
;; It can also throw errors, like div-by-zero above, but the errors now carry some
;;  additional information carried within the error itself.

(defun get-logline-type (in-line)
  (if (not (typep in-line 'string))
      ;; if the in-line isn't a string, throw a logline-parse-error, and set the :reason and :original-line
      (error 'logline-parse-error :original-line in-line :reason :bad-type-reason))
  (cond
    ((equal 0 (search "TIMESTAMP" in-line)) :timestamp-logline-type)
    ((if (equal 0 (search "HTTP" in-line)) :http-logline-type))
    ;; if we don't recognize the first token,  throw a logline-parse-error, and set the :reason and :original-line
    (t (error 'logline-parse-error :original-line in-line :reason :unknown-token-reason))))


(define-test test-errors-have-slots
    (assert-equal ____
                  (handler-case (get-logline-type "TIMESTAMP y13m01d03")
                    (logline-parse-error (condition) (list (reason condition) (original-line condition)))))
    (assert-equal ____
                  (handler-case (get-logline-type "HTTP access 128.0.0.100")
                    (logline-parse-error (condition) (list (reason condition) (original-line condition)))))
    (assert-equal ____
                  (handler-case (get-logline-type "bogus logline")
                    (logline-parse-error (condition) (list (reason condition) (original-line condition)))))
    (assert-equal ____
                  (handler-case (get-logline-type 5555)
                    (logline-parse-error (condition) (list (reason condition) (original-line condition))))))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

(define-test test-if-then-else
    (let ((result))
      (if t
          (setf result "true value")
          (setf result "false value"))
      (assert-equal result ____)
      (if nil
          (setf result "true value")
          (setf result "false value"))
      (assert-equal result ____)))


(define-test test-when-and-unless
    (let ((result-1 nil)
          (result-2 nil)
          (when-nums nil)
          (unless-nums nil))
      (dolist (x '(1 2 3 4 5 6 7 8 9 10))
        (when (> x 5)
          (setf result-1 x)
          (push x when-nums))
        (unless (> x 5)
          (setf result-2 x)
          (push x unless-nums)))
      (assert-equal result-1 ___)
      (assert-equal result-2 ___)
      (assert-equal when-nums ___)
      (assert-equal unless-nums ___)))


(define-test test-and-short-circuits
    "and only evaluates forms until one evaluates to nil"
  (assert-equal
   ____
   (let ((x 0))
     (and
      (setf x (+ 1 x))
      (setf x (+ 1 x))
      nil ;; <- ends execution of and.
      (setf x (+ 1 x)))
     x)))


(define-test test-or-also-short-circuits
    "or only evaluates until one argument evaluates to non-nil"
  (assert-equal
   ____
   (let ((x 0))
     (or
      (setf x (+ 1 x))
      (setf x (+ 1 x))
      nil
      (setf x (+ 1 x)))
     x)));;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


; based on about_dice_project.rb

;; In this project we are going to build a CLOS class representing
;; a simple set of dice.  There are only two operations on the dice,
;; reading the values, and re-rolling.


;;  YOU WRITE THIS PART:
(defclass dice-set ()
  () ;; WRITE DICE-SET CLASS BODY HERE
)

(defmethod get-values ((object dice-set))
  ;; WRITE GET-VALUES METHOD DEFINITION HERE
)

(defmethod roll (how-many (object dice-set))
  ;; WRITE ROLL METHOD DEFINITION HERE
)


(define-test test-create-dice-set
;; tests making an instance of the dice-set
    (let ((dice (make-instance 'dice-set)))
      (assert-true dice)))


(define-test test-rolling-the-dice-returns-a-set-of-integers-between-1-and-6
;; tests rolling the dice
    (let ((dice (make-instance 'dice-set)))
      (roll 5 dice)
      (assert-true (typep (get-values dice) 'list))
      (assert-equal 5 (length (get-values dice)))
      (dolist (x (get-values dice))
        (assert-true (and (>= x 1)
                          (<= x 6)
                          (typep x 'integer))))))


(define-test test-dice-values-do-not-change-unless-explicitly-rolled
;; tests that dice don't change just by looking at them
    (let ((dice (make-instance 'dice-set)))
      (roll 100 dice)
      (let ((first-time (get-values dice))
            (second-time (get-values dice)))
        (assert-equal first-time second-time))))


(define-test test-dice-values-should-change-between-rolls
;; tests that rolling the dice DOES change the values.
    (let ((dice (make-instance 'dice-set))
          (first-time nil)
          (second-time nil))
      (roll 100 dice)
      (setf first-time (get-values dice))
      (roll 100 dice)
      (setf second-time (get-values dice))
      (assert-false (equal first-time second-time))))

(define-test test-you-can-roll-different-numbers-of-dice
;; tests count parameter of how many dice to roll
    (let ((dice (make-instance 'dice-set)))
      (assert-equal 5 (length (roll 5 dice)))
      (assert-equal 100 (length (roll 100 dice)))
      (assert-equal 1 (length (roll 1 dice)))))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;; the most common equality predicates are eq, eql, equal and equalp
;; eq is similar to comparing c pointers
(define-test test-eq
    "(eq x y) is true if and only if x and y are the same identical object
     eq is like comparing pointers in c.  If the values are EQ, any non-nil
     value may be returned."
  (true-or-false? ___ (eq 'a 'a))
  (true-or-false? ___ (eq 3 3.0))
  (true-or-false? ___ (eq '(1 2) '(1 2)))
  (true-or-false? ___ (eq "Foo" "Foo"))
  (true-or-false? ___ (eq "Foo" (copy-seq "Foo")))
  (true-or-false? ___ (eq "FOO" "Foo")))

(define-test test-eql
    "(eql x y) is true if (eq x y)
     also it is true if x and y are numeric of the same type
     and represent the same number.
     (eql x y) also if x and y are the same characters."
   (true-or-false? ___ (eql 'a 'a))
   (true-or-false? ___ (eql 3 3))
   (true-or-false? ___ (eql 3 3.0))
   (true-or-false? ___ (eql '(1 2) '(1 2)))
   (true-or-false? ___ (eql  '(:a . :b) '(:a . :b)))
   (true-or-false? ___ (eql ))
   (true-or-false? ___ (eql "Foo" "Foo"))
   (true-or-false? ___ (eql "Foo" (copy-seq "Foo")))
   (true-or-false? ___ (eql "FOO" "Foo")))

(define-test test-equal
    "(equal x y) is true if (eql x y), or
     x and y are lists with equal elements, or
     x and y character or bit arrays with equal elements"
   (true-or-false? ___ (equal 'a 'a))
   (true-or-false? ___ (equal 3 3))
   (true-or-false? ___ (equal 3 3.0))
   (true-or-false? ___ (equal '(1 2) '(1 2)))
   (true-or-false? ___ (equal  '(:a . :b) '(:a . :b)))
   (true-or-false? ___ (equal  '(:a . :b) '(:a . :doesnt-match)))
   (true-or-false? ___ (equal ))
   (true-or-false? ___ (equal "Foo" "Foo"))
   (true-or-false? ___ (equal "Foo" (copy-seq "Foo")))
   (true-or-false? ___ (equal "FOO" "Foo")))

(define-test test-equalp
    "(equalp x y) if (equal x y) or
     if x and y are strings with the same characters (case independent).
     if x and y are arrays with the same dimensions and equal elements
     if x and y are numeric of different types but one may be upgraded to
     the other type without loss and still exhibit equality."
   (true-or-false? ___ (equalp 'a 'a))
   (true-or-false? ___ (equalp 3 3))
   (true-or-false? ___ (equalp 3 3.0))
   (true-or-false? ___ (equalp '(1 2) '(1 2)))
   (true-or-false? ___ (equalp  '(:a . :b) '(:a . :b)))
   (true-or-false? ___ (equalp  '(:a . :b) '(:a . :doesnt-match)))
   (true-or-false? ___ (equalp ))
   (true-or-false? ___ (equalp "Foo" "Foo"))
   (true-or-false? ___ (equalp "Foo" (copy-seq "Foo")))
   (true-or-false? ___ (equalp "FOO" "Foo")))

(define-test test-numeric-equal
    "(= x y) is only for numerics
     and can take multiple arguments
     if x or y is not numeric there will be a compiler error."
   (true-or-false? ___ (= 99.0 99 99.000))
   (true-or-false? ___ (= 0 1 -1))
   (true-or-false? ___ (= (/ 2 3) (/ 6 9) (/ 86 129))))

; EQ, EQL, EQUAL, and EQUALP are general equality predicates.
; Additionally, Lisp also provides the type-specific predicates.
; For example, STRING= and STRING-EQUAL are predicates for strings.
(define-test test-string-equal
  "string-equal is just like string= except that differences in case are ignored."
  (true-or-false? ___ (string= "Foo" "Foo"))
  (true-or-false? ___ (string= "Foo" "FOO"))
  (true-or-false? ___ (string= "together" "frog" :start1 1 :end1 3 :start2 2))
  (true-or-false? ___ (string-equal "Foo" "FOO"))
  (true-or-false? ___ (string-equal "together" "FROG" :start1 1 :end1 3 :start2 2)))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


;; based on http://psg.com/~dlamkins/sl/chapter03-02.html

(define-test test-function-name-is-first-argument
    "In most imperative languages, the syntax of a function call has
     the function name succeeded by a list of arguments.  In lisp,
     the function name and arguments are all part of the same list,
     with the function name the first element of that list."

  "in these examples, the function names are +, -, and *"
  (assert-equal ___ (+ 2 3))
  (assert-equal ___ (- 1 3))
  (assert-equal ___ (* 7 4))
  "'>' and '=' are the boolean functions (predicates) 'greater-than' and
   'equal to'"
  (assert-equal ___ (> 100 4))
  (assert-equal ___ (= 3 3))
  "'NUMBERP' is a predicate which returns true if the argument is a number"
  (assert-equal ___ (numberp 5))
  (assert-equal ___ (numberp "five")))


(define-test test-evaluation-order
    "Arguments to functions are evaluated before the function"
  (assert-equal ___ (* (+ 1 2) (- 13 10))))


(define-test test-quoting-behavior
    "Preceding a list with a quote (') will tell lisp not to evaluate a list.
     The quote special form suppresses normal evaluation, and instead returns
     the literal list.
     Evaluating the form (+ 1 2) returns the number 3,
     but evaluating the form '(+ 1 2) returns the list (+ 1 2)"
  (assert-equal ____ (+ 1 2))
  (assert-equal ____ '(+ 1 2))
  "'LISTP' is a predicate which returns true if the argument is a list"
  " the '(CONTENTS) form defines a list literal containing CONTENTS"
  (assert-equal ___ (listp '(1 2 3)))
  (assert-equal ___ (listp 100))
  (assert-equal ___ (listp "Word to your moms I came to drop bombs"))
  (assert-equal ___ (listp nil))
  (assert-equal ___ (listp (+ 1 2)))
  (assert-equal ___ (listp '(+ 1 2)))
  "equalp is an equality predicate"
  (assert-equal ___ (equalp 3 (+ 1 2)))
  "the '(xyz ghi) syntax is syntactic sugar for the (QUOTE (xyz ghi)) function."
  (true-or-false? ___ (equalp '(/ 4 0) (quote (/ 4 0)))))
;; EXTRA CREDIT:
;;
;; Create a program that will play the Greed Game.
;; Rules for the game are in GREED_RULES.TXT.
;;
;; You already have a DiceSet class and score function you can use.
;; Write a player class and a Game class to complete the project.  This
;; is a free form assignment, so approach it however you desire.;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


;; FORMAT is lisp's counterpart to the c function printf. Refer to
;; http://www.gigamonkeys.com/book/a-few-format-recipes.html for more
;; on this topic.


;; FORMAT takes two fixed parameters. The first one specifies an
;; output stream that the result goes to, and if left as nil, FORMAT
;; will return the output as a string instead. The second parameter
;; specifies the format, where format specifier will be replaced by
;; formatting the rest of the parameters.

(define-test test-format-with-plain-text
  "If there is no format specifier, FORMAT just returns the string
   itself."
  (assert-equal ___ (format nil "this is plain text.")))

(define-test test-format-with-general-specifier
  "~a is a general specifier that translates to the print form of a
    parameter."
  (assert-equal ___ (format nil "~a" 42))
  (assert-equal ___ (format nil "~a" ))
  (assert-equal ___ (format nil "~a" "galaxy far far away"))
  ;; ~a can also translate to list
  ;; and parameters to FORMAT are passed by value
  (assert-equal ___
                (format nil "~a evaluates to ~a"
                        '(/ 8 (- 3 (/ 8 3)))
                        (/ 8 (- 3 (/ 8 3))))))

(define-test some-fancy-specifiers
  "format enclosed by ~{ and ~} applies to every element in a list."
  (assert-equal ___
                (format nil "~{[~a]~}" '(1 2 3 4)))
  ;; ~^ within the ~{ ~} stops processing the last element in the list.
  (assert-equal "1|2|3|4|" (format nil ___ '(1 2 3 4)))
  (assert-equal ___ (format nil "~{~a~^|~}" '(1 2 3 4)))
  ;; ~r reads the integer
  (assert-equal ___ (format nil "~r" 42))
  ;; put them all together
  (assert-equal ___
                (format nil "~{~r~^,~}" '(1 2 3 4))))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


; borrows from about_methods.py

(defun some-named-function (a b)
  (+ a b))

(define-test test-call-a-function
    "DEFUN defines global functions"
  (assert-equal ___ (some-named-function 7 11)))


(define-test test-shadow-a-function
    "Local functions are defined with FLET or LABELS.  One major difference
     between the two is that local functions defined with LABELS may refer
     to themselves, whereas local functions defined with FLET may not."
   (assert-eq 18 (some-named-function 7 11))
   "flet binds a function to a name within a lexical environment"
   (flet ((some-named-function (a b) (* a b)))
     (assert-equal ___ (some-named-function 7 11)))
   (assert-equal ___  (some-named-function 7 11)))


; borrowed from Common Lisp The Language chapter 5.2.2
(defun func-with-opt-params (&optional (a 2) (b 3) )
  ; each optional parameter has a form like (var default-val)
  (list a b))

(define-test test-optional-parameters
    "Optional parameters are filled in with their default value."
   (assert-equal (func-with-opt-params :test-1 :test-2) ___)
   (assert-equal (func-with-opt-params :test-1) ___)
   (assert-equal (func-with-opt-params) ___))


;; ----


(defun func-with-opt-params-and-indication (&optional (a 2 a?) (b 3 b?))
  (list a a? b b?))

(define-test test-optional-parameters-with-indication
   "Common Lisp optional params may bind a symbol which indicate whether the
    value was provided or defaulted.  Each optional parameter binding has the
    form (var default-form supplied-p)."
   (assert-equal (func-with-opt-params-and-indication :test-1 :test-2) ___)
   (assert-equal (func-with-opt-params-and-indication :test-1) ___)
   (assert-equal (func-with-opt-params-and-indication) ___))


;; ----


(defun func-with-rest-params (&rest x)
  x)

(define-test test-func-with-rest-params
  "With &rest, the remaining params, are handed in as a list.  Remaining
   arguments (possibly none) are collected into a list."
  (assert-equal (func-with-rest-params) ___)
  (assert-equal (func-with-rest-params 1) ___)
   (assert-equal (func-with-rest-params 1 :two 333) ___))


;; ----


(defun func-with-key-params (&key a b)
  (list a b))

(define-test test-key-params ()
  "Key params allow the user to specify params in any order"
   (assert-equal (func-with-key-params) ___)
   (assert-equal (func-with-key-params :a 11 :b 22) ___)
   ; it is not necessary to specify all key parameters
   (assert-equal (func-with-key-params :b 22) ___)
   ; order is not important
   (assert-equal (func-with-key-params :b 22 :a 0) ___))

(defun func-key-params-can-have-defaults (&key  (a 3 a?) (b 4 b?))
  (list a a? b b?))

(define-test test-key-params-can-have-defaults
    "key parameters can have defaults also"
   (assert-equal (func-key-params-can-have-defaults) ____)
   (assert-equal (func-key-params-can-have-defaults :a 3 :b 4) ___)
   (assert-equal (func-key-params-can-have-defaults :a 11 :b 22) ___)
   (assert-equal (func-key-params-can-have-defaults :b 22) ___)
   ; order is not important
   (assert-equal (func-key-params-can-have-defaults :b 22 :a 0) ___))


;; ----


;; borrowed from common lisp the language 5.2.2
(defun func-with-funky-parameters (a &rest x &key b (c a))
   (list a b c x))

(define-test test-many-kinds-params
    "CL provides the programmer with more than enough rope to hang himself."
   (assert-equal (func-with-funky-parameters 1) ___)
   (assert-equal (func-with-funky-parameters 1 :b 2) ___)
   (assert-equal (func-with-funky-parameters 1 :b 2 :c 3) ___)
   (assert-equal (func-with-funky-parameters 1 :c 3 :b 2) ___))


;; Note that &rest parameters have to come before &key parameters.
;; This is an error: (defun f (&key a &rest x) () )
;; But this is ok:   (defun f (&rest x &key a) () )


(define-test test-lambdas-are-nameless-functions
    "A lambda form defines a function, but with no name.  It is possible
     to execute that function immediately, or put it somewhere for later use."
   (assert-equal 19 ((lambda (a b) (+ a b)) 10 9))
  (let ((my-function))
    (setf my-function (lambda (a b) (* a b)))
    (assert-equal ___ (funcall my-function 11 9)))
  (let ((list-of-functions nil))
    (push (lambda (a b) (+ a b)) list-of-functions)
    (push (lambda (a b) (* a b)) list-of-functions)
    (push (lambda (a b) (- a b)) list-of-functions)
    (assert-equal ___ (funcall (second list-of-functions) 2 33))))

(define-test test-lambdas-can-have-optional-params
   (assert-equal ___ ((lambda (a &optional (b 100)) (+ a b)) 10 9))
   (assert-equal ___ ((lambda (a &optional (b 100)) (+ a b)) 10)))


; returns sign x
(defun sign-of (x)
  (if (< x 0) (return-from sign-of -1))
  (if (eq x 0) (return-from sign-of 0))
  1)

(define-test test-return-from-function-early
   (assert-equal (sign-of -5.5) ___)
   (assert-equal (sign-of 0) ___)
   (assert-equal (sign-of ___) 1))


;; ----


;; Lambdas create "lexical closures", meaning that the resulting function, when
;; called, will execute in an environment wherein the lexical bindings to all
;; referred to names still apply.
;; This example from "Common Lisp The Language" Ch. 7

(defun adder (x)
  "The result of (adder n) is a nameless function with one parameter.
  This function will add n to its argument."
  (lambda (y) (+ x y)))

(define-test test-lexical-closure-over-adder ()
  (let ((add-100 (adder 100))
        (add-500 (adder 500)))
  "add-100 and add-500 now refer to different bindings to x"
   (assert-equal ___ (funcall add-100 3))
   (assert-equal ___ (funcall add-500 3))))


;; ----


;; The closure gives the returned function access to the bindings, not just the
;; values.  This means that two functions which close over the same variables
;; will always see the same values of those variables if one does a setq.

(defun two-funs (x)
  "Returns a list of two functions.
   The first takes no parameters and returns x.
   The second takes one parameter, y, and resets x to the value of y."
  (list (function (lambda () x))
        (function (lambda (y) (setq x y)))))

(define-test test-lexical-closure-interactions
    "An illustration of how lexical closures may interact."
  (let ((tangled-funs-1 (two-funs 1))
        (tangled-funs-2 (two-funs 2)))
     (assert-equal (funcall (first tangled-funs-1)) ___)
     (funcall (second tangled-funs-1) 0)
     (assert-equal (funcall (first tangled-funs-1)) ___)

     (assert-equal (funcall (first tangled-funs-2)) ___)
     (funcall (second tangled-funs-2) 100)
     (assert-equal (funcall (first tangled-funs-2)) ___)))


(define-test test-apply-function-with-apply
  "APPLY calls the function parameter on a list of all the remaining
   parameters"
  (let (f1 f2 f3)
    (setq f1 '+)
    (setq f2 '-)
    (setq f3 'max)

    (assert-equal ___ (apply f1 '(1 2)))
    (assert-equal ___ (apply f2 '(1 2)))

    ; after the function name, the parameters are consed onto the front
    ; of the very last parameter
    (assert-equal ___ (apply f1 1 2 '(3)))
    (assert-equal ___ (apply f3 1 2 3 4 '()))))


(define-test test-apply-function-with-funcall
  "FUNCALL calls the function parameter on a list of all the remaining
   parameters.  Remaining params do not expect a final list."
  (let (f1 f2 f3)
    (setq f1 '+)
    (setq f2 '-)
    (setq f3 'max)
    (assert-equal ___ (funcall f1 1 2))
    (assert-equal ___ (funcall f2 1 2))
    (assert-equal ___ (funcall f1 1 2 3))
    (assert-equal ___ (funcall f3 1 2 3 4))))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


; based on python koans: about_dictionaries.py


(define-test test-create-hash-table
    "make hash table with make-hash-table"
  (let ((my-hash-table))
    (setf my-hash-table (make-hash-table))
    (true-or-false? ___ (typep my-hash-table 'hash-table))
    (true-or-false? ___  (hash-table-p my-hash-table))
    (true-or-false? ___  (hash-table-p (make-array '(3 3 3))))
    (assert-equal ___ (hash-table-count my-hash-table))))


(define-test test-hash-table-access
    "gethash is for accessing hash tables"
  (let ((table-of-cube-roots (make-hash-table)))

  "assign the key-value pair 1->'uno'"
  (setf (gethash 1 table-of-cube-roots) "uno")
  (assert-equal "uno" (gethash 1 table-of-cube-roots))
  (assert-equal 1 (hash-table-count table-of-cube-roots))

  (setf (gethash 8 table-of-cube-roots) 2)
  (setf (gethash -3 table-of-cube-roots) -27)
  (assert-equal ___ (gethash -3 table-of-cube-roots))
  (assert-equal ___ (hash-table-count table-of-cube-roots))

  "accessing unset keys returns nil"
  (assert-equal ___ (gethash 125 table-of-cube-roots))))


(define-test test-hash-key-equality
    "hash tables need to know how to tell if two keys are equivalent.
     The programmer must be careful to know which equality predicate is right."
  (let ((hash-table-eq nil)
        (hash-table-equal nil)
        (hash-table-default nil))

    "define three hash tables, with different equality tests"
    (setf hash-table-eq (make-hash-table :test 'eq))
    (setf hash-table-equal (make-hash-table :test 'equal))
    (setf hash-table-default (make-hash-table))

    "add the same string twice, to each"
    (setf (gethash "one" hash-table-eq) "uno")
    (setf (gethash "one" hash-table-eq) "uno")

    (setf (gethash "one" hash-table-equal) "uno")
    (setf (gethash "one" hash-table-equal) "uno")

    (setf (gethash "one" hash-table-default) "uno")
    (setf (gethash "one" hash-table-default) "uno")

    "count how many unique key-value pairs in each"
    (assert-equal ___ (hash-table-count hash-table-eq))
    (assert-equal ___ (hash-table-count hash-table-equal))
    (assert-equal ___ (hash-table-count hash-table-default))))


(define-test test-hash-table-equality
    (let ((h1 (make-hash-table :test 'equal))
          (h2 (make-hash-table :test 'equal)))
      (setf (gethash "one" h1) "yat")
      (setf (gethash "one" h2) "yat")
      (setf (gethash "two" h1) "yi")
      (setf (gethash "two" h2) "yi")
      (true-or-false? ___ (eq h1 h2))
      (true-or-false? ___ (equal h1 h2))
      (true-or-false? ___ (equalp h1 h2))))


(define-test test-changing-hash-tables
    (let ((babel-fish (make-hash-table :test 'equal))
          (expected (make-hash-table :test 'equal)))
      (setf (gethash "one" babel-fish) "uno")
      (setf (gethash "two" babel-fish) "dos")
      (setf (gethash "one" expected) "eins")
      (setf (gethash "two" expected) "zwei")

      (setf (gethash "one" babel-fish) "eins")
      (setf (gethash "two" babel-fish) ____)

      (assert-true (equalp babel-fish expected))))


(define-test test-hash-key-membership
    "hash tables use multiple value return to tell you if the key exists"
    (let ((prev-pres (make-hash-table :test 'equal))
          (value-and-exists nil))
      (setf (gethash "Obama" prev-pres) "Bush")
      (setf (gethash "Lincoln" prev-pres) "Buchanan")
      (setf (gethash "Washington" prev-pres) nil)

      (setf value-and-exists (multiple-value-list (gethash "Obama" prev-pres)))
      (assert-equal value-and-exists '("Bush" t))
      (setf value-and-exists (multiple-value-list (gethash "Lincoln" prev-pres)))
      (assert-equal value-and-exists ____)
      (setf value-and-exists (multiple-value-list (gethash "Washington" prev-pres)))
      (assert-equal value-and-exists ____)
      (setf value-and-exists (multiple-value-list (gethash "Franklin" prev-pres)))
      (assert-equal value-and-exists ____)))


(define-test test-make-your-own-hash-table
    "make a hash table that meets the following conditions"
  (let ((colors (make-hash-table))
        values)

    (assert-equal (hash-table-count colors) 4)
    (setf values (list (gethash "blue" colors)
                       (gethash "green" colors)
                       (gethash "red" colors)))
    (assert-equal values '((0 0 1) (0 1 0) (1 0 0)))))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


;; There are many options for iteration in lisp.
;; This set of koans will introduce a few of the most common ones


;; Dolist evaluates a form for every element of a list.

(defvar some-primes '(10301 11311 19991 999565999))

(define-test test-dolist
    "'dolist' iterates over values in a list, binding each value to a lexical
      variable in turn"
  (let ((how-many-in-list 0)
        (biggest-in-list (first some-primes)))
    "this dolist loops over some-primes, defined above"
    (dolist (one-prime some-primes)
      (if (> one-prime biggest-in-list)
          (setf biggest-in-list one-prime))
      (incf how-many-in-list))
    (assert-equal ___ how-many-in-list)
    (assert-equal ___ biggest-in-list))
  (let ((sum 0))
    "write your own dolist here to calculate the sum of some-primes"
    "you may be interested in investigating the 'incf' function"
    ;(dolist ... )
    (assert-equal 999607602 sum)))


(define-test test-dolist-with-return
    "Dolist can accept a return variable, which will be the return value
     upon completion of the iteration."
    (let ((my-list '(1 2 3 4))
          (my-return))
      (dolist (x my-list my-return)
        (push (* x x) my-return))
      (assert-equal ____ my-return)))


(define-test test-dotimes
    "'dotimes' iterates over the integers from 0 to (limit - 1),
      binding them in order to your selected symbol."
    (let ((out-list nil))
      (dotimes (y 3) (push y out-list))
      (assert-equal out-list ___)))


(defvar *x* "global")
(define-test test-dotimes-binding
    "dotimes establishes a local lexical binding which may shadow
     a global value."
  (dotimes (*x* 4)
    (true-or-false? ___ (equal "global" *x*)))
  (true-or-false? ___ (equal "global" *x*)))


(define-test test-loop-until-return
    "Loop loops forever, unless some return condition is executed.
     Note that the loop macro includes many additional options,
     which will be covered in a future koan."
    (let ((loop-counter 0))
      (loop
        (incf loop-counter)
        (if (>= loop-counter 100) (return loop-counter)))
      (assert-equal ___ loop-counter)))


(define-test test-mapcar
    "mapcar takes a list and a function.  It returns a new list
     with the function applied to each element of the input"
  (let ((mc-result (mapcar 'evenp '(1 2 3 4 5))))
    (assert-equal mc-result ____)))


;; ----


(defun vowelp (c)
  "returns true if c is a vowel"
  (find c "AEIOUaeiou"))

(defun vowels-to-xs (my-string)
  "converts all vowels in a string to the character 'x'"
  (coerce
   (loop for c across my-string
         with new-c
         do (setf new-c (if (vowelp c) c))
         collect new-c)
   'string))

(define-test test-mapcar-with-defun
  "mapcar is a convenient way to apply a function to a collection"
  (assert-equal (vowels-to-xs "Astronomy") "xstrxnxmy")
  (let* ((subjects '("Astronomy" "Biology" "Chemistry" "Linguistics"))
         (mc-result (mapcar 'vowels-to-xs subjects)))
    (assert-equal mc-result ____)))


;; ----

(define-test test-mapcar-with-lambda
    (let ((mc-result (mapcar (lambda (x) (mod x 10)) '(21 152 403 14))))
      (assert-equal mc-result ____)))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


;; based on python koans 'about_lists.py'
;; based also on "Lisp 3rd Edition" ch. 17. "List storage, surgery and reclamation"


(define-test test-creating-lists
    "lists can be created using the quote form, or the 'list' function"
  (let ((fruits nil)
        (some-evens nil))
    (setf fruits '(orange pomello clementine))
    (setf some-evens (list (* 2 1) (* 2 2) (* 2 3)))
    (assert-equal fruits ___)
    (assert-equal ___ (length some-evens))))


(define-test test-list-cons
    "cons CONStructs new lists, by prefixing some list with
     a new element like (cons new-element some-list)"
    (let ((nums nil))
      (setf nums (cons :one nums))
      (assert-equal '(:one) nums)

      (setf nums (cons :two nums))
      (assert-equal ___ nums)

      "lists can contain anything, even mixtures of different things"
      (setf nums (cons 333 nums))
      (assert-equal ___ nums)

      "lists can of course contain lists"
      (setf nums (cons '("the" "rest") nums))
      (assert-equal ___ nums)))


(define-test test-push-pop
    (let ((stack '(10 20 30 40))
          (firstval nil))
      "push adds an element to the beginning of a list referred to by some symbol"
      (push "last" stack)
      (assert-equal '("last" 10 20 30 40) stack)

       "pop is the opposite of push.
       It removes and returns the first element of a list"
      (setf firstval (pop stack))
      (assert-equal "last" firstval)
      (assert-equal '(10 20 30 40) stack)

      (setf firstval (pop stack))
      (assert-equal ___ firstval)
      (assert-equal ___ stack)))


(define-test test-append
    "append attaches one list to the end of another."
  (assert-equal '(:a :b :c) (append '(:a :b) '(:c)))

  (let ((abc '(:a :b :c))
        (xyz '(:x :y :z))
        (abcxyz nil))
    (setf abcxyz (append abc xyz))
    (assert-equal ___ abc)
    (assert-equal ___ xyz)
    (assert-equal ___ abcxyz)))


(define-test test-accessing-list-elements
    (let ((noms '("peanut" "butter" "and" "jelly")))
      (assert-equal "peanut" (first noms))
      (assert-equal ___ (second noms))
      (assert-equal ___ (fourth noms))
      "last returns a singleton list of the final element"
      (assert-equal ___ (last noms))
      (assert-equal "butter" (nth 1 noms)) ; k 1
      (assert-equal ___ (nth 0 noms))
      (assert-equal ___ (nth 2 noms))
      "'elt' is similar to 'nth', with the arguments reversed"
      (assert-equal ___ (elt noms 2))))


(define-test test-slicing-lists
    (let ((noms '("peanut" "butter" "and" "jelly")))
      (assert-equal ___ (subseq noms 0 1))
      (assert-equal ___ (subseq noms 0 2))
      (assert-equal ___ (subseq noms 2 2))
      (assert-equal ___ (subseq noms 2))))


(define-test test-list-breakdown
    "car (aka. 'first') returns the first value in a list"
  (assert-equal ___ (car '(1 2 3)))
  (assert-equal ___ (car nil))
    "cdr (aka. 'rest') refers to the remainder of the list,
     after the first element"
  (assert-equal ___ (cdr '(1 2 3)))
  (assert-equal ___ (cdr nil)))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;; see http://www.gigamonkeys.com/book/loop-for-black-belts.html
;; "Loop for blackbelts" for more on the loop macro.

(define-test test-basic-loop
    (let* ((letters '(:a :b :c :d))
           (loop-result
             (loop for letter in letters
                   collect letter)))
      (assert-equal loop-result ____)))


(define-test test-compound-loop
    (let* ((letters '(:a :b :c :d))
           (loop-result
             (loop for letter in letters
                   for i from 1 to 1000
                   collect (list i letter))))
      (assert-equal loop-result ____)))


(define-test test-counting-loop-skip-by-syntax
   "with multiple 'for' clauses, loop ends when the first is exhausted"
    (let* ((letters '(:a :b :c :d))
           (loop-result
             (loop for letter in letters
                   for i from 0 to 1000 by 5
                   collect (list i letter))))
      (assert-equal loop-result ____ )))


(define-test test-counting-backwards
    (let ((loop-result
             (loop for i from 10 downto -10 by 5
                   collect i )))
      (assert-equal loop-result ____ )))


(define-test test-loop-in-vs-loop-on
    (let* ((letters '(:a :b :c))
           (loop-result-in
            (loop for letter in letters collect letter))
           (loop-result-on
            (loop for letter on letters collect letter)))
      (assert-equal loop-result-in ____)
      (assert-equal loop-result-on ____ )))


(define-test test-loop-in-skip-by
    (let* ((letters '(:a :b :c :d :e :f))
           (loop-result-in
            (loop for letter in letters collect letter))
           (loop-result-in-cdr
            (loop for letter in letters by 'cdr collect letter))
           (loop-result-in-cddr
            (loop for letter in letters by 'cddr collect letter))
           (loop-result-in-cdddr
            (loop for letter in letters by 'cdddr collect letter)))
      (assert-equal loop-result-in ____)
      (assert-equal loop-result-in-cdr ____)
      (assert-equal loop-result-in-cddr ____)
      (assert-equal loop-result-in-cdddr ____)))


(define-test test-loop-across-vector
    (let* ((my-vector (make-array '(5) :initial-contents '(0 1 2 3 4)))
           (loop-result
            (loop for val across my-vector collect val)))
      (assert-equal ____ loop-result)))


(define-test test-loop-across-2d-array
    (let* ((my-array (make-array '(3 3) :initial-contents '((0 1 2) (3 4 5) (6 7 8))))
           (loop-result
            (loop for i from 0 below (array-total-size my-array) collect (row-major-aref my-array i))))
      (assert-equal loop-result ____ )))


(define-test test-loop-across-2d-array-respecting-shape
    (let* ((my-array (make-array '(3 2) :initial-contents '((0 1) (2 3) (4 5))))
           (loop-result
            (loop for i from 0 below (array-dimension my-array 0) collect
              (loop for j from 0 below (array-dimension my-array 1) collect
                (expt (aref my-array i j) 2)))))
      (assert-equal loop-result ____ )))


(defvar books-to-heros)
(setf books-to-heros (make-hash-table :test 'equal))
(setf (gethash "The Hobbit" books-to-heros) "Bilbo")
(setf (gethash "Where The Wild Things Are" books-to-heros) "Max")
(setf (gethash "The Wizard Of Oz" books-to-heros) "Dorothy")
(setf (gethash "The Great Gatsby" books-to-heros) "James Gatz")


(define-test test-loop-over-hash-tables
    (let* ((pairs-in-table
            (loop for k being the hash-keys in books-to-heros
                  using (hash-value v)
                  collect (list k v))))
      (assert-equal ____ (length pairs-in-table))
      (true-or-false? ____ (find '("The Hobbit" "Bilbo") pairs-in-table :test 'equal))))


(define-test test-value-accumulation-forms
    (let ((loop-1
           (loop for x in '(1 2 4 8 16)
                 collect x into collected
                   count x into counted
                 sum x into summed
                 maximize x into maximized
                 minimize x into minimized
                 finally (return (list collected counted summed maximized minimized)))))
      (destructuring-bind (col count sum max min) loop-1
        (assert-equal col ____)
        (assert-equal count ____)
        (assert-equal sum ____)
        (assert-equal max ____)
        (assert-equal min ____))))


(define-test test-destructuring-bind
    (let* ((count 0)
           (result (loop for (a b) in '((1 9) (2 8) (3 7) (4 6))
                        do (setf count (+ 1 count))
                         collect (+ a b))))
      (assert-equal ____ count)
      (assert-equal ____ result)))


(define-test test-conditional-execution
    (let ((loop-return
           (loop for x in '(1 1 2 3 5 8 13)
                 when (evenp x) sum x)))
      (assert-equal loop-return ____)))


(defun greater-than-10-p (x)
  (> x 10))

(define-test test-conditional-with-defun
    (let ((loop-return
           (loop for x in '(1 1 2 3 5 8 13)
                 when (greater-than-10-p x) sum x)))
      (assert-equal loop-return ____)))


(define-test test-conditional-with-lambda
    (let ((loop-return
           (loop for x in '(1 1 2 3 5 8 13)
                 when ((lambda (z) (equal 1 (mod z 3))) x) sum x)))
      (assert-equal loop-return ____)));;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


;; A lisp macro is like a function which takes an input lisp form
;; and produces a new output lisp form.  Calling the macro
;; first produces new form, and then evaluates it in the context
;; of the macro call.  The first phase, the creation of the new
;; macro form, is called 'macro expansion'.



(defmacro repeat-2 (f) (list 'progn f f))

(define-test test-macro-expands
    "assert-expands checks the expanded macro form against expectation."
  (assert-expands
   '(progn (do-something arg1 arg2) (do-something arg1 arg2))
   (repeat-2 (do-something arg1 arg2)))

  (assert-expands
   ____
   (repeat-2 (setf x (+ 1 x)))))


;; ----


(define-test test-backtick-form
    "backtick (`) form is much like single-quote (') form, except that subforms
     preceded by a comma (,) are evaluated, rather than left as literals"
  (let ((num 5)
        (word 'dolphin))
    (true-or-false? ___  (equal '(1 3 5) `(1 3 5)))
    (true-or-false? ___  (equal '(1 3 5) `(1 3 num)))
    (assert-equal ____ `(1 3 ,num))
    (assert-equal ____ `(word ,word ,word word))))


(define-test test-at-form
    "The at form, (@) in the backtick context splices a list variables into
     the form."
    (let ((axis '(x y z)))
      (assert-equal '(x y z) axis)
      (assert-equal '(the axis are (x y z)) `(the axis are ,axis))
      (assert-equal '(the axis are x y z) `(the axis are ,@axis)))
    (let ((coordinates '((43.15 77.6) (42.36 71.06))))
      (assert-equal ____
        `(the coordinates are ,coordinates))
      (assert-equal ____
        `(the coordinates are ,@coordinates))))


;; ---- On Gensym: based on ideas from common lisp cookbook

;; sets sym1 and sym2 to val
(defmacro double-setf-BAD (sym1 sym2 val)
  `(progn (setf ,sym1 ,val) (setf ,sym2 ,val)))

(define-test test-no-gensym
    "macro expansions may introduce difficult to see
     interactions"
  (let ((x 0)
        (y 0))
    (double-setf-BAD x y 10)
    (assert-equal x 10)
    (assert-equal y 10))

  (let ((x 0)
        (y 0))
    (double-setf-BAD x y (+ x 100))
    (assert-equal x ____)
    (assert-equal y ____)))

;; sets sym1 and sym2 to val
(defmacro double-setf-SAFER (sym1 sym2 val)
  (let ((new-fresh-symbol (gensym)))
    `(let ((,new-fresh-symbol ,val))
       (progn (setf ,sym1 ,new-fresh-symbol) (setf ,sym2 ,new-fresh-symbol)))))

(define-test test-with-gensym
    "gensym creates a new symbol."
  (let ((x 0)
        (y 0))
    (double-setf-SAFER x y 10)
    (assert-equal x 10)
    (assert-equal y 10))

  (let ((x 0)
        (y 0))
    (double-setf-SAFER x y (+ x 100))
    (assert-equal x ____)
    (assert-equal y ____)))


;; ----

(defvar *log* nil)

(defmacro log-form (form)
  "records the body form to the list *log* and then evalues the body normally"
  `(let ((retval ,form))
     (push ',form *log*)
     retval))

(define-test test-basic-log-form
  "illustrates how the basic log-form macro above works"
  (assert-equal 1978 (* 2 23 43))
  (assert-equal nil *log*)
  "log-form does not interfere with the usual return value"
  (assert-equal 1978 (log-form (* 2 23 43)))
  "log-form records the code which it has been passed"
  (assert-equal ___ (length *log*))
  (assert-equal ___ (first *log*))
  "macros evaluating to more macros is ok, if confusing"
  (assert-equal 35 (log-form (log-form (- 2013 1978))))
  (assert-equal 3 (length *log*))
  (assert-equal '(log-form (- 2013 1978)) (first *log*))
  (assert-equal '(- 2013 1978) (second *log*)))

;; Now you must write a more advanced log-form, that also records the value
;; returned by the form

(defvar *log-with-value* nil)

;; you must write this macro
(defmacro log-form-with-value (form)
  "records the body form, and the form's return value
   to the list *log-with-value* and then evalues the body normally"
  `(let ((logform nil)
         (retval ,form))

     ;; YOUR MACRO COMPLETION CODE GOES HERE.

     retval))



(define-test test-log-form-and-value
    "log should start out empty"
  (assert-equal nil *log-with-value*)
  "log-form-with-value does not interfere with the usual return value"
  (assert-equal 1978 (log-form-with-value (* 2 23 43)))
  "log-form records the code which it has been passed"
  (assert-equal 1 (length *log-with-value*))
  (assert-equal '(:form (* 2 23 43) :value 1978) (first *log-with-value*))
  "macros evaluating to more macros is ok, if confusing"
  (assert-equal 35 (log-form-with-value (log-form-with-value (- 2013 1978))))
  (assert-equal 3 (length *log-with-value*))
  (assert-equal '(:form (log-form-with-value (- 2013 1978)) :value 35) (first *log-with-value*))
  (assert-equal '(:form (- 2013 1978) :value 35) (second *log-with-value*)))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

(define-test test-mapcar-basics
    "We can apply a function to each member
     of a list using mapcar."
  (defun times-two (x) (* x 2))
  (assert-equal ____ (mapcar 'times-two '(1 2 3)))
  (assert-equal ____ (mapcar 'first '((3 2 1)
                                      ("little" "small" "tiny")
                                      ("pigs" "hogs" "swine")))))


(define-test test-mapcar-multiple-lists
    "The mapcar function can be applied to
     more than one list. It applies a function
     to successive elements of the lists."
  (assert-equal ____ (mapcar '* '(1 2 3) '(4 5 6)))
  (assert-equal ____ (mapcar 'list '("lisp" "are") '("koans" "fun"))))


(define-test test-transpose-using-mapcar
    "Replace the usage of WRONG-FUNCTION in 'transpose' with the
     correct lisp function (don't forget the #')."
  (defun WRONG-FUNCTION-1 (&rest rest) '())
  (defun transpose (L) (apply 'mapcar (cons 'WRONG-FUNCTION-1 L)))
  (assert-equal '((1 4 7)
                  (2 5 8)
                  (3 6 9))
                (transpose '((1 2 3)
                             (4 5 6)
                             (7 8 9))))
  (assert-equal '(("these" "pretzels" "are")
                  ("making" "me" "thirsty"))
                (transpose '(("these" "making")
                             ("pretzels" "me")
                             ("are" "thirsty")))))


(define-test test-reduce-basics
    "The reduce function combines the elements
     of a list, from left to right, by applying
     a binary function to the list elements."
  (assert-equal ___  (reduce '+ '(1 2 3 4)))
  (assert-equal ___ (reduce 'expt '(2 3 2))))


(define-test test-reduce-right-to-left
    "The keyword :from-end allows us to apply
     reduce from right to left."
  (assert-equal ___ (reduce '+ '(1 2 3 4) :from-end t))
  (assert-equal ___ (reduce 'expt '(2 3 2) :from-end t)))


(define-test test-reduce-with-initial-value
    "We can supply an initial value to reduce."
  (assert-equal ___ (reduce 'expt '(10 21 34 43) :initial-value 1))
  (assert-equal ___ (reduce 'expt '(10 21 34 43) :initial-value 0)))


(defun WRONG-FUNCTION-2 (a b) (a))
(defun WRONG-FUNCTION-3 (a b) (a))

(define-test test-mapcar-and-reduce
    "mapcar and reduce are a powerful combination.
     insert the correct function names, instead of WRONG-FUNCTION-X
     to define an inner product."
  (defun inner (x y)
    (reduce 'WRONG-FUNCTION-2 (mapcar 'WRONG-FUNCTION-3 x y)))
  (assert-equal 32 (inner '(1 2 3) '(4 5 6)))
  (assert-equal 310 (inner '(10 20 30) '(4 3 7))))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.



"In lisp, it is possible for a function to return more than one value.
This is distinct from returning a list or structure of values."

(define-test test-floor-returns-multiple-values
    (let ((x)
          (y))
      (setf x (floor 1.5))
      (assert-equal x 1)
      (setf x (multiple-value-list (floor 3.2)))
      (assert-equal x '(1 1.2)))
  (assert-equal (multiple-value-list (floor 99.4)) ____))

(defun next-fib (a b)
  (values b (+ a b)))

(define-test test-multi-value-bind
    (let ((x)
          (y))
      (setf x (next-fib 2 3))
      (assert-equal x ___)
      (setf x (multiple-value-list (next-fib 2 3)))
      (assert-equal x ___)
      "multiple-value-bind binds the variables in the first form
       to the outputs of the second form.  And then returns the output
       of the third form using those bindings"
      (setf y (multiple-value-bind (b c) (next-fib 3 5) (* b c)))
      (assert-equal y ___)
      "multiple-value-setq is like setf, but can set multiple variables"
      (multiple-value-setq (x y) (values :v1 :v2))
      (assert-equal (list x y) '(:v1 :v2))
      (multiple-value-setq (x y) (next-fib 5 8))
      (assert-equal (list x y) ____)))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

(define-test test-t-and-nil-are-opposites
    "not is a function which returns the boolean opposite of its argument"
   (true-or-false? ___ (not nil))
   (true-or-false? ___ (not t)))


(define-test test-nil-and-empty-list-are-the-same-thing
  (true-or-false? ___ ())
  (true-or-false? ___ (not ())))


(define-test test-lots-of-things-are-true
   " every value, other than nil, is boolean true"
   (true-or-false? ___ 5)
   (true-or-false? ___ (not 5))
   (true-or-false? ___ "A String")
   "only nil is nil.  Everything else is effectively true."
   "the empty string"
   (true-or-false? ___ "")
   "a list containing a nil"
   (true-or-false? ___ '(nil))
   "an array with no elements"
   (true-or-false? ___ (make-array '(0)))
   "the number zero"
   (true-or-false? ___ 0))


(define-test test-and
   "and can take multiple arguments"
   (true-or-false? ___ (and t t t t t))
   (true-or-false? ___ (and t t nil t t))
   "if no nils, and returns the last value"
   (assert-equal ___ (and t t t t t 5)))


(define-test test-or
   "or can also take multiple arguments"
   (true-or-false? ____  (or nil nil nil t nil))
   "or returns the first non nil value, or nil if there are none."
   (assert-equal ____ (or nil nil nil))
   (assert-equal ____ (or 1 2 3 4 5)));;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


(defun shadow-z (z)
;; reuses the symbol name z to build a return value
;; returns a list like (value-of-z, 2)
  (cons z
        (cons (let ((z 2)) z)
              nil)))

(define-test test-shadowing-a-variable
  (assert-equal ___ (shadow-z 1)))


(defun code-block-01 ()
;; illustrates a basic property of code-blocks
  (block here
    (return-from here 4)
    5))

(defun code-block-02 ()
  (block outer
    (block inner
      (return-from outer 'space)
      (return-from inner 'tube))
    (return-from outer 'valve)))

(define-test test-code-block-01
  (assert-equal ___ (code-block-01)))

(define-test test-code-block-02
  (assert-equal ___ (code-block-02)))


;; About closures and the distinction of lexical and dynamic bindings

;; this recipe from stackoverflow
;; http://stackoverflow.com/questions/463463/dynamic-and-lexical-variables-in-common-lisp
; (print "no special x: a typical closure.")

;; bind f to a function which depends on a local variable x
;; then invoke f to see which value of x is returned.

(define-test test-lexical-bindings-may-be-shadowed
  (assert-eq ___ (let ((f (let ((x 10))
                 (lambda () x))))  ; <-- x bound lexically
    (let ((x 20))          ; form 2
      (funcall f)))))


(define-test test-special-bindings-look-back-on-execution-path
  (assert-eq ___ (let ((f (let ((x 10))
             (declare (special x))
             (lambda () x))))      ; <-- x bound dynamically
    (let ((x 20))          ; form 2
      (declare (special x))
    (funcall f)))))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


;;;;;;;;;;;;;;
;; GREED !! ;;
;;;;;;;;;;;;;;


;; Modified from Ruby Koans: about_scoring_project.rb

; *Greed* is a dice game where you roll up to five dice to accumulate
; points.  The following "score" function will be used to calculate the
; score of a single roll of the dice.
;
; A greed roll is scored as follows:
;
; * A set of three ones is 1000 points
;
; * A set of three numbers (other than ones) is worth 100 times the
;   number. (e.g. three fives is 500 points).
;
; * A one (that is not part of a set of three) is worth 100 points.
;
; * A five (that is not part of a set of three) is worth 50 points.
;
; * Everything else is worth 0 points.
;
;
; Examples:
;
; (score '(1 1 1 5 1)) => 1150 points
; (score '(2 3 4 6 2)) => 0 points
; (score '(3 4 5 3 3)) => 350 points
; (score '(1 5 1 2 4)) => 250 points
;
; More scoring examples are given in the tests below:
;
; Your goal is to write the score method.

(defun score (dice)
  ; You need to write this method
)

(define-test test-score-of-an-empty-list-is-zero
    (assert-equal 0 (score nil)))

(define-test test-score-of-a-single-roll-of-5-is-50
    (assert-equal 50 (score '(5))))


(define-test test-score-of-a-single-roll-of-1-is-100
    (assert-equal 100 (score '(1))))

(define-test test-score-of-multiple-1s-and-5s-is-the-sum-of-individual-scores
    (assert-equal 300 (score '(1 5 5 1))))

(define-test test-score-of-single-2s-3s-4s-and-6s-are-zero
    (assert-equal 0 (score '(2 3 4 6))))


(define-test test-score-of-a-triple-1-is-1000
    (assert-equal 1000  (score '(1 1 1))))

(define-test test-score-of-other-triples-is-100x
    (assert-equal 200  (score '(2 2 2)))
    (assert-equal 300  (score '(3 3 3)))
    (assert-equal 400  (score '(4 4 4)))
    (assert-equal 500  (score '(5 5 5)))
    (assert-equal 600  (score '(6 6 6))))

(define-test test-score-of-mixed-is-sum
    (assert-equal 250  (score '(2 5 2 2 3)))
    (assert-equal 550  (score '(5 5 5 5))))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


; Special forms are evaluatable lisp forms (lists) which are
; neither functions nor macros.  Here is an introduction to a
; few of them.

; based on http://psg.com/~dlamkins/sl/chapter03-03.html

(defvar my-name)
(defvar my-clones-name)
(defvar a)
(defvar b)
(defvar c 0)

(define-test test-setf
    "setf is used to assign values to symbols.  These symbols may refer to
     variables with lexical or dynamic scope."
  (setf my-name "David")
  (assert-equal my-name ____)
  " In SBCL, if the symbol isn't defined as a variable, via a top-level defvar
  or let statement, the setf call may result in a warning."
  (setf my-clones-name my-name)
  (assert-equal "David" ____)
  (setf a 5)
  (setf b 10)
  (setf c ___)
  (assert-equal 50 c))


(define-test test-let
    "The let form establishes a lexical extent, within which explicit symbols
     may be bound to values.  The binding only extends over the extent of the
     lexical form.  After which, the previous value, if it exists, is visible again."
  (setf a 10)
  (setf b 20)
  (assert-equal a ___)
  (assert-equal b ___)
  (let ((a 1111)
        (b 2222))
    (assert-equal a ___)
    (assert-equal b ___))
  (assert-equal a ___)
  (assert-equal b ___))


(define-test test-let-default-value
    "let vars have a default value"
    (let ((x))
      (assert-equal ___ x)))

(define-test test-let-bindings-are-parallel
    "When defining the bindings in the let form, later bindings may not depend
     on earlier ones"
  (setf a 100)
  (let ((a 5)
        (b (* 10 a)))
    (assert-equal b ___)))

(define-test test-let*-bindings-are-series
    "let* is like let, but successive bindings may use values of previous ones"
  (setf a 100)
  (let* ((a 5)
         (b (* 10 a)))
    (assert-equal b ___))
  (assert-equal a ___))


(define-test write-your-own-let-statement
    "fix the let statement to get the tests to pass"
  (setf a 100)
  (setf b 23)
  (setf c 456)
  (let ((a __)
        (b __)
        (c __))
    (assert-equal a 100)
    (assert-equal b 200)
    (assert-equal c "Jellyfish"))
  (let* ((a __)
         ;; add more here
         )
    (assert-equal a 121)
    (assert-equal b 200)
    (assert-equal c (+ a (/ b a)))))

(define-test test-case
    "the case form is like the C switch statement: it
    compares an input with a set of values and evaluates an
    expression once a match is found"
  (setf a 4)
  (setf b
        (case a (4 :four)
                (5 :five)
                ;; t specifies default behavior
                (t :unknown)))
  (assert-equal ____ b)
  "case can also check if a list of values contains
   the input"
  (setf c
        (case a (5 :five)
                ((3 4) :three-or-four)))
  (assert-equal ____ c))

(defun cartoon-dads (input)
    "you should be able to complete this case statement"
  (case input (:this-one-doesnt-happen :fancy-cat)
              (t :unknown)))

(define-test test-your-own-case-statement
    "fix this by completing the 'cartoon-dads' function above"
  (assert-equal (cartoon-dads :bart) :homer)
  (assert-equal (cartoon-dads :stewie) :peter)
  (assert-equal (cartoon-dads :stan) :randy)
  (assert-equal (cartoon-dads :space-ghost) :unknown))

(define-test test-limits-of-case
    "case is not suitable for all kinds of values, because
     it uses the function eql for comparisons. We will explore
     the implications of this in the equality-distinctions lesson"
  (let* ((name "John")
         (lastname (case name ("John" "Doe")
                              ("Max" "Mustermann")
                              (t "Anonymous"))))
  (assert-equal ____ lastname)))

(define-test test-cond
    "cond is the general purpose form for checking multiple
     conditions, until a condition is met"
  (setf a 4)
  (setf c
        (cond ((> a 0) :positive)
              ((< a 0) :negative)
              (t :zero)))
  (assert-equal ____ c))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

(define-test test-double-quoted-strings-are-strings
    (let ((my-string "do or do not"))
      (true-or-false? ___ (typep my-string 'string))
      "strings are the same thing as vectors of characters"
      (true-or-false? ___ (typep my-string 'array))
      (assert-equal (aref "meat" 2) (aref "fiesta" 5))
      "strings are not integers :p"
      (true-or-false? ___ (typep my-string 'integer))))


(define-test test-multi-line-strings-are-strings
    (let ((my-string "this is
                      a multi
                      line string"))
      (true-or-false? ___ (typep my-string 'string))))


(define-test test-escape-quotes
    (let ((my-string "this string has one of these in it"))
      (true-or-false? ___ (typep my-string 'string))))


; This test from common lisp cookbook
(define-test test-substrings
    "since strings are sequences, you may use subseq"
  (let ((my-string "Groucho Marx"))
    (assert-equal "Marx" (subseq my-string 8))
    (assert-equal (subseq my-string 0 7) ____)
    (assert-equal (subseq my-string 1 5) ____)))

(define-test test-accessing-individual-characters
  "char literals look like this"
  (true-or-false? ___ (typep 'character))
  (true-or-false? ___ (typep "A" 'character))
  (true-or-false? ___ (typep 'string))
  "char is used to access individual characters"
  (let ((my-string "Cookie Monster"))
    (assert-equal (char my-string 0) )
    (assert-equal (char my-string 3) )
    (assert-equal (char my-string 7) ___)))


(define-test test-concatenating-strings
    "concatenating strings in lisp is a little cumbersome"
  (let ((a "this")
        (b "is")
        (c "unwieldy"))
    (assert-equal ___ (concatenate 'string a " " b " " c))))


(define-test test-searching-for-characters
    "you can use position to detect characters in strings
     (or elements of sequences)"
  (assert-equal ___ (position "abc"))
  (assert-equal ___ (position "abc"))
  (assert-equal ___ (find "abc")))


(define-test test-finding-substrings
    "search finds subsequences"
  (let ((title "A supposedly fun thing I'll never do again"))
    (assert-equal 2 (search "supposedly" title))
    (assert-equal 12 (search "CHANGETHISWORD" title))))

;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


;; Lisp structures encapsulate data which belongs together.  They are
;; a template of sorts, providing a way to generate multiple instances of
;; uniformly organized information
;;
;; Defining a struct also interns accessor functions to get and set the fields
;; of the structure.


;; Define a new struct with the defstruct form.  The following call creates a
;; new structure type named basketball-player, with slots named:
;; 'name', 'team', and number.
(defstruct basketball-player name team number)

(define-test test-make-struct
    ;; Create a basketball structure instance, and then read out the values.
  (let ((player-1 (make-basketball-player
                   :name "larry" :team :celtics :number 33)))
    (assert-equal "larry" (basketball-player-name player-1))
    (assert-equal ___ (basketball-player-team player-1))
    (assert-equal ___ (basketball-player-number player-1))
    (assert-equal 'basketball-player (type-of player-1))
    (setf (basketball-player-team player-1) :RETIRED)
    (assert-equal ___ (basketball-player-team player-1))))


;; Struct fields can have default values
;; fields without explicit defaults default to nil.

(defstruct baseball-player name (position :outfield) (team :red-sox))

(define-test test-struct-defaults
    (let ((player-2 (make-baseball-player)))
      (assert-equal ___ (baseball-player-position player-2))
      (assert-equal ___ (baseball-player-team player-2))
      (assert-equal ___ (baseball-player-name player-2))))


;; The accessor names can get pretty long.  It's possible to specify
;; a nickname to make code readable with the :conc-name option.

(defstruct (american-football-player (:conc-name nfl-guy-)) name position team)

(define-test test-abbreviated-struct-access
    (let ((player-3 (make-american-football-player
                     :name "Drew Brees" :position :QB :team "Saints")))
      (assert-equal ___ (nfl-guy-position player-3))))


;; Structs can be defined as EXTENSIONS to previous structures.
;; This form of inheritance allows composition of objects.

(defstruct (nba-contract (:include basketball-player)) salary start-year end-year)

(define-test test-structure-extension
    (let ((contract-1 (make-nba-contract
                       :salary 136000000
                       :start-year 2004
                       :end-year 2011
                       :name "Kobe Bryant"
                       :team :LAKERS
                       :number 24)))
      (assert-equal ___ (nba-contract-start-year contract-1))
      (assert-equal ___ (type-of contract-1))
      ;; do inherited structures follow the rules of type hierarchy?
      (true-or-false? ___ (typep contract-1 'BASKETBALL-PLAYER))
      ;; can you access structure fields with the inherited accessors?
      (assert-equal ___ (nba-contract-team contract-1))
      (assert-equal ___ (basketball-player-team contract-1))))


;; Copying of structs is handled with the copy-{name} form.  Note that
;; copying is shallow.

(define-test test-structure-copying
    (let ((manning-1 (make-american-football-player :name "Manning" :team '("Colts" "Broncos")))
          (manning-2 (make-american-football-player :name "Manning" :team '("Colts" "Broncos"))))
      ;; manning-1 and manning-2 are different objects
      (true-or-false? ___ (eq manning-1 manning-2))
      ;; but manning-1 and manning-2 contain the same information
      ;; (note the equalp instead of eq
      (true-or-false? ___ (equalp manning-1 manning-2))
      ;; copied structs are much the same.
      (true-or-false? ___ (equalp manning-1 (copy-american-football-player manning-1)))
      (true-or-false? ___ (eq     manning-1 (copy-american-football-player manning-1)))
      ;; note that the copying is shallow
      (let ((shallow-copy (copy-american-football-player manning-1)))
        (setf (car (nfl-guy-team manning-1)) "Giants")
        (assert-equal ___ (car (nfl-guy-team manning-1)))
        (assert-equal ___ (car (nfl-guy-team shallow-copy))))))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;; NOTE: This koan group uses language features specific to sbcl, that are
;; not part of the Common Lisp specification.  If you are not using sbcl,
;; feel free to skip this group by removing it from '.koans'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making threads with sb-thread:make-thread  ;;
;; Joining threads with sb-thread:join-thread ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sb-thread takes a -function- as a parameter.
;; This function will be executed in a separate thread.

;; Since the execution order of separate threads is not guaranteed,
;; we must -join- the threads in order to make our assertions.

(defvar *greeting* "no greeting")

(defun sets-socal-greeting ()
  (setf *greeting* "Sup, dudes"))

(define-test test-hello-world-thread
    "Create a thread which returns 'hello world', then ends.
    using a lambda as the supplied function to execute."
  (assert-equal *greeting* "no greeting")
  (let ((greeting-thread
         (sb-thread:make-thread
          (lambda ()
            (setf *greeting* "hello world")))))
    (sb-thread:join-thread greeting-thread)
    (assert-equal *greeting* "hello world")
    (setf greeting-thread (sb-thread:make-thread 'sets-socal-greeting))
    (sb-thread:join-thread greeting-thread)
    (assert-equal *greeting* ____)))


(define-test test-join-thread-return-value
    "the return value of the thread is passed in sb-thread:join-thread"
  (let ((my-thread (sb-thread:make-thread
                    (lambda () (* 11 99)))))
    (assert-equal ____ (sb-thread:join-thread my-thread))))


(define-test test-threads-can-have-names
    "Threads can have names.  Names can be useful in diagnosing problems
     or reporting."
  (let ((empty-plus-thread
         (sb-thread:make-thread '+
                                :name "what is the sum of no things adding?")))
    (assert-equal (sb-thread:thread-name empty-plus-thread)
                  ____)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending arguments to the thread function: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun returns-hello-name (name)
  (format nil "Hello, ~a" name))

(defun double-wrap-list (x y z)
  (list (list x y z)))

;; Create a thread which will return "Hello -Name-" using
;; the named returns-hello-name function.   Arguments are handed
;; to threads as a list, unless there is just a single argument
;; then it does not need to be wrapped in a list.

(define-test test-sending-arguments-to-thread
    (assert-equal "Hello, Buster"
                  (sb-thread:join-thread
                   (sb-thread:make-thread 'returns-hello-name
                                          :arguments "Buster")))
    (assert-equal ____
                  (sb-thread:join-thread
                   (sb-thread:make-thread 'double-wrap-list
                                          :arguments '(3 4 5)))))


;; ----

(defvar *accum* 0)

(defun accum-after-time (time arg1)
    "sleeps for time seconds and then adds arg1 to *accum*"
  (sleep time)
  (incf *accum* arg1))

(defvar *before-time-millisec* 0)
(defvar *after-time-millisec* 0)

;; cheap and dirty time measuring function
(defun duration-ms ()
  (- *after-time-millisec* *before-time-millisec*))

(define-test test-run-in-series
    "get internal real time returns a time stamp in milliseconds"
  (setf *accum* 0)
  (setf *before-time-millisec* (get-internal-real-time))
  (accum-after-time 0.3 1)
  (accum-after-time 0.2 2)
  (accum-after-time 0.1 4)
  (setf *after-time-millisec* (get-internal-real-time))
  (true-or-false? ___ (> (duration-ms) 500))
  (true-or-false? ___ (< (duration-ms) 700))
  (assert-equal *accum* ___))

(define-test test-run-in-parallel
    "same program as above, executed in threads.  Sleeps are simultaneous"
  (setf *accum* 0)
  (setf *before-time-millisec* (get-internal-real-time))
  (let ((thread-1 (sb-thread:make-thread 'accum-after-time :arguments '(0.3 1)))
        (thread-2 (sb-thread:make-thread 'accum-after-time :arguments '(0.2 2)))
        (thread-3 (sb-thread:make-thread 'accum-after-time :arguments '(0.1 4))))
    (sb-thread:join-thread thread-1)
    (sb-thread:join-thread thread-2)
    (sb-thread:join-thread thread-3))
  (setf *after-time-millisec* (get-internal-real-time))
  (true-or-false? ___ (> (duration-ms) 200))
  (true-or-false? ___  (< (duration-ms) 400))
  (assert-equal *accum* ___))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; killing renegade threads            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun spawn-looping-thread (name)
  "create a never-ending looping thread with a given name"
  (sb-thread:make-thread (lambda () (loop)) :name name))

(defvar *top-thread* sb-thread:*current-thread*)
(defun main-thread-p (thread) (eq thread *top-thread*))

(defun kill-thread-if-not-main (thread)
" kills a given thread, unless the thread is the main thread.
 returns nil if thread is main.
 returns a 'terminated~' string otherwise"
  (unless (main-thread-p thread)
    (sb-thread:terminate-thread thread)
    (concatenate 'string "terminated " (sb-thread:thread-name thread))))

(defun kill-spawned-threads ()
  "kill all lisp threads except the main thread."
  (map 'list 'kill-thread-if-not-main (sb-thread:list-all-threads)))

(defun spawn-three-loopers ()
  "Spawn three run-aways."
  (progn
    (spawn-looping-thread "looper one")
    (spawn-looping-thread "looper two")
    (spawn-looping-thread "looper three")))

(define-test test-counting-and-killing-threads
    "list-all-threads makes a list of all running threads in this lisp.  The sleep
     calls are necessary, as killed threads are not instantly removed from the
     list of all running threads."
  (assert-equal ___ (length (sb-thread:list-all-threads)))
  (kill-thread-if-not-main (spawn-looping-thread "NEVER CATCH ME~!  NYA NYA!"))
  (sleep 0.01)
  (assert-equal ___ (length (sb-thread:list-all-threads)))
  (spawn-three-loopers)
  (assert-equal ___ (length (sb-thread:list-all-threads)))
  (kill-spawned-threads)
  (sleep 0.01)
  (assert-equal ___ (length (sb-thread:list-all-threads))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bindings are not inherited across threads ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *v* 0)

(defun returns-v ()
  *v*)

(define-test test-threads-dont-get-bindings
    "bindings are not inherited across threads"
  (let ((thread-ret-val (sb-thread:join-thread
                         (sb-thread:make-thread 'returns-v))))
    (assert-equal thread-ret-val ____))
  (let ((*v* "LEXICAL BOUND VALUE"))
    (assert-equal *v* ____)
    (let ((thread-ret-val (sb-thread:join-thread
                           (sb-thread:make-thread 'returns-v))))
      (assert-equal thread-ret-val ____))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global state (special vars) are ;;
;; shared across threads           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *g* 0)

(defun waits-and-increments-g (&optional (n 0.2))
  "sets *g* to 1 + the value of *g* n seconds ago"
  (let ((my-remembered-g *g*))
    (sleep n)
    (setq *g* (+ 1 my-remembered-g))))

(define-test test-serial-wait-and-increment
 "incrementing *g* three times and expecting
  the final value to be three works."
  (setf *g* 0)
  (waits-and-increments-g)
  (waits-and-increments-g)
  (waits-and-increments-g)
  (assert-equal *g* ___))


(define-test test-parallel-wait-and-increment
    (setf *g* 0)
  (let ((thread-1 (sb-thread:make-thread 'waits-and-increments-g))
        (thread-2 (sb-thread:make-thread 'waits-and-increments-g))
        (thread-3 (sb-thread:make-thread 'waits-and-increments-g)))
    (sb-thread:join-thread thread-1)
    (sb-thread:join-thread thread-2)
    (sb-thread:join-thread thread-3)
    (assert-equal *g* ___)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state can be protected ;;
;; with a mutex.                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *g* 0)
(defvar *gs-mutex* (sb-thread:make-mutex :name "g's lock"))

(defun protected-increments-g (&optional (n 0.1))
  "Surround all references to *g* within the with-mutex form."
  (sb-thread:with-mutex (*gs-mutex*)
    (let ((my-remembered-g *g*))
      (sleep n)
      (setq *g* (+ 1 my-remembered-g)))))

(define-test test-parallel-wait-and-increment-with-mutex
    (setf *g* 0)
  (let ((thread-1 (sb-thread:make-thread 'protected-increments-g))
        (thread-2 (sb-thread:make-thread 'protected-increments-g))
        (thread-3 (sb-thread:make-thread 'protected-increments-g)))
    (sb-thread:join-thread thread-1)
    (sb-thread:join-thread thread-2)
    (sb-thread:join-thread thread-3)
    (assert-equal *g* ___)))

;;;;;;;;;;;;;;;;
;; Semaphores ;;
;;;;;;;;;;;;;;;;

;; Incrementing a semaphore is an atomic operation.
(defvar *g-semaphore* (sb-thread:make-semaphore :name "g" :count 0))

(defun semaphore-increments-g ()
  (sb-thread:signal-semaphore *g-semaphore*))

(define-test test-increment-semaphore
    (assert-equal 0 (sb-thread:semaphore-count *g-semaphore*))
  (sb-thread:join-thread (sb-thread:make-thread 'semaphore-increments-g :name "S incrementor 1"))
  (sb-thread:join-thread (sb-thread:make-thread 'semaphore-increments-g :name "S incrementor 2"))
  (sb-thread:join-thread (sb-thread:make-thread 'semaphore-increments-g :name "S incrementor 3"))
  (assert-equal ___ (sb-thread:semaphore-count *g-semaphore*)))


;; Semaphores can be used to manage resource allocation, and to trigger
;; threads to run when the semaphore value is above zero.

(defvar *apples* (sb-thread:make-semaphore :name "how many apples" :count 0))
(defvar *orchard-log* (make-array 10))
(defvar *next-log-idx* 0)
(defvar *orchard-log-mutex* (sb-thread:make-mutex :name "orchard log mutex"))

(defun add-to-log (item)
  (sb-thread:with-mutex (*orchard-log-mutex*)
    (setf (aref *orchard-log* *next-log-idx*) item)
    (incf *next-log-idx*)))

(defun apple-eater ()
  (sb-thread:wait-on-semaphore *apples*)
  (add-to-log "apple eaten."))

(defun apple-grower ()
  (sleep 0.1)
  (add-to-log "apple grown.")
  (sb-thread:signal-semaphore *apples*))

(defun num-apples ()
  (sb-thread:semaphore-count *apples*))

(define-test test-orchard-simulation
    (assert-equal (num-apples) ___)
  (let ((eater-thread (sb-thread:make-thread 'apple-eater :name "apple eater thread")))
    (let ((grower-thread (sb-thread:make-thread 'apple-grower :name "apple grower thread")))
      (sb-thread:join-thread eater-thread)))
  (assert-equal (aref *orchard-log* 0) ____)
  (assert-equal (aref *orchard-log* 1) ____))




;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


"you need to write the triangle method"

(define-condition triangle-error  (error) ())

(defun triangle (a b c)
  :write-me)


(define-test test-equilateral-triangles-have-equal-sides
    (assert-equal :equilateral (triangle 2 2 2))
    (assert-equal :equilateral (triangle 10 10 10)))


(define-test test-isosceles-triangles-have-two-equal-sides
    (assert-equal :isosceles (triangle 3 4 4))
    (assert-equal :isosceles (triangle 4 3 4))
    (assert-equal :isosceles (triangle 4 4 3))
    (assert-equal :isosceles (triangle 10 10 2)))


(define-test test-scalene-triangles-have-no-equal-sides
    (assert-equal :scalene (triangle 3 4 5))
    (assert-equal :scalene (triangle 10 11 12))
    (assert-equal :scalene (triangle 5 4 2)))


(define-test test-illegal-triangles-throw-exceptions
    (assert-error 'triangle-error (triangle 0 0 0))
    (assert-error 'triangle-error (triangle 3 4 -5))
    (assert-error 'triangle-error (triangle 1 1 3))
    (assert-error 'triangle-error (triangle 2 4 2)));;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;; Common lisp types have hierarchy.  Any object may belong a family of types.
;; The top level type, which includes everything else, is 't'

(define-test test-check-some-common-types
   (true-or-false? ___  (typep "hello" 'string))
   (true-or-false? ___  (typep "hello" 'array))
   (true-or-false? ___  (typep "hello" 'list))
   (true-or-false? ___  (typep "hello" '(simple-array character (5))))

   (true-or-false? ___  (typep '(1 2 3) 'list))
   (true-or-false? ___  (typep 99 'integer))
   (true-or-false? ___  (typep nil 'NULL))
   (true-or-false? ___  (typep 22.7 'ratio))
   (true-or-false? ___  (typep 4.0 'float))
   (true-or-false? ___  (typep 'character))
   (true-or-false? ___  (typep 'length 'function)))


(define-test test-get-type-with-type-of
   (assert-equal ____ (type-of ()))
   (assert-equal ____ (type-of 4.6)))

(define-test test-type-sets-may-overlap
   (true-or-false? ___  (typep () 'list))
   (true-or-false? ___  (typep () 'atom))
   (true-or-false? ___  (typep () 'NULL))
   (true-or-false? ___  (typep () t)))


(define-test test-integers-can-get-really-big
   (true-or-false? ____ (typep 0123456789012 'integer))
   ;; Integers are either fixnum or bignum.
   ;; The boundary between fixnum and bignum is given by the constant:
   ;;   most-positive-fixnum
   (assert-true (typep 890123456789 'fixnum))
   (assert-true (typep 234567890 'bignum))
   (true-or-false? ___ (typep most-positive-fixnum 'fixnum))
   (true-or-false? ___ (typep (+ 1 most-positive-fixnum) 'fixnum)))


(define-test test-lisp-type-system-is-hierarchy
   (assert-true (typep 1 'bit))
   (assert-true (typep 1 'integer))
   (assert-true (typep 2 'integer))
   (true-or-false? ____ (subtypep 'bit 'integer))
   (true-or-false? ____ (subtypep (type-of 1) (type-of 2)))
   (true-or-false? ____ (subtypep (type-of 2) (type-of 1))))


(define-test test-some-types-are-lists
   (assert-true(typep (make-array 0 :element-type 'integer) '(SIMPLE-VECTOR 0)))
   (true-or-false? ____ (typep (make-array '(3 3) :element-type 'integer) '(SIMPLE-ARRAY T (3 3)))))


(define-test test-type-specifier-lists-also-have-hierarchy
   (true-or-false? ____ (subtypep '(SIMPLE-ARRAY T (3 3)) '(SIMPLE-ARRAY T *)))
   (true-or-false? ____ (subtypep '(vector double-float 100) '(vector * 100)))
   (true-or-false? ____ (subtypep '(vector double-float 100) '(vector double-float *)))
   (true-or-false? ____ (subtypep '(vector double-float 100) '(vector * *)))
   (true-or-false? ____ (subtypep '(vector double-float 100) '(array number *)))
   (true-or-false? ____ (subtypep '(vector double-float 100) t)))


(define-test test-type-coersion
   (assert-true (typep 0 'integer))
   (true-or-false? ___ (typep 0 'short-float))
   (true-or-false? ___ (subtypep 'integer 'short-float))
   (true-or-false? ___ (subtypep 'short-float 'integer))
   (true-or-false? ___ (typep (coerce 0 'short-float) 'short-float)))


(define-test test-atoms-are-anything-thats-not-a-cons
  (true-or-false? ___ (atom 4))
  (true-or-false? ___ (atom '(1 2 3 4)))
  (true-or-false? ___ (atom 'some-unbound-name))
  (assert-true (typep (make-array '(4 4)) '(SIMPLE-ARRAY * *)))
  (true-or-false? ___ (atom (make-array '(4 4)))))


(define-test test-functionp
    "the functionp predicate is true iff the argument is a function"
  (assert-true (functionp (lambda (a b c) (+ a b c))))
  (true-or-false? ___ (functionp 'make-array))
  (true-or-false? ___ (functionp '(1 2 3)))
  (true-or-false? ___ (functionp t)))


(define-test test-there-are-some-other-type-predicates
  ; see http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node73.html for more.
  (true-or-false? ___ (numberp 999))
  (true-or-false? ___ (listp '(9 9 9)))
  (true-or-false? ___ (integerp 999))
  (true-or-false? ___ (rationalp 9.99))
  (true-or-false? ___ (floatp 9.99))
  (true-or-false? ___ (stringp "nine nine nine"))
  (true-or-false? ___ (characterp ))
  (true-or-false? ___ (bit-vector-p #*01001)))


(define-test test-guess-that-type!
    (let ((x ____))
      (assert-true (subtypep  x '(SIMPLE-ARRAY T (* 3 *))))
      (assert-true (subtypep  x '(SIMPLE-ARRAY T (5 * *))))
      (assert-true (subtypep  x '(SIMPLE-ARRAY ARRAY *)))
      (assert-true (typep (make-array '(5 3 9) :element-type 'STRING ) x))
      (assert-true (typep (make-array '(5 3 33) :element-type 'VECTOR ) x))))
;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

(defun test-variable-assignment-with-setf ()
  ;; the let pattern allows us to create local variables with
  ;; lexical scope.
  (let (var_name_1 (var_name_2 "Michael"))
  ;; variables may be defined with or without initial values.
  (and
   (equalp var_name_2 "Michael")
   ; new values may be assigned to variables with setf
   (setf var_name_2 "Janet")
   (equalp var_name_2 "Janet")
   ; setf may assign multiple variables in one form.
   (setf var_name_1 "Tito"
         var_name_2 "Jermaine")
   (equalp var_name_1 "Tito")
   (equalp var_name_2 "Jermaine"))))

(defun test-setf-for-lists ()
  ;; setf also works on list elements
  (let (l)
    (setf l '(1 2 3))
    (equalp l '(1 2 3))
    ; First second and third are convenient accessor functions
    ; referring to the elements of a list
    ; For those interested, they are convenient to car, cadr, and caddr
    (setf (first l) 10)
    (setf (second l) 20)
    (setf (third l) 30)
    (equalp l '(10 20 30))))

(defparameter param_name_1 "Janet")
; defparameter requires an initial form.  It is a compiler error to exclude it
;(defparameter param_no_init)  ;; this will fail
(defconstant additive_identity 0)
; defconstant also requires an initial form
; (defconstant constant_no_init)

; reassigning parameters to new values is also ok, but parameters carry the
; connotation of immutability.  If it's going to change frequently, it should
; be a var.
(setf param_name_1 "The other one")

; reassigning a constant is an error.
; this should result in a compile time error
; (setf additive_identity -1)


;; -------------------------------
;; below is necessary to run tests.
;; -------------------------------

(defvar failed-test-names nil)

(defun run-test (testfun)
  (let ((fun-name (function-name testfun)))
    (if (apply testfun '())
        (format t ".")
        (progn
          (setf failed-test-names (cons fun-name failed-test-names))
          (format t "F")))))

(defun function-name (function) (nth-value 2 (function-lambda-expression function)))


(run-test 'test-variable-assignment-with-setf)
(run-test 'test-setf-for-lists)

(format t "~%")

(defun report-failure (test-name)
  (format t "~S failed.~%" test-name))

(if (endp failed-test-names)  ; no failed tests
    (format t "all tests pass.~%")
    (mapcar 'report-failure failed-test-names));;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

"vectors are just like rank 1 arrays"

(define-test test-vector-types
  " #(x y z) defines a vector literal containing x y z"
  (true-or-false? ___ (typep #(1 11 111) 'vector))
  (assert-equal ___ (aref #(1 11 111) 1)))


(define-test test-length-works-on-vectors
  (assert-equal (length #(1 2 3)) ___ ))


(define-test test-bit-vector
    "#*0011 defines a bit vector literal with four elements, 0, 0, 1 and 1"
  (assert-equal #*0011 (make-array '4 :element-type 'bit))
  (true-or-false? ____ (typep #*1001 'bit-vector))
  (assert-equal ____ (aref #*1001 1)))


(define-test test-some-bitwise-operations
    (assert-equal ___ (bit-and #*1100 #*1010))
    (assert-equal ___ (bit-ior #*1100 #*1010))
    (assert-equal ___ (bit-xor #*1100 #*1010)))


(defun list-to-bit-vector (my-list)
  nil)

(define-test test-list-to-bit-vector
    "you must complete list-to-bit-vector"
  (assert-true (typep (list-to-bit-vector '(0 0 1 1 0)) 'bit-vector))
  (assert-equal (aref (list-to-bit-vector '(0)) 0) 0)
  (assert-equal (aref (list-to-bit-vector '(0 1)) 1) 1)
  (assert-equal (length (list-to-bit-vector '(0 0 1 1 0 0 1 1))) 8))
