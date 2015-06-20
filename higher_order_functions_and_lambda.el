;; Everthing in LISP is a List
;; defining a variable in emacs  with setq
(setq a 5)

(setq b 5)

;;another way is

;; Note: quoting an argument in LISP just returns that argument without evaluating it
(set 'c 10)

(+ a b c)


;; Here x and y are bound to  6 and 8 resp.
;; They are local to the scope i.e not accessible outside the scope
;; Notice how if is written

;; (if then-form else-form)
(
 (let ( ( x 4 )
	( y 8 )

	)
 
 (if (> x 5)  (+ x y) (* x y)   )  

 )

;; In above examples, x,y , a, b abc c are all atoms 
;; An atom, as the name suggests is a type which cannot be split further
;; Except a list everything is an atom in ELISP

;; the function atom tests if the given argument is an atom
(atom '(2 1 3)) ;; nil since it is a list

(atom 5) ;; true


;;cond chooses among an arbitrary number of alternatives
;; each clause in the cond must be a list
;; It is similar to the switch case in other programming languages
;; The default condition is specified by t in condition clause.
;; If none of the previous conditions evaluated to true, the default condition gets evaluated
(defun printChocolate (num)

  (cond ( (= num 1) "Melody")

	(  (> num 0) "Eclairs"  )
	( t "Incorrect input. You will be given a small bite of Cadbury.")

  )

)


;; this is how you invoke a function in LISP
(printChocolate 1)

 
(defun add (x y ) (+ x y))

(defun square (x) (* x x))

(defun cube (x) (* x x x))

(setq sq 'square)

;; funcall expression computes the value of function
(defun operation  (fn x)  (funcall fn x )   )

;; sq is now a function reference to square function
(operation sq 3)
;; here we are passing  cube function as a parameter to operation
;; note when we ae quoting it, it means that e do not want the function to be evaluated but to be treated as a variable
(operation  'cube   3  )
;;similarly for squaring
(operation 'square 12)

(operation 'printChocolate 2)

;; here we are passing an anonymous function i.e lambda to the operation function
(operation (lambda (x)  (% x 2)  )   15   )

;; the above can also be done as
;; the syntax #' is a short hand for using function
(operation  ( function (lambda (x)  (% x 2) )   )  19  )


;;apply takes a function name and a list of objects, then calls the function with those objects as separate arguments
;; note: the semantics of apply in LISP are different than those in Appians SAIL language
;; the semantics of apply in LISP are very similar to the semantics of apply in JavaScript
;; so apply is like a function which "spreads" the given list into arguments of the function which it calls
(apply '+ '( 2 7 8 ) )
;;same thing...
(apply (function +)  '(2 3 4) )


(apply 'add '(3 4))

;;mapcar takes a function and a list, applies the function to each element in the list, and returns a new list of results
;; this is analogous to the Appian SAIL's apply function
(mapcar 'cube  '(3 1 4 2.2) )

;;sorting in elisp
(sort '(12 10 11 1)  '<)
;; same thing using lambda. It shud be a predicate
(sort  '(12 10 11)  (lambda (x y) (< x y)))


;;usage of partial function
(funcall  (apply-partially 'sort '(1 6 2 5 10 8))  '>   )

;;same thing using lambda
(funcall  (apply-partially 'sort '(1 6 2 5 10 8))  (lambda (x y) (> x y)  )   )

;;same thing using apply
(apply  (apply-partially 'sort '(1 6 2 5 10 8))  '( < )  )

;; similar thing using apply and lambda with partial function
(apply  (apply-partially 'sort '(1 6 2 5 10 8))  '( (lambda (x y) (> x y) ))  )


(mapcar 'atom '(2 1 3))  ;; t t t


(mapcar 'upcase '("chetan" "Mac" "trinity"))

(1+ 6)

(setq d 5)


;; the list function creates a list with the given arguments
;; it also evaluates the list arguments
(list 1 3 2  )

(list 1 2 1 (* 9 4) )   ;; outputs (1 2 1 36)

(list 1 2 1 '(* 9 3))   ;; outputs (1 2 1 (* 9 3)) , since the the last argument is quoted


(apply 'list '(2 3 1 7)) ;; outputs (2 3 1 7)


;; The special marker ‘,’ inside of the argument to backquote indicates a value that isn’t constant. The Emacs Lisp evaluator evaluates the argument of ‘,’, and puts the value in the list structure:
`(2 3 4 ,(+ 3 2) )

`(1 2 ,(+ 3 (+ 4 5) ))


;; Macros enable you to define new control constructs and other language features. A macro is defined much like a function, but instead of telling how to compute a value, it tells how to compute another Lisp expression which will in turn compute the value. We call this expression the expansion of the macro.

;;Macros can do this because they operate on the unevaluated expressions for the arguments, not on the argument values as functions do. They can therefore construct an expansion containing these argument expressions or parts of them.

;;If you are using a macro to do something an ordinary function could do, just for the sake of speed, consider using an inline function instead. See section Inline Functions.

;;Suppose we would like to define a Lisp construct to increment a variable value, much like the ++ operator in C. We would like to write (inc x) and have the effect of (setq x (1+ x)). Here's a macro definition that does the job: 
(defmacro inc (var)

  (list 'setq var (list '1+ var) )
  
  )

;;When this is called with (inc x), the argument var has the value x---not the value of x. The body of the macro uses this to construct the expansion, which is (setq x (1+ x)). Once the macro definition returns this expansion, Lisp proceeds to evaluate it, thus incrementing x. 

(setq y -3)
(inc y)


;;A list quoted with the backquote is quoted except for the elements prefixed with a comma. Those are evaluated before inserting. Therefore `(1 2 ,(+ 3 4)) ⇒ (1 2 7).
(inc y)
`(2 3 ,(       + 3 (* 6 7 4)  ))


(defmacro incbytwo (var)

  `(setq ,var (+ 2  ,var))

  )

(setq x 8)

(incbytwo x)

;;the inc by one example can be re-written using backquotes as:
(defmacro ++ (var)
  `(setq ,var (1+ ,var) )
  )

(++ x)

;; macros in LISP are thus a way to extend the syntax of the language and introducing newer semantics


;;funnyAdder takes one required and one optional arg.
;; if only one arg is provided it will add 2 to it and return
;; else it will add both given numbers
(defun funnyAdder (x &optional y)

  (if y
      (+ x y)
    (+ 2 x)
    )
  )



(funnyAdder 2 6)
(funnyAdder 5)
