;; Everthing in LISP is a List
;; defining a variable in emacs  with setq
(setq a 5)

(setq b 5)

;;another way is
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

(printChocolate 1)

 
(defun add (x y ) (+ x y))

(defun square (x) (* x x))

(defun cube (x) (* x x x))


;; funcall expression computes the value of function
(defun operation  (fn x)  (funcall fn x )   )

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
;; so apply is like a function which "spreads" the given list into argumemnts
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

