;;; Step 2: define, lambda, function application
;;; Requires: Step 1 + define + lambda + closures

;; Simple define
(define x 42)
x               ;; => 42

;; Define and use
(define y (+ 1 2))
y               ;; => 3

;; Lambda
((lambda (x) x) 42)              ;; => 42
((lambda (x y) (+ x y)) 1 2)    ;; => 3
((lambda () 42))                 ;; => 42

;; Define function (sugar)
(define (double x) (* x 2))
(double 5)      ;; => 10

;; Define function (lambda form)
(define triple (lambda (x) (* x 3)))
(triple 5)      ;; => 15

;; Define function with no args (sugar)
(define (fortytwo) 42)
(fortytwo)      ;; => 42

;; Recursive function
(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
(fact 0)        ;; => 1
(fact 1)        ;; => 1
(fact 5)        ;; => 120
(fact 10)       ;; => 3628800

;; Mutual recursion
(define (my-even? n)
  (if (= n 0) #t (my-odd? (- n 1))))
(define (my-odd? n)
  (if (= n 0) #f (my-even? (- n 1))))
(my-even? 0)    ;; => #t
(my-even? 4)    ;; => #t
(my-odd? 3)     ;; => #t
(my-odd? 4)     ;; => #f

;; Higher-order function
(define (apply-twice f x) (f (f x)))
(apply-twice double 3)   ;; => 12

;; Closure
(define (make-adder n)
  (lambda (x) (+ n x)))
(define add5 (make-adder 5))
(add5 10)       ;; => 15
(add5 20)       ;; => 25

;; === Variadic arguments (rest parameter) ===

;; lambda with rest only (Arg ::= Id)
((lambda args args) 1 2 3)   ;; => (1 2 3)
((lambda args args))          ;; => ()

;; define with rest parameter
(define (f x . rest) rest)
(f 1 2 3)       ;; => (2 3)
(f 1)           ;; => ()

;; define with only rest parameter
(define (g . args) args)
(g 1 2 3)       ;; => (1 2 3)
(g)             ;; => ()

;; define sugar with multiple fixed + rest
(define (h x y . z) z)
(h 1 2 3 4)    ;; => (3 4)
(h 1 2)        ;; => ()

;; lambda with multiple fixed + rest
((lambda (x y . z) z) 1 2 3 4 5) ;; => (3 4 5)

;; Arity error: too few arguments
;; (f) with (define (f x . rest) rest) should error since x is required
;; (already defined above, redefine to be safe)
(define (need-one x . rest) x)
;; (need-one) ;; => ERROR

;; === Body with internal define ===
(define (body-test x)
  (define y (* x 2))
  (+ y 1))
(body-test 5)   ;; => 11

;; Multiple internal defines
(define (multi-def x)
  (define a 1)
  (define b 2)
  (define c 3)
  (+ x a b c))
(multi-def 10)  ;; => 16

;; Body with multiple expressions (returns last)
(define (multi-expr)
  (define x 1)
  x
  (+ x 1))
(multi-expr)    ;; => 2

;; Variable shadowing
(define outer 10)
((lambda (outer) outer) 20)   ;; => 20
outer                          ;; => 10
