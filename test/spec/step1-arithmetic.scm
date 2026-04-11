;;; Step 1: Basic arithmetic and built-in predicates
;;; Requires: Parser + Eval (function application) + arithmetic builtins

;; Addition
(+ 1 2)         ;; => 3
(+ 1 2 3)       ;; => 6
(+)             ;; => 0

;; Subtraction
(- 5 3)         ;; => 2
(- 10 3 2)      ;; => 5
(- 5)           ;; => -5

;; Multiplication
(* 2 3)         ;; => 6
(* 2 3 4)       ;; => 24
(*)             ;; => 1

;; Division
(/ 10 2)        ;; => 5
(/ 10 3)        ;; => 3

;; Division by zero
(/ 10 0)        ;; => ERROR

;; Comparison
(= 1 1)         ;; => #t
(= 1 2)         ;; => #f
(< 1 2)         ;; => #t
(< 2 1)         ;; => #f
(< 1 1)         ;; => #f
(<= 1 1)        ;; => #t
(<= 2 1)        ;; => #f
(> 2 1)         ;; => #t
(> 1 1)         ;; => #f
(>= 1 1)        ;; => #t
(>= 1 2)        ;; => #f

;; Nested arithmetic
(+ 1 (* 2 3))       ;; => 7
(* (+ 1 2) (+ 3 4)) ;; => 21
(- (* 3 3) (* 2 2)) ;; => 5

;; Negative number arithmetic
(+ -1 -2)       ;; => -3
(* -3 -4)       ;; => 12

;; Type predicates
(number? 42)     ;; => #t
(number? #t)     ;; => #f
(number? "hi")   ;; => #f
(number? '())    ;; => #f
(boolean? #t)    ;; => #t
(boolean? 1)     ;; => #f
(string? "hi")   ;; => #t
(string? 42)     ;; => #f

;; Type errors in arithmetic
(+ 1 "a")        ;; => ERROR
(* "a" 2)        ;; => ERROR
(= "a" "a")      ;; => ERROR
(< #t #f)        ;; => ERROR

;; Arity errors
(-)              ;; => ERROR
