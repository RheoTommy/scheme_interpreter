;;; Step 7: Error handling
;;; Requires: All required features
;;; The interpreter must report errors without crashing.
;;; Each expression below should produce an error message,
;;; and the REPL should continue to accept input afterwards.

;; === Parse errors ===
(                       ;; => ERROR: unexpected end of input
())(                    ;; => ERROR: unexpected token
"unterminated           ;; => ERROR: unterminated string

;; === Unbound variable ===
undefined-var           ;; => ERROR: unbound variable

;; === Type errors ===
(+ 1 "a")              ;; => ERROR: not a number
(car 1)                ;; => ERROR: not a pair
(cdr 1)                ;; => ERROR: not a pair
(car '())              ;; => ERROR: empty list
(cdr '())              ;; => ERROR: empty list
(set-car! 1 2)         ;; => ERROR: not a pair
(set-cdr! 1 2)         ;; => ERROR: not a pair
(length 42)            ;; => ERROR: not a list
(/ 1 0)                ;; => ERROR: division by zero

;; === Arity errors ===
(if)                    ;; => ERROR
(if 1)                  ;; => ERROR
(if 1 2 3 4)           ;; => ERROR
(lambda)               ;; => ERROR
(quote)                ;; => ERROR
(quote 1 2)            ;; => ERROR
(-)                    ;; => ERROR

;; === Application of non-procedure ===
(1 2 3)                ;; => ERROR: not a procedure
("hello" 1)            ;; => ERROR: not a procedure

;; === set! on unbound variable ===
(set! no-such-var 42)  ;; => ERROR: unbound variable

;; === Invalid define syntax ===
;; (define)              ;; => ERROR
;; (define 1 2)          ;; => ERROR: not an identifier
;; (define (1) 2)        ;; => ERROR: not an identifier

;; === Invalid let syntax ===
;; (let)                 ;; => ERROR
;; (let ((1 2)) 3)       ;; => ERROR: not an identifier in binding
;; (let (x) x)           ;; => ERROR: bad binding form

;; === Invalid cond syntax ===
;; (cond)                ;; => ERROR: no clauses
;; (cond (#t))           ;; => ERROR: clause needs body

;; === Arity mismatch in user-defined functions ===
((lambda (x y) x) 1)       ;; => ERROR: too few arguments
((lambda (x) x) 1 2)       ;; => ERROR: too many arguments

;; === Multiple errors then recovery ===
;; After all the above errors, these should still work:
(+ 1 2)                ;; => 3
(* 3 4)                ;; => 12
(list 1 2 3)           ;; => (1 2 3)
