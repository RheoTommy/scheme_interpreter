;;; Step 3: Special forms (if, cond, and, or, begin, set!, quote)
;;; Requires: Step 2 + all special forms except let/letrec/do

;; === if ===
(if #t 1 2)         ;; => 1
(if #f 1 2)         ;; => 2
(if #t 1)           ;; => 1
(if 0 1 2)          ;; => 1
(if "" 1 2)         ;; => 1
(if '() 1 2)        ;; => 1

;; Note: In Scheme, only #f is falsy. 0, "", '() are all truthy.

;; if with no else, condition false (unspecified return)
(if #f 1)           ;; => (unspecified, e.g. () or void)

;; === cond ===
(cond (#t 1))                    ;; => 1
(cond (#f 1) (#t 2))            ;; => 2
(cond (#f 1) (#f 2) (else 3))   ;; => 3
(cond ((= 1 1) 10))             ;; => 10
(cond ((= 1 2) 10) (else 20))   ;; => 20

;; cond with multiple expressions in a clause
(cond (#t 1 2 3))               ;; => 3

;; cond with else only
(cond (else 42))                ;; => 42

;; cond with no matching clause and no else (unspecified)
(cond (#f 1) (#f 2))            ;; => (unspecified)

;; cond syntax errors
;; (cond)            ;; => ERROR (no clauses, spec note 5)
;; (cond (#t))       ;; => ERROR (clause needs Exp+, not just test)

;; === and ===
(and)               ;; => #t
(and 1)             ;; => 1
(and 1 2)           ;; => 2
(and 1 2 3)         ;; => 3
(and #f 2)          ;; => #f
(and 1 #f 3)        ;; => #f

;; and returns the actual value, not converted to bool
(and 1 '())         ;; => ()
(and 1 "hello")     ;; => "hello"

;; Short-circuit evaluation
(define and-test 0)
(and #f (set! and-test 1))
and-test            ;; => 0

;; === or ===
(or)                ;; => #f
(or 1)              ;; => 1
(or #f 2)           ;; => 2
(or #f #f 3)        ;; => 3
(or #f #f #f)       ;; => #f

;; or returns the first truthy value
(or '() 2)          ;; => ()
(or "" 2)           ;; => ""

;; Short-circuit evaluation
(define or-test 0)
(or 1 (set! or-test 1))
or-test             ;; => 0

;; === begin ===
(begin 1)           ;; => 1
(begin 1 2 3)       ;; => 3

;; Side effects in sequence
(define seq-test 0)
(begin (set! seq-test 1) (set! seq-test 2) seq-test) ;; => 2

;; begin with define
(begin (define begin-var 42) begin-var)  ;; => 42

;; === quote ===
(quote x)           ;; => x
(quote (1 2 3))     ;; => (1 2 3)
'x                  ;; => x
'(1 2 3)            ;; => (1 2 3)
'(a b c)            ;; => (a b c)
'(+ 1 2)            ;; => (+ 1 2)
'()                 ;; => ()
'(a (b c) d)        ;; => (a (b c) d)
'(a . b)            ;; => (a . b)

;; Nested quote
(quote (quote x))   ;; => (quote x)
''x                 ;; => (quote x)

;; Quoted dot pair normalizes to list
'(a . (b . (c . ()))) ;; => (a b c)

;; === set! ===
(define a 1)
(set! a 2)
a                   ;; => 2

;; set! with closure (shared mutable state)
(define counter 0)
(define (inc!) (set! counter (+ counter 1)))
(inc!)
(inc!)
(inc!)
counter             ;; => 3

;; set! on unbound variable
;; (set! no-such-var 42)  ;; => ERROR
