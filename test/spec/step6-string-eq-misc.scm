;;; Step 6: String operations, equality, misc builtins
;;; Requires: Step 5 + remaining builtins

;; === string operations ===
(string-append "hello" " " "world")  ;; => "hello world"
(string-append)                       ;; => ""
(string-append "a" "b")              ;; => "ab"

(symbol->string 'hello)              ;; => "hello"
(string->symbol "hello")             ;; => hello
(string->number "42")                ;; => 42
(string->number "-42")               ;; => -42
(number->string 42)                  ;; => "42"
(number->string -42)                 ;; => "-42"

;; Type errors in string operations
;; (string-append "a" 1)             ;; => ERROR
;; (symbol->string 42)               ;; => ERROR
;; (string->symbol 42)               ;; => ERROR
;; (string->number 42)               ;; => ERROR
;; (number->string "42")             ;; => ERROR

;; string->number with non-numeric string (behavior may vary: error or #f)
;; (string->number "abc")            ;; => #f or ERROR

;; === equality ===
;; eq? tests reference equality
(eq? 'a 'a)             ;; => #t
(eq? 'a 'b)             ;; => #f
(eq? 1 1)               ;; => #t
(eq? #t #t)             ;; => #t
(eq? '() '())           ;; => #t
;; eq? on lists: reference equality, typically #f for distinct allocations
;; (eq? '(1) '(1))      ;; => implementation-dependent

;; neq?
(neq? 'a 'b)            ;; => #t
(neq? 'a 'a)            ;; => #f

;; equal? tests structural equality
(equal? '(1 2 3) '(1 2 3))   ;; => #t
(equal? '(1 2) '(1 3))       ;; => #f
(equal? "hello" "hello")     ;; => #t
(equal? '(1 (2 3)) '(1 (2 3))) ;; => #t
(equal? '(1 . 2) '(1 . 2))  ;; => #t
(equal? 1 "1")               ;; => #f

;; === not ===
(not #f)                ;; => #t
(not #t)                ;; => #f
(not 0)                 ;; => #f
(not '())               ;; => #f
(not "")                ;; => #f
(not 1)                 ;; => #f

;; === procedure? ===
(procedure? car)         ;; => #t
(procedure? +)           ;; => #t
(procedure? (lambda (x) x)) ;; => #t
(procedure? 42)          ;; => #f
(procedure? "hi")        ;; => #f
