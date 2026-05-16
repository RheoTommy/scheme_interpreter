;;; R5RS-portable Mini-Scheme subset
;;; Requires: required Mini-Scheme features only
;;; This file is also suitable for differential testing against an R5RS
;;; implementation because it avoids Mini-Scheme-only behavior,
;;; unspecified return values, implementation-dependent eq? cases, and
;;; integer division differences.

;; Literals and quote
42                         ;; => 42
#t                         ;; => #t
#f                         ;; => #f
"hello"                    ;; => "hello"
'()                        ;; => ()
'x                         ;; => x
'(a b c)                   ;; => (a b c)
'(a . b)                   ;; => (a . b)

;; Arithmetic and comparison
(+ 1 2 3)                  ;; => 6
(- 10 3 2)                 ;; => 5
(* 2 3 4)                  ;; => 24
(/ 10 2)                   ;; => 5
(= 2 2)                    ;; => #t
(< 1 2)                    ;; => #t
(<= 2 2)                   ;; => #t
(> 3 2)                    ;; => #t
(>= 3 3)                   ;; => #t

;; Conditionals and boolean semantics
(if #f 1 2)                ;; => 2
(if '() 1 2)               ;; => 1
(cond ((= 1 2) 'no)
      ((= 2 2) 'yes)
      (else 'bad))         ;; => yes
(and 1 2 3)                ;; => 3
(and 1 #f 3)               ;; => #f
(or #f 'ok)                ;; => ok
(not #f)                   ;; => #t
(not '())                  ;; => #f

;; Bindings and closures
(define portable-x 10)
portable-x                 ;; => 10
((lambda (x y) (+ x y)) 2 3) ;; => 5
(let ((x 1) (y 2)) (+ x y)) ;; => 3
(let* ((x 1) (y (+ x 1))) y) ;; => 2
(letrec ((even? (lambda (n)
                  (if (= n 0) #t (odd? (- n 1)))))
         (odd? (lambda (n)
                 (if (= n 0) #f (even? (- n 1))))))
  (odd? 5))                ;; => #t
(let loop ((n 5) (acc 1))
  (if (= n 0) acc (loop (- n 1) (* acc n)))) ;; => 120

;; Lists
(cons 1 2)                 ;; => (1 . 2)
(list 1 2 3)               ;; => (1 2 3)
(car '(1 2 3))             ;; => 1
(cdr '(1 2 3))             ;; => (2 3)
(append '(1 2) '(3 4))     ;; => (1 2 3 4)
(length '(1 2 3))          ;; => 3
(memq 'b '(a b c))         ;; => (b c)

;; Equality and strings
(eq? 'a 'a)                ;; => #t
(equal? '(1 (2 3)) '(1 (2 3))) ;; => #t
(string-append "a" "b")    ;; => "ab"
(symbol->string 'hello)    ;; => "hello"
(string->symbol "hello")   ;; => hello
(number->string 42)        ;; => "42"
(string->number "42")      ;; => 42

;; do
(define portable-sum 0)
(do ((i 0 (+ i 1)))
    ((= i 5) portable-sum)
  (set! portable-sum (+ portable-sum i))) ;; => 10
