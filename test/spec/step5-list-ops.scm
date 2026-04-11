;;; Step 5: List operations (built-in functions)
;;; Requires: Step 4 + list builtins

;; === cons, car, cdr ===
(cons 1 2)              ;; => (1 . 2)
(cons 1 '())            ;; => (1)
(cons 1 (cons 2 '()))   ;; => (1 2)
(cons '(1 2) '(3 4))    ;; => ((1 2) 3 4)
(car '(1 2 3))          ;; => 1
(cdr '(1 2 3))          ;; => (2 3)
(car (cdr '(1 2 3)))    ;; => 2
(cdr (cdr '(1 2 3)))    ;; => (3)
(cdr '(1))              ;; => ()

;; car/cdr on empty list should error
;; (car '())             ;; => ERROR
;; (cdr '())             ;; => ERROR

;; === list ===
(list)                  ;; => ()
(list 1)                ;; => (1)
(list 1 2 3)            ;; => (1 2 3)
(list 1 (+ 1 1) 3)     ;; => (1 2 3)

;; === predicates ===
(null? '())             ;; => #t
(null? '(1))            ;; => #f
(null? 1)               ;; => #f
(pair? '(1 2))          ;; => #t
(pair? (cons 1 2))      ;; => #t
(pair? '())             ;; => #f
(pair? 1)               ;; => #f
(list? '())             ;; => #t
(list? '(1 2 3))        ;; => #t
(list? (cons 1 2))      ;; => #f
(list? '((1 2) (3 4)))  ;; => #t
(symbol? 'x)            ;; => #t
(symbol? 1)             ;; => #f
(symbol? "x")           ;; => #f
(symbol? '())           ;; => #f

;; === length ===
(length '())            ;; => 0
(length '(1))           ;; => 1
(length '(1 2 3))       ;; => 3
;; (length (cons 1 2))  ;; => ERROR (improper list)
;; (length 42)          ;; => ERROR

;; === append ===
(append '(1 2) '(3 4))           ;; => (1 2 3 4)
(append '() '(1 2))              ;; => (1 2)
(append '(1 2) '())              ;; => (1 2)
(append '(1) '(2) '(3))          ;; => (1 2 3)
(append)                          ;; => ()
(append '(1 2))                   ;; => (1 2)
(append '() '())                  ;; => ()

;; === last ===
(last '(1 2 3))         ;; => 3
(last '(1))             ;; => 1
;; (last '())           ;; => ERROR

;; === memq ===
(memq 2 '(1 2 3))      ;; => (2 3)
(memq 4 '(1 2 3))      ;; => #f
(memq 'b '(a b c))     ;; => (b c)
(memq 1 '())            ;; => #f

;; === set-car!, set-cdr! ===
(define p (cons 1 2))
(set-car! p 10)
(car p)                 ;; => 10
(set-cdr! p 20)
(cdr p)                 ;; => 20

(define q (list 1 2 3))
(set-car! q 10)
q                       ;; => (10 2 3)

;; set-cdr! to truncate a list
(define r (list 1 2 3))
(set-cdr! r '())
r                       ;; => (1)

;; set-car!/set-cdr! on non-pair should error
;; (set-car! 1 2)       ;; => ERROR
;; (set-cdr! 1 2)       ;; => ERROR
