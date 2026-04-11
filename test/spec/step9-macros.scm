;;; Step 9: Common-Lisp-style macros (Option O5)
;;; Requires: All required features + define-macro

;; === Basic macro ===
(define-macro (positive x)
  (list '> x 0))

(positive 5)            ;; => #t
(positive -3)           ;; => #f
(positive (+ 1 2))      ;; => #t

;; === Macro vs function: arguments not evaluated ===
;; This macro receives the unevaluated expression (+ 1 2),
;; not the value 3.
(define-macro (my-if cond then else)
  (list 'if cond then else))

(my-if #t 'yes 'no)     ;; => yes
(my-if #f 'yes 'no)     ;; => no

;; === Macro that creates definitions ===
(define-macro (def-const name val)
  (list 'define name val))

(def-const pi 314)
pi                       ;; => 314

;; === let* defined as a macro ===
(define (let*-expander vars body)
  (if (null? vars)
    (cons 'begin body)
    (list 'let (list (car vars))
          (let*-expander (cdr vars) body))))

(define-macro (my-let* vars . body)
  (let*-expander vars body))

(my-let* ((x 1) (y (+ x 1)) (z (+ x y)))
  z)                     ;; => 4

;; === when / unless ===
(define-macro (when test . body)
  (list 'if test (cons 'begin body) #f))

(define-macro (unless test . body)
  (list 'if test #f (cons 'begin body)))

(when #t 'yes)           ;; => yes
(when #f 'yes)           ;; => #f
(unless #f 'yes)         ;; => yes
(unless #t 'yes)         ;; => #f

;; === while loop as macro ===
(define-macro (while test . body)
  (list 'let 'loop '()
    (list 'if test
      (cons 'begin (append body '((loop))))
      ''done)))

(define i 0)
(while (< i 5) (set! i (+ i 1)))
i                        ;; => 5

;; === Macro that generates a macro ===
(define-macro (def-predicate name op val)
  (list 'define-macro (list name 'x)
    (list 'list (list 'quote op) 'x val)))

(def-predicate negative < 0)
(negative -3)            ;; => #t
(negative 3)             ;; => #f
