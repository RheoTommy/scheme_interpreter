;;; Step 4: let, let*, letrec, named let, do
;;; Requires: Step 3 + let forms + do

;; === let ===
(let () 1)                          ;; => 1
(let ((x 1)) x)                    ;; => 1
(let ((x 1) (y 2)) (+ x y))       ;; => 3

;; let does NOT see its own bindings
;; This should use the outer x=10, not the let-bound x=1
(define let-outer 10)
(let ((x 1) (y let-outer)) y)     ;; => 10

;; More precise scope test: y should NOT see x from the same let
;; (let ((x 1) (y x)) y) would use whatever x is in outer scope
(define scope-x 99)
(let ((x 1) (y scope-x)) y)       ;; => 99

;; let with body containing multiple expressions
(let ((x 1))
  (define y 2)
  (+ x y))                         ;; => 3

;; set! inside let modifies outer variable
(define let-mut 0)
(let ((x 1)) (set! let-mut 10))
let-mut                             ;; => 10

;; set! inside let modifies let-bound variable
(let ((x 1)) (set! x 2) x)        ;; => 2

;; === named let ===
(let loop ((n 5) (acc 1))
  (if (= n 0) acc
      (loop (- n 1) (* acc n))))   ;; => 120

(let loop ((lst '(1 2 3)) (sum 0))
  (if (null? lst) sum
      (loop (cdr lst) (+ sum (car lst))))) ;; => 6

;; named let with no bindings
(define named-let-counter 0)
(let loop ()
  (if (= named-let-counter 3) 'done
      (begin (set! named-let-counter (+ named-let-counter 1))
             (loop))))             ;; => done

;; === let* ===
(let* () 1)                         ;; => 1
(let* ((x 1)) x)                   ;; => 1
(let* ((x 1) (y (+ x 1))) y)      ;; => 2
(let* ((x 1) (y (+ x 1)) (z (+ x y))) z) ;; => 4

;; let* shadowing: same name can be rebound
(let* ((x 1) (x (+ x 1))) x)      ;; => 2

;; === letrec ===
(letrec ((x 1)) x)                 ;; => 1
(letrec () 1)                      ;; => 1

;; letrec for mutually recursive local functions
(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
         (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))
  (even? 10))                       ;; => #t

(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
         (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))
  (odd? 11))                        ;; => #t

;; === do ===
;; (do ((var init step) ...) (test result ...) body ...)

;; Simple counting loop
(do ((i 0 (+ i 1)))
    ((= i 5) i)
  #f)                               ;; => 5

;; Factorial with do
(do ((n 5 (- n 1))
     (acc 1 (* acc n)))
    ((= n 0) acc)
  #f)                               ;; => 120

;; do with body (side effects)
(define do-sum 0)
(do ((i 1 (+ i 1)))
    ((> i 5))
  (set! do-sum (+ do-sum i)))
do-sum                              ;; => 15

;; do with multiple result expressions (returns last)
(do ((i 0 (+ i 1)))
    ((= i 1) 'a 'b)
  #f)                               ;; => b

;; do with no result expression (unspecified return)
(do ((i 0 (+ i 1)))
    ((= i 1))
  #f)                               ;; => (unspecified)
