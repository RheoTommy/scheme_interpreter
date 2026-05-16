;;; Step 11: Target regressions
;;; Requires: required Mini-Scheme features only
;;; These examples come from the next demo target set, restricted to programs
;;; that should already be executable without optional features.

;; === let* captures the earlier binding ===
(let* ((x 1)
       (f (lambda () x))
       (x 2))
  (f)) ;; => 1

;; === malformed dotted special/application forms report errors ===
(begin . 1) ;; => ERROR
(cons . 1)  ;; => ERROR

;; === runtime errors do not poison the top-level environment ===
(define top-error-x 0)
(define (top-error-f x) (/ 1 0))
(top-error-f 10) ;; => ERROR
top-error-x      ;; => 0

;; === type errors are reported without crashing ===
(car (lambda (x) x)) ;; => ERROR

;; === S/K combinators ===
(define s (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
(define k (lambda (x) (lambda (y) x)))
(((s k) k) 123) ;; => 123

;; === fold-left ===
(define (fold-left f a l)
  (if (null? l) a
      (fold-left f (f a (car l)) (cdr l))))
(fold-left + 0 '(1 2 3 4 5)) ;; => 15

;; === closure-local mutable state ===
(define gen-counter
  (lambda (start)
    (define v start)
    (lambda (b) (set! v (+ v 1)) v)))
(define c0up (gen-counter 0))
(c0up 0) ;; => 1
(c0up 0) ;; => 2
(define c1up (gen-counter 0))
(c1up 0) ;; => 1
(c0up 0) ;; => 3

;; === nested improper lists ===
(cons 1 (cons (cons 2 (cons "A" "B")) (cons 3 4)))
;; => (1 (2 "A" . "B") 3 . 4)

;; === division errors ===
(/ 0)       ;; => ERROR
(/ 1 2 0 3) ;; => ERROR

;; === letrec initializers cannot observe peer placeholders ===
(letrec ((a b) (b a)) a) ;; => ERROR

;; === state captured by nested closures ===
(define (demo-counter-f x) (lambda () (set! x (+ 1 x)) x))
(define demo-counter-g (demo-counter-f 10))
(demo-counter-g) ;; => 11

(define (demo-counter-f2 x)
  ((lambda () (lambda () (set! x (+ 1 x)) x))))
(define demo-counter-g2 (demo-counter-f2 10))
(demo-counter-g2) ;; => 11

;; === internal defines ===
(define demo-local-f (lambda (x) (define y x) (set! y 10) y))
(demo-local-f 1) ;; => 10

(define (demo-label-f x y)
  (define (label x y) (begin (* x y)))
  (label (+ x x) (+ y y)))
(demo-label-f 2 3) ;; => 24

;; === named let ===
(define (fact-let n)
  (let loop ((n1 n) (p n))
    (if (= n1 1)
        p
        (let ((m (- n1 1)))
          (loop m (* p m))))))
(fact-let 5) ;; => 120

;; === small recursive sums ===
(define (sum-direct n) (if (= n 0) 0 (+ n (sum-direct (- n 1)))))
(define (sum-tail n acc) (if (= n 0) acc (sum-tail (- n 1) (+ acc n))))
(sum-direct 10) ;; => 55
(sum-tail 10 0) ;; => 55

;; === do loop ===
(define reverse-do
  (lambda (xs)
    (do ((ls xs (cdr ls)) (result '() result))
        ((null? ls) result)
      (set! result (cons (car ls) result)))))
(reverse-do '(a b c d e)) ;; => (e d c b a)

(do ((x 0 (+ x 1))) ((= x 10) x) x) ;; => 10

;; === continuation-passing factorial without call/cc ===
(define (fact-k n)
  (let fact-k-loop ((n n) (cont (lambda (n) n)))
    (if (= n 0)
        (cont 1)
        (fact-k-loop (- n 1) (lambda (x) (cont (* x n)))))))
(fact-k 10) ;; => 3628800

;; === mutation of a parameter binding ===
(define shadow-arg-f (lambda (f) (set! f (+ f 1)) f))
(shadow-arg-f 0) ;; => 1
(shadow-arg-f 2) ;; => 3

;; === cyclic pair traversal remains finite for bounded access ===
(define cyclic-l (cons 1 (cons 3 '())))
(set-cdr! (cdr cyclic-l) cyclic-l)
(- (car (cdr (cdr (cdr (cdr (cdr cyclic-l))))))
   (car (cdr (cdr cyclic-l)))) ;; => 2

;; === cyclic pair output is guarded ===
(define circular-output (list 1 2 3 4))
(set-cdr! (cdr (cdr circular-output)) circular-output)
circular-output ;; => (1 2 3 . #<cycle>)
