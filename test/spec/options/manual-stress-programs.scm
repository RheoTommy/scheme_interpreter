;;; Manual stress programs
;;; These programs intentionally diverge or stress resource behavior. The
;;; Haskell test suite only parse-checks this file.

;; Mutual recursion should not overflow the host stack once TCO is implemented.
(define manual-loop-a (lambda (v) (manual-loop-b v)))
(define manual-loop-b (lambda (v) (manual-loop-a v)))
(manual-loop-a 32)

;; Infinite do loop; useful for Ctrl-C and bounded-runner behavior.
(do ((x 0 (+ x 1))) (#f x) (print x))

;; Recursive allocation with cyclic garbage.
(define (cyclic-allocation-f x)
  (define (g x)
    (let* ((t (cons 1 '()))
           (s (cons 2 t)))
      (set-cdr! t s)
      (cyclic-allocation-f x)))
  (if (<= x 0)
      '()
      (g (- x 1))))
