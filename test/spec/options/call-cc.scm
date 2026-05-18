;;; Optional call/cc behavior.
;;;
;;; These forms exercise the first-class continuation requirements that
;;; are intentionally outside the Mini-Scheme core.

;;; Basic escape: invoking the captured continuation discards the pending
;;; multiplication and resumes at the surrounding addition.
(+ 5000 (call/cc (lambda (cont) (* 2 (cont 5) 3))) 1000 2000) ;; => 8005

;;; R5RS name alias.
(call-with-current-continuation (lambda (k) 7)) ;; => 7

;;; A continuation invocation abandons the rest of the current procedure body.
(call/cc (lambda (k) (k 'escaped) 'unreachable)) ;; => escaped

;;; Continuations and call/cc names are procedures.
(procedure? call/cc) ;; => #t
(procedure? call-with-current-continuation) ;; => #t

;;; Captured continuations can be stored and invoked after the original
;;; call/cc has returned.
(define saved-cont #f)
(+ 1 (call/cc (lambda (k) (set! saved-cont k) 10))) ;; => 11
(saved-cont 20) ;; => 21
(saved-cont 30) ;; => 31

;;; Early exit from a loop.
(define find-do
  (lambda (fn ls)
    (call/cc
      (lambda (k)
        (do ((xs ls (cdr xs)))
            ((null? xs) #f)
          (if (fn (car xs)) (k (car xs))))))))

(find-do (lambda (x) (eq? 'c x)) '(a b c d e)) ;; => c
(find-do (lambda (x) (eq? 'c x)) '(a b d e f)) ;; => #f

;;; Backtracking through a globally stored failure continuation.
(define fail (lambda () (print "no solution")))
(define in-range
  (lambda (a b)
    (call/cc
      (lambda (cont)
        (enumerate a b cont)))))
(define enumerate
  (lambda (a b cont)
    (if (> a b)
        (fail)
        (let ((save fail))
          (set! fail
            (lambda ()
              (set! fail save)
              (enumerate (+ a 1) b cont)))
          (cont a)))))

(let ((x (in-range 2 9))
      (y (in-range 2 9))
      (z (in-range 2 9)))
  (if (= (* x x) (+ (* y y) (* z z)))
      (list x y z)
      (fail))) ;; => (5 3 4)

;;; Error cases.
(call/cc) ;; => ERROR
(call/cc 1 2) ;; => ERROR
(call/cc 1) ;; => ERROR
