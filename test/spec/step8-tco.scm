;;; Step 8: Tail Call Optimization (Option O1)
;;; Requires: All required features + TCO
;;; These tests should complete without stack overflow.

;; Mutual recursion with large input
(define (even? n)
  (if (= n 0) #t (odd? (- n 1))))
(define (odd? n)
  (if (= n 0) #f (even? (- n 1))))

(even? 100000)          ;; => #t
(odd? 100001)           ;; => #t

;; Tail-recursive loop
(define (loop n)
  (if (= n 0) 'done (loop (- n 1))))
(loop 1000000)          ;; => done

;; Tail position in begin
(define (loop-begin n)
  (if (= n 0) 'done
      (begin 1 2 (loop-begin (- n 1)))))
(loop-begin 1000000)    ;; => done

;; Tail position in let
(define (loop-let n)
  (if (= n 0) 'done
      (let ((m (- n 1)))
        (loop-let m))))
(loop-let 1000000)      ;; => done

;; Tail position in let*
(define (loop-let* n)
  (if (= n 0) 'done
      (let* ((m (- n 1)))
        (loop-let* m))))
(loop-let* 1000000)     ;; => done

;; Tail position in letrec
(define (test-letrec-tco)
  (letrec ((f (lambda (n) (if (= n 0) 'done (f (- n 1))))))
    (f 1000000)))
(test-letrec-tco)       ;; => done

;; Tail position in cond
(define (loop-cond n)
  (cond ((= n 0) 'done)
        (else (loop-cond (- n 1)))))
(loop-cond 1000000)     ;; => done

;; Tail position in and
(define (loop-and n)
  (if (= n 0) 'done
      (and #t (loop-and (- n 1)))))
(loop-and 1000000)      ;; => done

;; Tail position in or
(define (loop-or n)
  (if (= n 0) 'done
      (or #f (loop-or (- n 1)))))
(loop-or 1000000)       ;; => done

;; Named let (tail recursive)
(let loop ((n 1000000))
  (if (= n 0) 'done (loop (- n 1)))) ;; => done

;; do (inherently iterative)
(do ((i 0 (+ i 1)))
    ((= i 1000000) 'done))          ;; => done
