;;; Step 11: next target programs
;;; Requires: optional features or compatibility aliases not in the required
;;; Mini-Scheme subset yet.
;;;
;;; This file is parse-checked by the Haskell test suite, but is not evaluated
;;; by default. Move individual cases into core once the corresponding feature
;;; is implemented.

;; === call/cc ===
(+ 5000 (call/cc (lambda (cont) (* 2 (cont 5) 3))) 1000 2000)
;; => 8005

(define bar1 (lambda (cont) (print "call bar1\n")))
(define bar2 (lambda (cont) (print "call bar2\n") (cont #f)))
(define bar3 (lambda (cont) (print "call bar3\n")))
(define test-callcc-bars (lambda (cont) (bar1 cont) (bar2 cont) (bar3 cont)))

(define find-do
  (lambda (fn ls)
    (call/cc
      (lambda (k)
        (do ((xs ls (cdr xs)))
            ((null? xs) #f)
          (if (fn (car xs)) (k (car xs))))))))

(find-do (lambda (x) (eq? 'c x)) '(a b c d e)) ;; => c
(find-do (lambda (x) (eq? 'c x)) '(a b d e f)) ;; => #f

(define fail (lambda () (print "no solution")))
(define in-range
  (lambda (a b)
    (call/cc (lambda (cont)
               (enumerate a b cont)))))
(define enumerate
  (lambda (a b cont)
    (if (> a b) (fail)
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
      (fail))) ;; => (4 3 5)

;; call/cc is call-with-current-continuation. The exact result of these
;; examples should be fixed when O6 semantics are implemented.
(define callcc-probe-f (lambda (x) x))
((call/cc call/cc) callcc-probe-f)
((call/cc call/cc) call/cc)

;; === display, print, and newline ===
(do ((x 0 (+ x 1))) ((= x 10) x) (print x)) ;; => 10

(define circular-display-target (list 1 2 3 4))
(set-cdr! (cdr (cdr circular-display-target)) circular-display-target)
(display circular-display-target)

;; === Common-Lisp-style macros ===
(define (let**-expander vars body)
  (if (null? vars)
      (cons 'begin body)
      (list 'let (list (car vars)) (let**-expander (cdr vars) body))))

(define-macro (let** vars . body)
  (let**-expander vars body))

;; === syntax-rules macros ===
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () body ...)
     ((lambda () body ...)))
    ((my-let* ((var val) rest ...) body ...)
     ((lambda (var) (let* (rest ...) body ...)) val))))

;; === R5RS-style car/cdr aliases ===
(define (solve H g)
  (define (in? x xl)
    (cond ((null? xl) #f)
          ((eq? x (car xl)) #t)
          (else (in? x (cdr xl)))))
  (define (solve-i g gl hl)
    (cond ((null? hl) #f)
          ((eq? (caar hl) g)
           (or (solve-i2 (cdar hl) (cons g gl))
               (solve-i g gl (cdr hl))))
          (else (solve-i g gl (cdr hl)))))
  (define (solve-i2 gs gl)
    (cond ((null? gs) #t)
          ((in? (car gs) gl) #f)
          (else
           (and (solve-i (car gs) gl H)
                (solve-i2 (cdr gs) gl)))))
  (solve-i g '() H))

(define env1 '((C B A) (B A) (A)))
(define env2 '((A B) (A C) (A D) (B C) (D C) (D)))
(solve env1 'B) ;; => #t
(solve env1 'C) ;; => #t
(solve env1 'D) ;; => #f
(solve env2 'A) ;; => #t
(solve env2 'B) ;; => #f

;; === floating-point literals and numeric tower support ===
(define (make-complex real imag)
  (list real imag))

(define (real-part z)
  (car z))

(define (imag-part z)
  (cadr z))

(define (complex-add z1 z2)
  (make-complex (+ (real-part z1) (real-part z2))
                (+ (imag-part z1) (imag-part z2))))

(define (complex-multiply z1 z2)
  (let ((a (real-part z1))
        (b (imag-part z1))
        (c (real-part z2))
        (d (imag-part z2)))
    (make-complex (- (* a c) (* b d))
                  (+ (* a d) (* b c)))))

(define (complex-magnitude-squared z)
  (+ (* (real-part z) (real-part z))
     (* (imag-part z) (imag-part z))))

(define (mandelbrot-iterate z c max-iter current-iter)
  (if (or (>= current-iter max-iter)
          (> (complex-magnitude-squared z) 4))
      current-iter
      (mandelbrot-iterate (complex-add (complex-multiply z z) c)
                          c
                          max-iter
                          (+ current-iter 1))))

(define (mandelbrot-point c max-iter)
  (mandelbrot-iterate (make-complex 0 0) c max-iter 0))

(define (mandelbrot-char iterations max-iter)
  (cond
    ((= iterations max-iter) "*")
    ((< iterations (/ max-iter 8)) " ")
    ((< iterations (/ max-iter 4)) ".")
    ((< iterations (/ max-iter 2)) "o")
    (else "+")))

(define (draw-mandelbrot width height x-min x-max y-min y-max max-iter)
  (define (draw-row y)
    (define (draw-col x)
      (if (< x width)
          (let* ((real-part (+ x-min (* (/ x width) (- x-max x-min))))
                 (imag-part (+ y-min (* (/ y height) (- y-max y-min))))
                 (c (make-complex real-part imag-part))
                 (iterations (mandelbrot-point c max-iter)))
            (display (mandelbrot-char iterations max-iter))
            (draw-col (+ x 1)))))
    (if (< y height)
        (begin
          (draw-col 0)
          (newline)
          (draw-row (+ y 1)))))
  (draw-row 0))

(define (julia-point z c max-iter)
  (mandelbrot-iterate z c max-iter 0))

(define (draw-julia width height x-min x-max y-min y-max c max-iter)
  (define (draw-row y)
    (define (draw-col x)
      (if (< x width)
          (let* ((real-part (+ x-min (* (/ x width) (- x-max x-min))))
                 (imag-part (+ y-min (* (/ y height) (- y-max y-min))))
                 (z (make-complex real-part imag-part))
                 (iterations (julia-point z c max-iter)))
            (display (mandelbrot-char iterations max-iter))
            (draw-col (+ x 1)))))
    (if (< y height)
        (begin
          (draw-col 0)
          (newline)
          (draw-row (+ y 1)))))
  (draw-row 0))

(newline)
(draw-mandelbrot 60 20 -2.5 1.0 -1.0 1.0 50)
(newline)

(newline)
(draw-julia 50 15 -1.5 1.5 -1.0 1.0 (make-complex -0.7 0.27015) 50)
(newline)
