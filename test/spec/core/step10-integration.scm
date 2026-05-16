;;; Step 10: Integration tests
;;; Requires: All features
;;; Larger programs that exercise multiple features together.

;; === Map and filter ===
(define (map f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (map f (cdr lst)))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(map (lambda (x) (* x x)) '(1 2 3 4 5))   ;; => (1 4 9 16 25)
(filter (lambda (x) (> x 3)) '(1 2 3 4 5)) ;; => (4 5)

;; === Reduce (fold-left) ===
(define (reduce f init lst)
  (if (null? lst) init
      (reduce f (f init (car lst)) (cdr lst))))

(reduce + 0 '(1 2 3 4 5))   ;; => 15
(reduce * 1 '(1 2 3 4 5))   ;; => 120

;; === Association list ===
(define (assq key alist)
  (cond ((null? alist) #f)
        ((eq? key (car (car alist))) (car alist))
        (else (assq key (cdr alist)))))

(define env '((x 1) (y 2) (z 3)))
(assq 'y env)               ;; => (y 2)
(assq 'w env)               ;; => #f

;; === Fibonacci ===
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(fib 0)     ;; => 0
(fib 1)     ;; => 1
(fib 10)    ;; => 55
(fib 20)    ;; => 6765

;; === Tail-recursive Fibonacci ===
(define (fib-iter n)
  (define (go n a b)
    (if (= n 0) a (go (- n 1) b (+ a b))))
  (go n 0 1))

(fib-iter 10)   ;; => 55
(fib-iter 30)   ;; => 832040

;; === Sorting (insertion sort) ===
(define (insert x lst)
  (cond ((null? lst) (list x))
        ((<= x (car lst)) (cons x lst))
        (else (cons (car lst) (insert x (cdr lst))))))

(define (isort lst)
  (if (null? lst) '()
      (insert (car lst) (isort (cdr lst)))))

(isort '(5 3 1 4 2))       ;; => (1 2 3 4 5)
(isort '())                 ;; => ()
(isort '(1))                ;; => (1)
(isort '(3 1 2 1))          ;; => (1 1 2 3)

;; === Accumulator pattern (closure with mutable state) ===
(define (make-counter)
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      n)))

(define c1 (make-counter))
(define c2 (make-counter))
(c1)    ;; => 1
(c1)    ;; => 2
(c1)    ;; => 3
(c2)    ;; => 1
(c2)    ;; => 2

;; === Getter/setter pattern (shared closure state) ===
(define (make-box init)
  (let ((val init))
    (cons (lambda () val)               ;; getter
          (lambda (x) (set! val x)))))  ;; setter

(define box (make-box 0))
((car box))            ;; => 0
((cdr box) 42)
((car box))            ;; => 42

;; === Church numerals ===
(define zero (lambda (f) (lambda (x) x)))
(define (succ n) (lambda (f) (lambda (x) (f ((n f) x)))))
(define (church->int n) ((n (lambda (x) (+ x 1))) 0))

(church->int zero)                   ;; => 0
(church->int (succ zero))            ;; => 1
(church->int (succ (succ zero)))     ;; => 2
(church->int (succ (succ (succ zero)))) ;; => 3

;; === Y combinator ===
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

(define fact
  (Y (lambda (self)
       (lambda (n)
         (if (= n 0) 1 (* n (self (- n 1))))))))

(fact 5)    ;; => 120
(fact 10)   ;; => 3628800

;; === Quote manipulation (data as code structure) ===
(car '(+ 1 2))         ;; => +
(cdr '(+ 1 2))         ;; => (1 2)
(symbol? (car '(+ 1 2))) ;; => #t

;; === do loop with internal define ===
(define do-def-result 0)
(do ((i 0 (+ i 1)))
    ((= i 3))
  (define x (* i i))
  (set! do-def-result (+ do-def-result x)))
do-def-result          ;; => 5
