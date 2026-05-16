;;; Step 0: Parser - Literals and atoms
;;; Requires: Parser only (SExpr output)
;;; Each line is an independent expression => expected value.

;; Integer literals
42          ;; => 42
0           ;; => 0
-1          ;; => -1
999999      ;; => 999999

;; Boolean literals
#t          ;; => #t
#f          ;; => #f

;; String literals
"hello"     ;; => "hello"
""          ;; => ""
"hello world" ;; => "hello world"
"escape: \" \\ \n" ;; => "escape: \" \\ \n"
"tab:\there" ;; => "tab:\there"

;; Empty list
'()         ;; => ()

;; Quoted symbols
'x          ;; => x
'hello      ;; => hello
'+          ;; => +
'-          ;; => -

;; Quoted values (not just symbols)
'#t         ;; => #t
'42         ;; => 42

;; Nested quote
''x         ;; => (quote x)

;; Identifiers with special characters
(define !x 1)
!x          ;; => 1
(define $y 2)
$y          ;; => 2
(define a->b 3)
a->b        ;; => 3
(define my-var? 4)
my-var?     ;; => 4

;; Dot pair in quote
'(a . b)         ;; => (a . b)
'(a b . c)       ;; => (a b . c)
'(a . (b . (c . ()))) ;; => (a b c)

;; Identifiers that look numeric but aren't
;; (these start with digit or sign but contain non-digit chars)
;; Note: exact rules depend on implementation of "exclude numeric tokens"
