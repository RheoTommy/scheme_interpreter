;;; Floating-point numbers
;;; Requires: required Mini-Scheme features + decimal numbers

1.5                         ;; => 1.5
-2.5                        ;; => -2.5
(+ 1.5 2)                   ;; => 3.5
(- 5.0 2)                   ;; => 3.0
(* 1.5 2)                   ;; => 3.0
(/ 10 2)                    ;; => 5
(/ 10 4)                    ;; => 5/2
(/ 10.0 2)                  ;; => 5.0
(/ 10.0 4)                  ;; => 2.5
(+ 1/2 1/3)                 ;; => 5/6
(= 1 1.0)                   ;; => #t
(< -2.5 1.0)                ;; => #t
(number? -2.5)              ;; => #t
(number->string 1.5)        ;; => "1.5"
(number->string 1/2)        ;; => "1/2"
(string->number "-2.5")     ;; => -2.5
(string->number "1/2")      ;; => 1/2
