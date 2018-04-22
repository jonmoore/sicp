#lang sicp

(#%require racket rackunit "double.rkt")

(module+ test
  (test-case
   "check twice 2 is 4"
   (check = (double 2) 4)
   (check = (quad 3) 12)))

(define (quad x)
  (double (double x)))

(module+ test
  (test-case
   "check 4 time 3 is 12"
   (check = (quad 3) 12)))

(module+ main
  (printf "4 times 3 is ~a" (quad 3)))

;  
;  
;  
;  
