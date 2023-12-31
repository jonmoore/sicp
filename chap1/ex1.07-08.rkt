#lang sicp
(#%require (only racket module+) rackunit "../utils/sicp-utils.rkt")

;; ex 1.7
(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter old-guess)
    (define new-guess (improve old-guess))
    (define rel-tol 1e-6)
    (define abs-tol 1e-18)
    (if (close-in-joint-error-or? old-guess new-guess rel-tol abs-tol)
        new-guess
        (sqrt-iter new-guess)))
  (if (< x 0)
      (error "can't find sqrt of negative input" x)
      (sqrt-iter 1.0)))

(define (square x)
  (* x x))

(module+ main
  (begin-example "1.7")
  (for-each
   (lambda (x)
     (display
      (list
       "sqrt of" x ":" (sqrt x)
       "relerr"  (/ (- (square (sqrt x)) x) x)
       ))
     (newline))
   '(1 2 3 4 100 1024 65536 1e6 1e12 1e24 1.1e24)))

(module+ test
  (test-case
   "test sqrt on positive numbers"
   (for-each
    (lambda (x)
      (check-= (square (sqrt x)) x 1e-6))
    (list 2 4 6 9)))
  (test-case
   "test sqrt of zero"
   (check-= (sqrt 0) 0 1e-6)))

;; ex-1.8
(define (cube x)
  (* x x x))

(define (cubert x)
  (define (improve y)
    (/
     (+ (/ x y y) (* 2 y))
     3))
  (define (cubert-iter guess)
    (define rel-tol 1e-6)
    (define abs-tol 1e-24)
    (if (close-in-joint-error-or? (cube guess) x abs-tol rel-tol)
        guess
        (cubert-iter (improve guess))))
  (cubert-iter (* x 1.0)))

(module+ main
  (begin-example "1.8")
  (for-each
   (lambda (x)
     (display
      (list
       "cubert of" x ":" (cubert x)
       "relerr"  (rel-error (cube (cubert x)) x)
       ))
     (newline))
   '( 1 2 3 4 100 1024 65536 1e6 1e12 1e24
        -1 -2 -3 -4 -100 -1024 -65536 -1e6 -1e12 -1e24)))

(module+ test
  (define test-nums
    (list 1 2 3 4 100 1024 65536 1e6 1e12 1e24))
  (define test-tol 1e-6)
  (test-case
   "test sqrt of zero"
   (check-= (sqrt 0) 0 test-tol))
  (test-case
   "test cubert on non-zero numbers"
   (for-each
    (lambda (x)
      (check-= (/ (cube (cubert x)) x) 1 test-tol)
      (check-= (/ (cube (cubert (- x))) (- x)) 1 test-tol))
    test-nums)))

(#%provide cubert)
 
