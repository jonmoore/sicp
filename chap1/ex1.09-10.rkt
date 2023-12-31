#lang sicp

(#%require (only racket module+) rackunit "../utils/sicp-utils.rkt")

;; ex 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8))
;; ...
;; (A 0 [9 times] (A 1 1))
;; 2^10

;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; 2^2^2^2
;; 2^16

;; (A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 2^2) - see above

(module+ main
  (begin-example "1.10")
  (display (list "(A 1 10) " (A 1 10))) (newline)
  (display (list "(A 2 4) " (A 2 4))) (newline)
  (display (list "(A 3 3) " (A 3 3))) (newline))

(module+ test

  (test-equal? "(A 1 10)" (A 1 10) (expt 2 10))
  (test-equal? "(A 2 4)" (A 2 4) (expt 2 16))
  (test-equal? "(A 3 3)" (A 3 3) (expt 2 16))
  
  (test-case
   "test (A 0 n)"
   (define (f n) (A 0 n)) ;; 2 * n
   (for-each
    (lambda (n)
      (check-equal? (f n) (* 2 n)))
    (list 1 2 3 4)))

  (test-case
   "test (A 1 n)"
   (define (g n) (A 1 n))
   (define (g2 n) (expt 2 n))
   (for-each
    (lambda (n)
      (check-equal? (g n) (g2 n)))
    (list 1 2 3 4)))

  ;; (A 2 n)
  ;; = (A 1 (A 2 n-1))
  ;; = 2^(A 2 n-1) ;; above (1)
  ;;
  ;; (A 2 1) = 2  ;; (2)
  ;;
  ;; (1,2) => 
  ;; (A 2 n) = 2^2..^2 (n 2s)
  (test-case
   "test (A 2 n)"
   (define (h n) (A 2 n))
   (define (h2 n) (if (= n 1) 2 (expt 2 (h2 (- n 1)))))
   (for-each
    (lambda (n)
      (check-equal? (h n) (h2 n)))
    (list 1 2 3 4))))
