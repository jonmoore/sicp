#lang sicp
(#%require (only racket module+ format) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

;; ex-1.17
(define (double x) (* x 2))
(define (halve x)
  (if (even? x)
      (/ x 2)
      (error "can't halve odd " x)))

(define (fast-mul a b)
  (define (iter a b)
    (fast-mul (double a) (halve b)))
  (cond
   ((= b 0) 0)
   ((even? b) (iter a b))
   (else (+ a (iter a (- b 1))))))

;; ex-1.18
(define (fast-mul-iter x y)  
  (define (iter a b m)
    ;; a + b*m = xy
    (cond
     ((= b 0) a)
     ((even? b) (iter a (halve b) (double m)))
     (else (iter (+ a m) (halve (- b 1)) (double m)))))
  (iter 0 x y))

(module+ main
  (begin-example "1.17")
  (displn (format "999*1001=~a" (fast-mul 999 1001))))

(module+ main
  (begin-example "1.18")
  (displn (format "999*1001=~a" (fast-mul-iter 999 1001))))

(module+ test
  (test-case
   "Matches built-in *"
   (for-each
    (lambda (x)
      (for-each
       (lambda (y)
         (check-equal? (* x y) (fast-mul x y))
         (check-equal? (* x y) (fast-mul-iter x y))
         )
       (iota 5)))
    (iota 5))))
