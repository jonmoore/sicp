#lang sicp
(#%require (only racket module+ format) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

;; Exercise 1.37:

(define (cont-frac-combiner n d)
  (lambda (i accum)(/ (n i) (+ (d i) accum))))

(define (cont-frac n d k)
  (fold-right
   (cont-frac-combiner n d)
   0
   (iota k 1)))

(define (phi-calc cont-frac)
  (lambda (k)
    (cont-frac (constant 1.0) (constant 1.0) k)))

(module+ main
  (begin-example "1.37")
  (displn "Recursive calculation of 1/phi with cont-frac, 10 terms")
  (displn  ((phi-calc cont-frac) 10)))

(#%provide search-int)
(define (search-int f a b)
  ;; Search for an adjacent pair of integers in [a,b] for which
  ;; f has different values
  (define (equiv-int x y)
    (equal? (f x) (f y)))
  (define (iter a b)
    (let ((midpoint
           (+ a (quotient (- b a) 2))))
      (cond
       ((= (- b a) 1)
        (list a b))
       ((equiv-int a midpoint)
        (iter midpoint b))
       (else
        (iter a midpoint)))))
  (if (equiv-int a b)
      (error "f must differ at a and b"))
  (iter a b))

(define invphi (/ (dec (sqrt 5)) 2.0))

(define (invk-error k)
  (abs (- invphi ((phi-calc cont-frac) k))))

(module+ main
  (displn "Accuracy of cont-frac for calculating invphi")
  (displn (map (lambda (k) (list k (invk-error k)))
               (iota 10 2)))
  (displn "Number of iterations needed for error < 1e-4")

  (define (invk-error-in-target? k)
    (positive? (- (invk-error k) 1e-4)))
  (displn
   (cadr
    (search-int invk-error-in-target? 2 32))))

(define (cont-frac-iter n d k)
  (define (iter i accum)
    (if (= i 0)
        accum
        (iter (dec i)
              ((cont-frac-combiner n d) i accum))))
  (iter k 0))

(module+ main
  (displn "Iterative calculation of 1/phi with cont-frac-iter, 10 terms")
  (displn ((phi-calc cont-frac-iter) 10)))

(define (exp-d-term i)
  ;; 1,2,3,... -> 1,2,1,1,4,1,1,6,1,...
  (if (= (remainder i 3) 2)
      (* 2 (inc (quotient i 3)))
      1))

(module+ main
  (begin-example "1.38")
  (displn "Behavior of exp-d-term")
  (displn (map exp-d-term (iota 24 1))))

(define e (exp 1))

(define (exp-calc cont-frac)
  (lambda (k)
    (+ 2 (cont-frac (constant 1.0) exp-d-term k))))

(module+ main
  (newline)
  (displn "Behavior of continued fraction calculation of e")
  (let* ((values (map (exp-calc cont-frac) (iota 10 1)))
         (errors (map (lambda (val) (- val e)) values)))
    (display "values: ") (displn values)
    (display "errors: ") (displn errors)))


(define (tan-cf x k)
  (define (n i) (if (= i 1) x (- (* x x))))
  (define (d i) (dec (* 2 i)))
  (cont-frac n d k))

(module+ main
  (begin-example "1.39")
  (let* ((x 1.0)
         (values (map (lambda (k) (tan-cf x k)) (iota 6 1)))
         (errors (map (lambda (val) (- val (tan x))) values)))
    (display "values: ") (displn values)
    (display "errors: ") (displn errors)))
    
