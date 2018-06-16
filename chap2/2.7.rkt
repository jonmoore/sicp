#lang sicp
(#%require (only racket module+) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

;; Extended exercise on interval arithmetic

(module+ main
  (begin-example "2.8"))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;; Exercise 2.8

(define (sub-interval x y)
  (make-interval
   (- (lower-bound x) (upper-bound y))
   (- (upper-bound x) (lower-bound y))))

(define (add-interval x y)
  (make-interval
   (+ (lower-bound x) (lower-bound y))
   (+ (upper-bound x) (upper-bound y))))

;; Exercise 2.9

(define (width x)
  (- (upper-bound x) (lower-bound x)))

;; Then
;; (width (sub-interval x y))
;; =  (- (upper-bound (sub-interval x y)) (lower-bound (sub-interval x y)))
;; =  (ubx-lby)-(lbx-uby)
;; =  ubx -lby -lbx + uby
;; =  wx + wy
;;
;; Similarly for addition

;; multiplication
;; with all+ve quantities
;; width of (a a+w) * (b b+z)
;; = (a+w)*(b+z)-ab
;; = wb+az+wz
;; so not a function of w and z only, and a,b,w,z are all independent
;; Specific examples
;; a=b=0 w=z=1 -> (0,1) (0,1) -> 1
;; a=0, b=1 w=z=1 -> (0,1) (1,2) -> 2


(module+ main
  (begin-example "2.10"))

(define (spans-zero y)
  (and (> (upper-bound y) 0)
       (< (lower-bound y) 0)))

(define (div-interval x y)
  (if (spans-zero y)
      (error "Cannot divide by interval that spans zero")
      (mul-interval x
                    (make-interval
                     (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval
     (min p1 p2 p3 p4)
     (max p1 p2 p3 p4))))

(module+ main
  (begin-example "2.11"))

(define (mul-interval-ben x y)
  (define (minus x)
    (make-interval (- (upper-bound x)) (- (lower-bound x))))
  (define (sign x)
    (cond ((< 0 (lower-bound x)) 1)
          ((< (upper-bound x) 0) -1)
          (else 0)))
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y))
        (sx (sign x))
        (sy (sign x)))
    (cond ((= sx -1) (minus (mul-interval-ben (minus x) y)))
          ((= sy -1) (minus (mul-interval-ben x (minus y))))
          ((and (= sx 1) (= sy 1)) (make-interval (* lx ly) (* ux uy)))
          ((and (= sx 0) (= sy 0)) (mul-interval x y))
          ((and (= sx 0) (= sy 1)) (make-interval (* lx uy) (* ux uy)))
          ((and (= sy 0) (= sx 1)) (make-interval (* ly ux) (* uy ux)))
          (else (error "Logic error - should not be reached")))))

(module+ main
  (begin-example "2.12"))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i) (/ (+ (lower-bound i) (upper-bound i))
                      2))

(define (half-width i) (/ (width i) 2))

(define (make-center-percent c p)
  (make-center-width c (* p 0.01 c)))

(define (percent i)
  (* 100 (/ (half-width i) (center i))))

;; 2.13

;; At lower bound (c-pc)(d-qd) = cd(1-p)(1-q) ~= cd(1-(p+q)) for small p,q
;; Likewise at upper bound, so tolerances are additive

;; 2.14

;; r1 r2 / (r1+r2) vs 1/(1/r1+1/r2)

(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval
      (div-interval one r1)
      (div-interval one r2)))))

(module+ main
  (begin-example "2.14")
  ;; Lem is indeed right - examples below

  (newline)
  (displn "testing two forms for parallel resistor")
  (let ((r1 (make-center-percent 3 1))
        (r2 (make-center-percent 3 1)))
    (for-each
     (lambda (calc)
       (let ((result (calc r1 r2)))
         (displn (list r1 r2))
         (displn result)
         (displn (percent result))
         (newline)))
     (list par1 par2)))
  (define one (make-interval 1 1))
  
  (define (compare calcs arg-list)
    (for-each
     (lambda (calc)
       (let ((result (apply calc arg-list)))
         (displn arg-list)
         (displn result)
         (displn (percent result))
         (newline)))
     calcs))
  
  (newline)
  (displn "testing A/A")
  (for-each
   (lambda (arg-list)
       (compare (list (lambda (x) one)  (lambda (x) (div-interval x x))) arg-list))
   (list (list one)
         (list (make-center-percent 3 1))))
  
  (newline)
  (displn "testing A/B")
  (for-each
   (lambda (arg-list)
     (compare (list (lambda (a b) (div-interval a b))) arg-list))
   (list (list one one)
         (list (make-center-percent 1 1) (make-center-percent 1 2)))))


;; Ex 2.15

;; Eva is right.  For a function with a set of independent parameters,
;; each with an interval of uncertainty, the interval for the results
;; is the interval including the union of possible results from
;; sampling points in the input space.  Adding duplicates of the input
;; parameters is equivalent to looking at the uncertainty in results
;; of a function of more inputs, and with given original input
;; uncertainties the results of the new function will be a supperset
;; of the results of the original function.

;; 2.16

;; The explanation above is part of the difficulty of writing an
;; interval arithmetic package.  It is straightforward to rewrite
;; expressions so that the inputs are mentioned only once but the
;; difficulty of tracking the intervals for outputs while accounting
;; for correlations in intermediate results to avoid being unduly
;; pessimistic remains.  Also, many functions do not attain their
;; extrema at points corresponding to ends of intervals (e.g. division
;; - we coud legitimately have taken the output interval for
;; denominators crossing zero to be the real line).
;;
;; A simplifying assumption is that the functions are locally linear,
;; in which case we can use the techniques from automatic
;; differentiation to calculate local gradients, hence intervals,
;; quickly and precisely.  This could also be extended to allow for
;; errors introduced by floating-point rounding errors.
