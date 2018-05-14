#lang sicp
(#%require (only racket module+ format) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

;; ex-1.19

;; T <->
;; q+p q
;; q   p
;; T*T <->
;; (q+p)(q+p)+q^2 (q+p)q+qp
;; q(q+p)+pq       q^2 +p^2
;; q' = q^2+2pq
;; p' = q^2+p^2

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  ;;  (display (list a b p q count))
  (cond ((= count 0) b)
        ((even? count) (fib-iter a
                                 b
                                 (+ (* q q) (* p p))
                                 (+ (* q q) (* 2 p q))
                                 (/ count 2)
                                 ))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(module+ main
  (begin-example "1.19")
  (display (map fib (iota 10))))

(module+ test
  (test-case
   "fib correctness"
   (let* ((num-els 10)
          (fibs (map fib (iota num-els))))
     (for-each
      (lambda (n)
        (check-equal? (list-ref fibs (+ n 2))
                      (+ (list-ref fibs n)
                         (list-ref fibs (+ n 1)))))
      (iota (- num-els 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generic version of the algorithms we have been looking at

(define (halve x)
  (cond ((even? x) (/ x 2))
        (else 
         (error "can't halve odd " x))))

(#%provide make-fast-combiner-iter)
(define (make-fast-combiner-iter double combine init)
  (define (iter term n a)
    (cond
     ((= n 0) a)
     ((odd? n) (iter term
                     (dec n)
                     (combine term a)))
     ((even? n) (iter (double term)
                      (halve n)
                      a))
     (else (error "unreachable"))))
  (lambda (term n)
    (iter term n init)))

(define mul-
  (make-fast-combiner-iter (lambda (x) (+ x x)) + 0))

(define exp-
  (make-fast-combiner-iter (lambda (x) (* x x)) * 1))

(define (r*m r b)
  (map
   (lambda (col-b)
     (dot r col-b))
   (transpose b)))

(define (m*m a b)
  (map (lambda (r) (r*m r b)) a))

(define (m*v m v)
  (r*m v (transpose m)))

(define (m^2 m) (m*m m m))

(define (fib- n)
  (cadr
   ((make-fast-combiner-iter m^2 m*v '(1 0))
    '((1 1)(1 0))
    n)))

(module+ test
  (test-case
   "fib and fib- match v2"
   (let ((cases (iota 10)))
     (check-equal?
      (map fib cases)
      (map fib- cases)))))
