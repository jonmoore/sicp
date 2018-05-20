#lang sicp
(#%require (only racket module+ format) rackunit)
(#%require
 "../utils/sicp-utils.rkt"
 "../utils/srfi-1.rkt"
 "ex1.21.rkt"
 )

(define (sum-recur term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-recur term (next a) next b))))

(define (simp f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (* (/ h 3.0)
     (sum-recur
      (lambda (k)
        (+ (y k) (* 4 (y (+ k 1))) (y (+ k 2))))
      0
      (lambda (k) (+ k 2))
      (dec n))))

(module+ main
  (begin-example "1.29")

  (define (simp-report f a b ns)
    (map
     (lambda (n)
       (simp f a b n))
     ns))
  (define (exp2 x) (expt 2 x))
  (define (log2 x) (log x 2))

  (displn (simp-report cos 0.0 1.0 (map exp2 (iota 5 5))))

  (define (exact-int intf a b) (- (intf b) (intf a)))
  
  (define (simp-error-report intf f a b ns)
    (define (simp-error n)
      (- (simp f a b n)
         (exact-int intf a b)))
    (map-tree log2 (zip ns (map simp-error ns))))

  (define (display-simp-report report)
    (displn "Error is O(h^4) so change in (log2 err) is -4*change in (log2 n)")
    (displn report)
    (displn (transpose (map diff (transpose report)))))    

  (display-simp-report
   (simp-error-report sin cos 0.0 1.0 (map exp2 (iota 5 1))))

  (display-simp-report 
   (simp-error-report tan (lambda (x) (/ 1.0 (expt (cos x) 2)))
                      0.0 1.0 (map exp2 (iota 5 1)))))


(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))
  
(module+ main
  (begin-example "1.30")
  (displn "  (sum-iter square 0 inc 3))")
  (sum-iter square 0 inc 3))
  
(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recur term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product-iter values 1 inc n))

(define (pi-wallis n-max)
  (* 4.0
     (product-iter
      (lambda (n) (/ (* (inc n) (dec n)) (square n) 1.0))
      3
      (lambda (n) (+ n 2))
      n-max)))

(define pi (* 4 (atan 1)))

(module+ main
  (begin-example "1.31")
  (displn "(factorial 3)")
  (displn (factorial 3))
  (displn "approximating pi")
  
  (for-each
   (lambda (n)
     (let* ((approx (pi-wallis n))
            (err (- approx pi))
            (nerr (* n err)))
     (displn (list n approx err nerr))))
   (iota 10 3 2)))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-accumulate term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-accumulate term a next b)
  (accumulate-iter * 1 term a next b))

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recur combiner null-value term (next a) next b))))

(module+ main
  (begin-example "1.32")
  (displn "5! via factorial (product-iter)")
  (displn (factorial 5))
  (displn "5! via product-accumulate")
  (displn (product-accumulate values 1 inc 5))
  (displn "5! via accumulate-recur")
  (displn (accumulate-recur * 1 values 1 inc 5)))

(define (filtered-accumulate-recur filter combiner null-value term a next b)
  (define (recur a)
    (cond
     ((> a b) null-value)
     ((filter a) (combiner (term a) (recur (next a))))
     (else (recur (next a)))))
  (recur a))

(module+ main
  (begin-example "1.33")
  (displn "Sum of squares of primes in 2 12")
  (define (sum-of-squares-of-primes a b)
    (filtered-accumulate-recur
     prime? + 0 square a inc b))
  (displn (sum-of-squares-of-primes 2 12))
  (displn "check")
  (displn (sum (map square '(2 3 5 7 11))))

  (displn "Product of numbers in [1,n) co-prime to n for n=8: 3*5*7")
  (define (product-of-smaller-co-primes n)
    (define (co-prime-n? i) (= (gcd i n) 1))
    (filtered-accumulate-recur
     co-prime-n? * 1 values 1 inc (dec n)))
  (displn (product-of-smaller-co-primes 8)))
