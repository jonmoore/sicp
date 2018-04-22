#lang sicp

(#%require "utils.rkt")
(#%require "plot-utils.rkt")

; ex 1.21
(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor next)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (next test-divisor)
               next))))

(define (smallest-divisor n)
  (find-divisor n 2 inc))

; ex 1.22
(define (prime?sd n)
  (= n (smallest-divisor n)))

; for 1.23
(define (next-opt n)
  (if (= n 2) 3 (+ n 2)))
(define (prime?sd2 n)
  (= n (find-divisor
        n 2 next-opt)))

; ex 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))
(define (make-fast-prime? times)
  (lambda (n) (fast-prime? n times)))

; ex 1.27
;; (define (full-fermat n)
;;   (fold-left (lambda (x y) (and x y))
;;              #t
;;              (map (lambda (a)
;;                     (= (expmod a n n) a))
;;                   (from-to 0 n))))

(define (full-fermat n)
  ;; Below, every should behave the same as Scheme's
  (every (lambda (a)
           (= (expmod a n n) a))
         (from-to 0 n)))

(define (start-timed-prime-test n start-time prime?)
  (if (prime? n)
      (list n (- (runtime) start-time))
      #f))

(define (make-timed-test prime?)
  (lambda (n)
    (start-timed-prime-test n (runtime) prime?)))

(define (results-n-primes-larger-than-x n x prime?)
  (if (even? x)
      (error "x must be odd.  Received" x))
  (define (iter n x)
    (cond
      ((= n 0)
       nil)
      (else
       (let ((result (prime? (+ x 2))))
         (if result
             (cons result
                   (iter (- n 1) (+ x 2)))
             (iter n (+ x 2)))))))
  (iter n x))

(define (n-primes-larger-than-x n x prime? get-result)
  (map get-result
       (results-n-primes-larger-than-x n x prime?)))

(define (prime-timings base powers num-primes prime?)
  (map
   (lambda (n)
     (list
      n
      (average
       (map cadr
            (results-n-primes-larger-than-x
             num-primes
             n
             (make-timed-test prime?))))))
   (map (lambda (power)
          (+ 1 (expt base power)))
        powers)))

(define (plot-prime-timings base powers num-primes prime?)
  (plot-points-loglog
   base
   (prime-timings base powers num-primes prime?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map
 smallest-divisor
 (list 199 1999 19999))

(map (make-timed-test prime?sd)
     (map (lambda (power)
            (- (* 2 (expt 10 power)) 1))
          (from-to 2 12)))

(define (transform-test test transformer)
  (lambda (n)
    (let ((test-n (test n)))
      (if test-n
          (transformer n test-n)
          #f))))

(define (with-n n test-n)
  (list n test-n))

(define (just-n n test-n)
  n)

(results-n-primes-larger-than-x
 3 999
 (transform-test prime?sd with-n))

(results-n-primes-larger-than-x
 3 999
 (transform-test prime?sd just-n))

(plot-prime-timings 10 (from-to 4 12) 3 prime?sd)
(map cadr (prime-timings 10 (from-to 4 12) 16 prime?sd))
; ex 1.23 - pretty close to half the time, esp. for large primes, but
; with noise
(map cadr (prime-timings 10 (from-to 4 12) 16 prime?sd2))

; ex 1.24
; sample with lots of primes (500 below).  We're searchign 500 primes
; forward which biases the results.  For a cleaner test we could
; sample forward randomly in a range and also count the times
; for testing non-primes.  Our test uses a per-test timing so it's
; not biased by the density of primes.  restrict the tested exponents
; because the built in random function has limited domain
(plot-points 
 (map (lambda (xy) (list (log10 (car xy)) (cadr xy)))
      (prime-timings 10 (from-to 4 9) 500 (make-fast-prime? 100))))

; ex 1.25 Alyssa is wrong because the proposed way will require
; keeping track of potentially huge integers to run correctly which
; will probably be slower (at least by a factor of log n)

; ex 1.26 The cost function will obey C(exp) ~=  2 * C(exp/2), i.e.
; linear in exp

; ex 1.27
(map full-fermat
       '(561 1105 1729 2465 2821 6601))

; ex 1.28

;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder 
;;           (square (expmod base (/ exp 2) m))
;;           m))
;;         (else
;;          (remainder 
;;           (* base (expmod base (- exp 1) m))
;;           m))))


(define (non-trival-square-root-of-one? candidate-square-root m)
  (and (= (remainder (square candidate-square-root) m) 1)
       (not (= candidate-square-root 1))
       (not (= candidate-square-root (- m 1)))))

(define (expmod-sig base exp m)
  "Raises base to the power of exp modulo m.  Version of expmod that
will signal if it discovers a non-trivial square root of 1"

  (display (list base exp m))
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((to-square (expmod-sig base (/ exp 2) m)))
           (if (non-trival-square-root-of-one? to-square m)
               0 ;; signal
               (remainder (square to-square) m))))
        ((odd? exp)
         (remainder 
          (* base (expmod-sig base (- exp 1) m))
          m))
        (else (error "Should not reach this"))))

(define (miller-rabin-check a n)
  "A check used in Miller-Rabin primality tests.  Returns #t if
testing with a is consistent with n being prime."
  (= 1 (expmod-sig a (- n 1) n)))

(define (full-miller-rabin-test n)
  "Miller-Rabin primality test.  "
  (every (lambda (a)
           (miller-rabin-check a n))
         (from-to 0 n)))

(define (repeated-check check num-checks)
  "Runs check up to num-checks times. Returns #f early if any call of
check returns false, otherwise (i.e. all num-checks calls return a
true value) returns #t."
  (cond ((= num-checks 0) #t)
        ((not (check)) #f)
        (else (repeated-check check (- num-checks 1)))))

(define (fast-miller-rabin-test n times)
  "Miller-Rabin primality test.  "
  (repeated-check
   (lambda ()
     (miller-rabin-check
      (+1 (random (- n 1))) n))
   times))