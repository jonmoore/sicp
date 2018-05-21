#lang sicp
(#%require (only racket module+ format exact-ceiling) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(#%require "ex1.34.rkt" "ex1.37.rkt")

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(module+ main
  (begin-example "1.40")

  (for-each
   (lambda (c)
     (let* ((f (cubic 1 1 c))
            (root (newtons-method f 1))
            (y-error (f root)))
       (display "c, root, y-error: ")
       (displn (list c root y-error))))
   (iota 5 0.0 .5)))

(define (double f)
  (lambda (x) (f (f x))))

;; (((double (double double)) inc) 5)

;; First (deliberately simplistic) approach -- expand mechanically.
;;
;; (double (double double))
;;         ^^^^^^^^^^^^^^^
;;         v                              v
;; (double (lambda (f) (double (double f))))
;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; v                                                                                      v
;; (lambda (x) ((lambda (f1) (double (double f1))) ((lambda (f2) (double (double f2))) x)))
;;                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                                                 v                 v
;; (lambda (x) ((lambda (f1) (double (double f1))) (double (double x))))
;;             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;             v                                   v
;; (lambda (x) (double (double (double (double x)))))
;; -> 2^4 applications

;; Specialized approach
;;
;; let (repeat n) be the function that, given f as an input, returns a
;; function that applies f n times.
;;
;; (double (repeat n))
;; -> (lambda (g) ((repeat n) ((repeat n) g)))
;; when applied to f, this returns a function that applies f n^2 times
;;
;; so (double (repeat n)) = (repeat (* n n))
;; (double double)
;; = (double (repeat 2)) ;; definition of double
;; = (repeat 4)
;;
;; (double (double double)) = (repeat 16) etc
;;
;; Similarly:
;; (double ((repeat n) f)) = ((repeat (* 2 n)) f)

(module+ main
  (begin-example "1.41")
  (display "((double inc) 3): ")
  (displn ((double inc) 3))
  (inc 0)
  ((double inc) 0)
  (((double double) inc) 0)
  (((double (double double)) inc) 0)
  (((double (double (double double))) inc) 0)
  ((double inc) 0)
  ((double (double inc)) 0)
  ((double (double (double inc))) 0)
  ((double (double (double (double inc)))) 0))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(module+ main
  (begin-example "1.42")
  (displn ((compose square inc) 6)))

(define (repeated f n)
  (if (= n 0)
      values
      (compose f (repeated f (dec n)))))

(define (repeat n)
  (lambda (f) (repeated f n)))

(module+ main
  (begin-example "1.43")
  (displn ((repeated square 2) 5))
  (displn (((repeat 2) square) 5))

  ;; (double ((repeat n) f)) = ((repeat (* 2 n)) f)
  (displn ((double ((repeat 5) inc)) 0))
  )

(define (smooth-dx dx)
  (lambda (f)
    (lambda (x)
      (/ (+ (f x) (f (- x dx)) (f (+ x dx)))
         3.0))))

(define (n-fold-smooth f dx n)
  ((repeated (smooth-dx dx) n) f))

(define (dirac-delta width)
  (lambda (x)
    (if (< (abs x) (* 0.5 width))
        (/ 1.0 width)
        0.0)))

(module+ main 
  (#%require plot)
  (#%require "../utils/plots.rkt")

  (plot-new-window? #t)

  (begin-example "1.44")
  (let* ((inv-h 10000)
         (h (/ 1.0 inv-h))
         (range 0.002)
         (n-samples (inc (* 2 range inv-h)))
         (xs (iota n-samples (- range) h))
         (dx-smooth 0.0001)
         (delta-width (* 1 dx-smooth))
         (smoothed-delta (lambda (n)
                           (n-fold-smooth (dirac-delta delta-width) dx-smooth n)))
         (n-smoothing '(1 2 4 8)))
    (plot
     (plottables->plottable
     (map
      (lambda (n)
        (lines
         (map
          (lambda (x)
            (list x  ((smoothed-delta n) x)))
          xs)))
      '(0 1 2 3 4 5 6 7))))))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (fixed-point-ex f guess test)
  (define (iter guess)
    (let ((next (f guess)))
      (if (test guess next)
          next
          (iter next))))
  (iter guess))

(define guess-value car)
(define guess-count cadr)

(define (fp-converged? v1 v2)
  (< (abs (- (guess-value v1) (guess-value v2))) 1e-6))
(define (fp-escaped? v2)
  (> (abs (guess-value v2)) 1e6))
(define (fp-count-exceeded? v2)
  (> (guess-count v2) 1000))

(define (damped-newton-nth-root-ex n dampings x guess)
  (define (f y)
    (/ x (expt y (- n 1))))

  (define (stop-iteration? v1 v2)
    (or (fp-converged? v1 v2)
        (fp-escaped? v2)
        (fp-count-exceeded? v2)))

  (define (damped-couting f)
    (lambda (counted-guess)
      (list (f (guess-value counted-guess)) (inc (guess-count counted-guess)))))

  (define (damped-f guess)
    (((repeated average-damp dampings) f) guess))
  
  (fixed-point-ex
   (damped-couting damped-f)
   (list guess 0)
   stop-iteration?))

(define (fp-success? result)
  (not (or (fp-escaped? result)
           (fp-count-exceeded? result))))

(define (damped-newton-nth-root-converges n dampings x guess)
  (fp-success? 
   (damped-newton-nth-root-ex n dampings x guess)))

(define (count-initial-falses lis)
  (define (iter lis count)
    (cond ((null? lis) count)
          ((car lis) count)
          (else (iter (cdr lis) (inc count)))))
  (iter lis 0))

(module+ main
  (begin-example "1.45")
  (displn "power : dampings required")
  (for-each
   (lambda (power)
     (display power)
     (display " : ")
     (displn
      (count-initial-falses
       (map
        (lambda (dampings)
          (let* ((x 1000)
                 (guess (inc (expt x (/ 1.0 power))))) ;; start near but not at true answer
            (damped-newton-nth-root-converges power dampings x guess)))
        (iota 8)))))
   (iota 33 1)))

(define (nth-root n x)
  (let* ((guess 1.0)
         (dampings (inc (exact-ceiling (log n 2))))
         (result (damped-newton-nth-root-ex n dampings x guess)))
    (cond ((fp-escaped? result) (error "escaped"))
          ((fp-count-exceeded? result) (error "count exceeded"))
          (else (guess-value result)))))

(define (nth-root-2 n x)
  (define (f y)
    (/ x (expt y (- n 1))))
  
  (define (damped-f guess)
    (let ((dampings (inc (exact-ceiling (log n 2)))))
      (((repeated average-damp dampings) f) guess)))
  
  (define (converged? v1 v2)
    (< (abs (- v1 v2)) 1e-6))
  
  (fixed-point-ex damped-f 1.0 converged?))

(module+ main
  (displn "Roots of 1000")
  (displn "(n nth root of 1000 - 2 versions)")
  (for-each
   (lambda (n)
     (displn (list n (nth-root n 1000.0) (nth-root-2 n 1000.0))))
   (iota 16 1)))


(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess) (average guess (/ x guess)))) 1.0))

(define (fixed-point-iterative-improve f first-guess)
  ;; simple version. we could also extend guess to hold the current
  ;; and previous guesses
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) 1e-6))
  (f ((iterative-improve
       good-enough?
       f)
      first-guess)))

(module+ main
  (begin-example "1.46")
  (displn "some square roots")
  (displn "x x^2 sqrt(x)")
  (for-each
   (lambda (x)
     (displn (list x (square x) (sqrt (square x)))))
   (iota 10 .5 .5))

  (newline)
  (let ((fp-cos (fixed-point-iterative-improve cos 0.0)))
    (display "fixed point of cos : ")
    (displn fp-cos)
    (display "cos of this : ")
    (displn (cos fp-cos))))

