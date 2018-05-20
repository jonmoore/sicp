#lang sicp
(#%require (only racket module+ format) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

;; Exercise 1.34: Suppose we define the procedure

;; (define (f g) (g 2))

;; What happens if we (perversely) ask the interpreter to evaluate the
;; combination (f f)? Explain.
;;
;; (f f) -> (f 2) -> (2 2) -> error

(define tolerance 1e-5)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point-ex f guess test)
  (define (iter guess)
    (let ((next (f guess)))
      (if (test guess next)
          next
          (iter next))))
  (iter guess))

(define (fixed-point f guess)
  (fixed-point-ex f guess close-enough?))

(define phi (fixed-point (lambda (x) (inc (/ 1.0 x))) 1.0))

(module+ main
  (begin-example "1.35")
  (displn "phi from fixed point of x -> 1 + 1/x")
  (displn phi)
  (displn "phi from (1+sqrt(5))/2")
  (displn (/ (inc (sqrt 5)) 2.0)))

(define (fixed-point-display f guess)
  (define (f-displaying x)
    (displn x)
    (f x))
  (fixed-point f-displaying guess))

(define guess-value car)
(define guess-count cadr)
(define (fixed-point-counting f guess)
  (define (test guess1 guess2)
    (close-enough? (guess-value guess1) (guess-value guess2)))
  (define (f-counting guess)
    (list (f (guess-value guess)) (inc (guess-count guess))))
  (fixed-point-ex f-counting (list guess 0) test))

(define (average-damp f damping)
  (define (weighted-average a b w)
    (+ (* w a) (* (- 1.0 w) b)))
  (lambda (x)
    (weighted-average x (f x) damping)))

(module+ main
  (begin-example "1.36")
  (displn "Solving x^x=1000")
  (define (log-base-x y)
    (lambda (x)
      (/ (log y) (log x))))
  
  (displn "Without damping")
  (fixed-point-display (log-base-x 1000) 10)
  (displn (fixed-point-counting (log-base-x 1000) 10))

  (displn "With damping 0.5")
  (let ((log-base-x-damped (average-damp (log-base-x 1000) .5)))
    (fixed-point-display log-base-x-damped  10)
    (displn (fixed-point-counting log-base-x-damped 10)))


  (displn "Performance by damping in range 0.0 0.9")
  (define (steps-for-fixed-point f guess)
    (lambda (damping)
      (guess-count (fixed-point-counting (average-damp f damping) guess))))

  (let ((dampings (iota 10 0 .1)))
    (for-each displn
              (zip dampings
                   (map 
                    (steps-for-fixed-point (log-base-x 1000) 10)
                    dampings)))))
