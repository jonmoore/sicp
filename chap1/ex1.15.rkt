#lang sicp

(#%require racket/trace (only racket module+ format) plot rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")


;; ex-1.15
(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

;; basic version
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; more accurate version
(define (sine2 angle)
  (if (not (> (abs angle) 0.1))
      (+ angle
         (/ (cube angle) -6.0)
         (/ (expt angle 5) 120.0)
         )
      (p (sine2 (/ angle 3.0)))))

;; use sin at the core as a benchmark
(define (sine3 angle)
  (if (not (> (abs angle) 0.1))
      (sin angle)
      (p (sine3 (/ angle 3.0)))))

(define (trace-on)
  (trace p)
  (trace sine))

(define (trace-off)
  (untrace p)
  (untrace sine))

(module+ main
  (begin-example "1.15")
  (trace-on)
  (sine 12.14)
  (sine -50.0)
  (trace-off)
  ;; log(abs(a)) in both space and time

  (plot-new-window? #t)
  (plot (points
         (map
          (lambda (x)
            (list x (- (sine x) (sin x))))
          (iota 10000 -5000 1.0)))))

(module+ test
  (test-case
   "Check sine vs sin"
   (for-each
    (lambda (x)
      (check-= (sin x) (sine x) 5e-3 (format "x=~a" x)))
    (iota 1000 -5 0.01))))
