#lang sicp

; ex-1.15
(define (cube x)
  (* x x x))
(define (p x)
  (display "called p")
  (newline)
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(sine 12.14)

; log(abs(a)) in both space and time

