#lang sicp

;; https://stackoverflow.com/questions/9347294/mcons-in-dr-racket
(#%require r5rs/init)


;; ex 1.7

(define (is-small-fraction? change guess)
  (define small-fraction 1e-4)
  (<= (abs change) (* small-fraction (abs guess))))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (improve guess)
    (display guess)
    (average guess (/ x guess)))
  (define (good-enough? old-guess guess)
    (is-small-fraction?
     (- guess old-guess)
     guess))
  (define (sqrt-iter old-guess guess)
    (if (good-enough? old-guess guess)
        guess
        (sqrt-iter guess (improve guess))))
  (if (< x 0)
      (error "can't find sqrt of negative input" x)
      (sqrt-iter 100 1.0)))

;; ex-1.8
(define (cubert x)
  (define (improve y)
    (display y) (newline)
    (/
     (+ (/ x y y) (* 2 y))
     3))
  (define (good-enough? old-guess guess)
    (is-small-fraction?
     (- guess old-guess)
     guess))
  (define (iter old-guess guess)
    (if (good-enough? old-guess guess)
        guess
        (iter guess (improve guess))))
  (iter 100 1.0))

(map cubert (list -64))