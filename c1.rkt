#lang sicp

;; ex-1.1

(define a 3)
(define b (+ a 1 ))
(+ a b (* a b))
(= a b)
(if (and (> b a ) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

;; ex-1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6  2) (- 2 7)))

;; ex 1.3

(define (sum-larger-two a b c)
  (cond
    ((<= a b c) (+ b c))
    ((<= b c a) (+ c a))
    ((<= c a b) (+ a b))
    (else (error "wtf"))))

(sum-larger-two 2 3 5)
(sum-larger-two 2 3 3)
(sum-larger-two 2 3 "4")

