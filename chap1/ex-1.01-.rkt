#lang sicp

(#%require racket rackunit)

;; ex-1.1
(module+ main
  (define a 3)
  (define b (+ a 1 ))

  (+ a b (* a b))
  (= a b)
  (if (and (> b a ) (< b (* a b)))
      b
      a)
  (cond ((= a 4) 6)
        ((= b 4) (+ 6 7 a))
        (else 25)))

;; ex-1.2
(module+ main
  (display 
   '(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
       (* 3 (- 6  2) (- 2 7))))
  (newline))

;; ex 1.3
(define (sum-larger-two-squares a b c)
  (define (square x) (* x x))
  (- (+ (square a) (square b) (square c))
     (square (min a b c))))

(module+ main
  (printf "(sum-larger-two-squares 2 3 5) ~a~n"
          (sum-larger-two-squares 2 3 5)))

(module+ test
  (check-= (sum-larger-two-squares 3 2 4) 5 1e-6)
  (check-= (sum-larger-two-squares 4 3 2) 25 1e-6)
  (check-= (sum-larger-two-squares 2 4 3) 25 1e-6)
  (check-= (sum-larger-two-squares 2 2 3) 13 1e-6)
  (check-= (sum-larger-two-squares 2 2 1) 8 1e-6)
  (check-= (sum-larger-two-squares 2 2 2) 8 1e-6))
