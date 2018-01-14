#lang sicp

; ex-1.17
(define (double x) (* x 2))
(define (halve x)
  (if (even? x)
      (/ x 2)
      (error "can't halve odd " x)))

(define (mul2 a b)
  (define (*-double-halve a b)
    (mul2 (double a) (halve b)))
  (cond
    ((= b 0) 0)
    ((even? b) (*-double-halve a b))
    (else (+ a (*-double-halve a (- b 1))))))

(mul2 999 1001)

; ex-1.18

(define (mul3 x y)  
  (define (iter a b m)
    ; a + b*m = xy
    (cond
      ((= b 0) a)
      ((even? b) (iter a (halve b) (double m)))
      (else (iter (+ a m) (halve (- b 1)) (double m)))))
  (iter 0 x y))

(mul3 999 1001)