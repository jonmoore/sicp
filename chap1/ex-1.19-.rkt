#lang sicp

; ex-1.19

; T <->
; q+p q
; q   p
; T*T <->
; (q+p)(q+p)+q^2 (q+p)q+qp
; q(q+p)+pq       q^2 +p^2
; q' = q^2+2pq
; p' = q^2+p^2

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
;  (display (list a b p q count))
  (cond ((= count 0) b)
        ((even? count) (fib-iter a
                                 b
                                 (+ (* q q) (* p p))
                                 (+ (* q q) (* 2 p q))
                                 (/ count 2)
                                 ))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(map fib (list 1 2 3 4 5 6 7 8 9 10))

; ex-1.20
; (gcd 206 40) 0
; (gcd 40 6)   1
; (gcd 6 4)    2  
; (gcd 4 2)    3
; ...
;
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
; Normal Order
; ============
; without drawing out the shape we see that each iteration the
; a parameter gets replaced by the b and the b by (remainder a b).
; This introduces new calls to remainder that need to be evaluated
; inside each call we evaluate the predicate and then one of the consequent
; or the alternative
; let the number of remainders present in each term be x_n where
; x_0 = a, x_1 = b
; x_n+2 = x_n+1 + x_n + 1
; Solve like a diff equation.  Inhomogeneous solution: i_n = -1
; x_n = h_n + i_n => h_n = x_n + 1
; h_n+2 = h_n+1 +h_n
; x_0, x_1 = 0, 0 => h_0, h_1 = Fib(n)
; h_n = 0:1 1:1 2:2 3:3 4:5 5:8 6:13 7:21 etc
; x_n =   0   0   1   2   4   7   11
; sequence thus runs at each level
; 0: x_1 F1-1 = 0 40
; 1: x_2 F2-1 = 1 (r 206 40)
; 2:            2 (r  40  6)
; 3:            4 (r   6  4)
; 4: x_4        7 (r   4  2)
;               4 a
; 18 evaluations of remainder in total, assuming remainder is a primitive
; that does not apply normal evaluation internally.
;
; Applicative Order
; =================
; Will need 4 calls to remainder since (r 4 2) is the last call made.

