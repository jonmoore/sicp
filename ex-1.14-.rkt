#lang sicp

(#%require "utils.rkt")
(#%require plot)

; ex-1.14
;; from the text
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else 
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination 
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc-datum-amount cc-datum) (car cc-datum))
(define (cc-datum-count cc-datum) (cadr cc-datum))
(define (cc-datum-time cc-datum) (caddr cc-datum))

(define (cc-datum-timing cc-datum)
  (list (cc-datum-amount cc-datum)
        (cc-datum-time cc-datum)))

(define (cc-datum-counting cc-datum)
  (list (cc-datum-amount cc-datum)
        (cc-datum-count cc-datum)))

(define (cc-test-amount cc kinds amount)
  (let ((timer (make-timer)))
    (let ((count (cc amount kinds)))
      (list amount count (timer)))))

(define (cc-test-amounts cc kinds amounts)
  (map (λ (amount) (cc-test-amount cc kinds amount))
       amounts))

(define (timings cc-data)
  (map cc-datum-timing
       (filter (λ (datum) (> (cc-datum-time datum) 1e-4))
               cc-data)))

(define (map-tree f tree)
  (if (atom? tree)
      (f tree)
      (map (λ (t) (map-tree f t))
           tree)))

(define (make-amounts from to)
  (map
   (λ (x) (floor (exp2 x)))
   (from-to-by from to 0.25)))

(define (plot-data cc-data)
  (plot (points (map-tree log2 cc-data))))

(define sample-data
  (cc-test-amounts cc 3
                   (make-amounts 7 10.5)))
(plot-data
 (timings
  sample-data))

(plot-data
 (map cc-datum-counting
      sample-data))


; time required is O(amount^#kinds);
; change count is O(amount^(#kinds-1))

; how to do this efficiently (iteratively)?
; one way is to memoize cc, with storage requirement ~=
; #kinds * amount
; storage could be optimized by not retaining values that will
; never be looked up again, giving a requirement
; #kinds * largest-denomination
; time requirements in both cases are O(1) per additional value
; computed (notably efficient) so #kinds * amount in total