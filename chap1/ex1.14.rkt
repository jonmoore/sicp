#lang sicp

(#%require (only racket module+) rackunit)
(#%require plot)
(#%require "../utils/srfi-1.rkt" "../utils/sicp-utils.rkt" "../utils/plots.rkt")

; ex-1.14
;; from the text
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

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

(define (count-change amount)
  (cc amount 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a protocol for "cc-run objects"
(define (make-cc-run amount count time)
  (list amount count time))

(define (cc-run-amount cc-run) (car cc-run))
(define (cc-run-count cc-run) (cadr cc-run))
(define (cc-run-time cc-run) (caddr cc-run))


(define (cc-run-timing cc-run)
  (list (cc-run-amount cc-run)
        (cc-run-time cc-run)))

(define (cc-run-counting cc-run)
  (list (cc-run-amount cc-run)
        (cc-run-count cc-run)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cc-perform-run cc kinds amount)
  (let ((timer (make-timer)))
    (let ((count (cc amount kinds)))
      (make-cc-run amount count (timer)))))

(define (cc-perform-runs cc kinds amounts)
  (map (lambda (amount)
         (cc-perform-run cc kinds amount))
       amounts))

(define (make-sample-amounts from to)
  (map
   (lambda (x) (floor (expt 2 x)))
   (iota (floor (/ (- to from) 1/4)) from 1/4)))

(define (cc-valid-timing? cc-datum min-time)
  (> (cc-run-time cc-datum) min-time))

;;(#%require racket/trace)
(define (map-tree f tree)
  (if (atom? tree)
      (f tree)
      (map (lambda (t) (map-tree f t))
           tree)))

(define (plot-cc-data cc-data)
  (define (log2 x)
    (log x 2))
  (plot (points (map-tree log2 cc-data))))

(module+ main
  (#%require (only racket displayln))

  (define sample-runs
    (let 
        ((sample-kinds 5)
         (sample-amounts (make-sample-amounts 7 9))
         (sample-min-time 1e-4))
      (filter (lambda (datum) (cc-valid-timing? datum sample-min-time))
              (cc-perform-runs cc sample-kinds sample-amounts))))

  (define sample-timings
    (map cc-run-timing sample-runs))
    
  (displayln sample-runs)
  (displayln sample-timings)

  (plot-new-window? #t)
  (cond
   ((> (length sample-timings) 1)
    (plot-points-loglog 2 sample-timings))))

;; time required is O(amount^#kinds);
;; change count is O(amount^(#kinds-1))

;; how to do this efficiently (iteratively)?
;; one way is to memoize cc, with storage requirement ~=
;; #kinds * amount
;; storage could be optimized by not retaining values that will
;; never be looked up again, giving a requirement
;; #kinds * largest-denomination
;; time requirements in both cases are O(1) per additional value
;; computed (notably efficient) so #kinds * amount in total
