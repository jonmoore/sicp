#lang sicp

(#%require (only racket module+) rackunit)
(#%require plot)
(#%require "../utils/srfi-1.rkt" "../utils/sicp-utils.rkt" "../utils/plots.rkt")

;; ex-1.14
;; from the text
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; some caching helpers
(define (make-cc-cache max-kinds-of-coins)
  (make-vector max-kinds-of-coins '()))
(define (cc-cache-get cache amount kinds-of-coins)
  (let ((entry (assoc amount (vector-ref cache (dec kinds-of-coins)))))
    (if entry (cadr entry) #f)))
(define (cc-cache-insert cache amount kinds-of-coins result)
  (let ((offset (dec kinds-of-coins)))
    (vector-set! cache offset
                 (cons
                  (list amount result)
                  (vector-ref cache offset)))))

;; we split the algorithm because there's little point in caching the
;; edge cases.  For this exercise we use a convention that a false
;; (#f) result means that no result could be obtained.
(define (cc-edge-cases amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else #f)))

;; add cc-fn as an argument to allow us to use caching in the
;; recursive calls too.
(define (cc-core amount kinds-of-coins cc-fn)
  (+ (cc-fn amount
            (- kinds-of-coins 1))
     (cc-fn (- amount (first-denomination kinds-of-coins))
            kinds-of-coins)))

;; basic algorithm without caching.  
(define (cc amount kinds-of-coins)
  (cond ((cc-edge-cases amount kinds-of-coins))
        (else
         (cc-core amount kinds-of-coins cc))))

;; adding caching to the basic algorithm.  Again we use the convention
;; that a #f return means no result could be found.
(define (cc-cached-for max-kinds-of-coins)
  (define cache (make-cc-cache max-kinds-of-coins))
  (define (cc-cached amount kinds-of-coins)
    (cond ((cc-edge-cases amount kinds-of-coins))
          ((cc-cache-get cache amount kinds-of-coins))
          (else
           ;; could make this shorter by returning new-result from
           ;; cc-cache-insert but the below is more obvious.
           (let ((new-result (cc-core amount kinds-of-coins cc-cached)))
             (cc-cache-insert cache amount kinds-of-coins new-result)
             new-result))))
  cc-cached)

;; In the second approach to caching we wrap our cache in an object.
;; This makes it super-obvious that we could get different behaviors,
;; including no caching, by passing in an appropriate object.
;;
;; If we'd wanted, we could have basically kept the cached code above
;; (except for making the cache) by just having cc-cache-get invoke
;; 'get on cache, now assumed to be an object.
(define (make-cache-obj max-kinds-of-coins)
  (define cache (make-cc-cache max-kinds-of-coins))
  (lambda (sym)
    (cond
     ((eq? sym 'get)
      (lambda (amount kinds-of-coins)
        (cc-cache-get cache amount kinds-of-coins)))
     ((eq? sym 'insert)
      (lambda (amount kinds-of-coins new-result)
        (cc-cache-insert cache amount kinds-of-coins new-result)))
     (else (error sym)))))

(define (cc-cached-for2 max-kinds-of-coins)
  (define cache-obj (make-cache-obj max-kinds-of-coins))
  (define (cc-cached amount kinds-of-coins)
    (cond ((cc-edge-cases amount kinds-of-coins))
          (((cache-obj 'get) amount kinds-of-coins))
          (else
           (let ((new-result (cc-core amount kinds-of-coins cc-cached)))
             ((cache-obj 'insert) amount kinds-of-coins new-result)
             new-result))))
  cc-cached)

(module+ test
  (test-case
   "equivalence with / without caching"
   (let ((cc-cached (cc-cached-for 4))
         (amounts-to-test '(50 100 200))
         (kinds-to-test '(2 3 4)))
     (for-each (lambda (kinds)
                 (for-each (lambda (amount)
                             (check-equal? (cc amount kinds)
                                           (cc-cached amount kinds)))
                           amounts-to-test))
               kinds-to-test)))

  (test-case
   "equivalence of different caching styles"
   (let ((cc-cached1 (cc-cached-for 4))
         (cc-cached2 (cc-cached-for2 4))
         (amounts-to-test '(50 100 200))
         (kinds-to-test '(2 3 4)))
     (for-each (lambda (kinds)
                 (for-each (lambda (amount)
                             (check-equal? (cc-cached1 amount kinds)
                                           (cc-cached2 amount kinds)))
                           amounts-to-test))
               kinds-to-test))))

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

(module+ main
  (define (data-points fn x0 x-limit y-limit)
    (define (next-x x) (floor (* 1.2 x)))
    (define (iter x pairs)
      (if (> x x-limit)
          pairs
          (let ((y (fn x)))
            (if (> y y-limit)
                (cons (list x y) pairs)
                (iter (next-x x)
                      (cons (list x y) pairs))))))
    (iter x0 '()))

  (define (estimate-order fn x0 x-limit y-limit)
    (define (dataf->order dataf)
      (let* ((largest2 (take dataf 2))
             (x1 (car (cadr largest2)))
             (y1 (cadr (cadr largest2)))
             (x2 (car (car largest2)))
             (y2 (cadr (car largest2))))
        (/ (- (log y1) (log y2))
           (- (log x1) (log x2)))))
    (let* ((data (data-points fn x0 x-limit y-limit))
           (dataf (map-tree exact->inexact data)))
      (dataf->order dataf)))

  (define (counters-for-kinds kind-list cc-fn)
    (map (lambda (kinds)
           (lambda (amount)
             (cc-fn amount kinds)))
         kind-list))

  ;; display how estimated order of growth varies by number of kinds
  (let* ((kinds-list '(2 3 4 5))
         (cc-cached (cc-cached-for 5)))
    (display "kinds: ")
    (displn kinds-list)
    (display "estimated orders of growth: ")
    (displn
     (map
      (lambda (counter)
        (estimate-order counter 100 1000 50000))
      (counters-for-kinds kinds-list cc-cached))))
  
  (define (growth-results fns x0 x-limit y-limit)
    (map-tree exact->inexact
              (map
               (lambda (fn)
                 (data-points fn x0 x-limit y-limit))
               fns)))

  (define (plot-growth-in fns x0 x-limit y-limit)
    (define (log10log10-points result)
      (loglog-points 10 result))
    (let* ((results (growth-results fns x0 x-limit y-limit))
           (logresults (map log10log10-points results))
           (line-renderers (plottables->plottable
                            (map pairs->plottable
                                 logresults))))
      (plot line-renderers)))

  (plot-new-window? #t)

  (displn "Plot growth in ways to make change vs amount for each kind of coin")
  (let ((kinds-list '(2 3 4 5))
        (cc-cached (cc-cached-for 5)))
    (plot-growth-in (counters-for-kinds kinds-list cc-cached)
                    100 1000 50000))

  (displn "Plot growth in 'time to count ways to make change with 5 kinds of coin' vs amount for algorithms with/without caching")
  ;; Plot how cached and un-cached functions perform
  (define (time-to-count-ways-to-change kinds cc)
    (lambda (amount)
      (cc-run-time (cc-perform-run cc kinds amount))))
  (let ((cc-cached (cc-cached-for 5)))
    (plot-growth-in (map (lambda (cc-fn) (time-to-count-ways-to-change 5 cc-fn))
                         (list cc cc-cached))
                    100 1000 1.0)))

;; expect change count is O(amount^(#kinds-1)), but with counts for
;; small amounts influenced by lower #s of coins
