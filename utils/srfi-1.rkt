#lang sicp

(#%require (only racket module+) rackunit)

;; subset of https://srfi.schemers.org/srfi-1/srfi-1.html

;; https://srfi.schemers.org/srfi-1/srfi-1.html#iota
(#%provide iota)
(define (iota count . args)
  (define start
    (if (>= (length args) 1)
        (car args)
        0))
  (define step
    (if (>= (length args) 2)
        (cadr args)
        1))
  (define (iter cur-len cur-list)
    (if (= cur-len count)
        cur-list
        (iter (+ cur-len 1)
              (cons (+ start
                       (* step (- count cur-len 1)))
                    cur-list))))
  (if (< count 0)
      (error "count must be positive")
      (iter 0 '())))

(module+ test
  (test-case
   "check (iota 5)"
   (check-equal? (iota 5) '(0 1 2 3 4)))

  (test-case
   "check iota lengths"
   (for-each
    (lambda (n)
      (check-equal? (length (iota n)) n)
      (check-equal? (length (iota n -5)) n)
      (check-equal? (length (iota n 7 3)) n))
    (iota 5))))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#fold 
(#%provide fold)
(define (fold proc init list)
  "Single-list version for now"
  (if (null? list)
      init
      (fold proc (proc init (car list)) (cdr list))))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#count
(#%provide count)
(define (count pred clist)
  (fold (lambda (prev-count el)
          (if (pred el)
              (inc prev-count)
              prev-count))
        0
        clist))

(module+ test
  (test-case
   "counts with iota"
   (for-each
    (lambda (n)
      (check-equal?
       (count (lambda (el) #t) (iota n))
       n))
    (iota 10)))
  (test-case
   "counting odd numbers"
   (for-each
    (lambda (n)
      (check-equal?
       (count odd? (iota (* 2 n)))
       n))
    (iota 10))))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#filter
(#%provide filter)
(define (filter pred list)
  (cond ((null? list) nil)
        ((pred (car list))
         (cons (car list)
               (filter pred (cdr list))))
        (else (filter pred (cdr list)))))

(module+ test
  (test-case
   "filter on empty list"
   (check-equal?
    (filter odd? '())
    '()))

  (test-case
   "filter with odd?"
   (for-each
    (lambda (n)
      (check-equal?
       (filter odd? (iota (* 2 n)))
       (iota n 1 2)))
    (iota 10))))

