#lang sicp

(#%provide iota)
(#%require racket rackunit)

;; subset of https://srfi.schemers.org/srfi-1/srfi-1.html

;; https://srfi.schemers.org/srfi-1/srfi-1.html#iota
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
   "check length (iota 5)"
   (for-each
    (lambda (n)
      (check-equal? (length (iota n)) n)
      (check-equal? (length (iota n -5)) n)
      (check-equal? (length (iota n 7 3)) n))
    (iota 5)))
  (test-case
   "check (iota 5 1)"
   (display "foo")
   (for-each
    (lambda (n)
      (check-equal? (list-ref (iota 5) n) n))
    (iota 5))))

(module+ main
  (iota 5))
