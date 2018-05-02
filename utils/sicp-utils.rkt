#lang sicp

(#%require (only racket module+) rackunit)

;; This creates a side-effect when this file is imported, making
;; racket output use "()" rather than "{}" to print lists created in
;; the sicp language.  We want this side-effect.  It's a subset of
;; r5rs/init.
(#%require (only racket print-mpair-curly-braces))
(#%require "srfi-1.rkt")
(print-mpair-curly-braces #f)

(#%provide disp)
(define (disp text)
  (display text (current-output-port)))

(#%provide displn)
(define (displn text)
  (disp text)
  (newline))

(#%provide begin-example)
(define (begin-example text)
  (newline)
  (disp "Example ") (disp text) (newline)
  (disp "-----------------------------------------------------------")
  (newline))

(#%provide nth)
(define nth list-ref)

(#%provide !=)
(define (!= a b)
  (not (= a b)))

(#%provide make-timer)
(define (make-timer)
  (define (timer-from start)
    (lambda ()
      (/ (- (runtime) start) 1e6)))
  (timer-from (runtime)))

;; https://stackoverflow.com/a/12610253 says that The Little Schemer
;; uses this.
(#%provide atom?)
(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))
(module+ test
  (test-case "empty list"
             (check-pred atom? #f)
             (check-true (atom? 3))
             (check-true (atom? #f))
             (check-false (atom? '()))
             (check-false (atom? '(1)))
             (check-false (atom? '(())))
             (check-false (atom? '((1))))
             (check-false (atom? '((1 2))))
             (check-false (atom? '((1) 2)))
             ))


(#%provide constant)
(define (constant x)
  (lambda () x))


(#%provide map-tree)
(define (map-tree f tree)
  (if (atom? tree)
      (f tree)
      (map (lambda (t) (map-tree f t))
           tree)))

(#%require (prefix r/ racket))
(#%provide list->r/list)
(define (list->r/list lis)
  "Map a sicp list to a racket list"
  (define (iter accum rest)
    (if (null? rest)
        accum
        (iter (r/cons (car rest) accum) (cdr rest))))
  (iter (r/list) lis))

(#%provide average)
(define (average x y)
  (/ (+ x y) 2))

(#%provide abs-error)
(define (abs-error x y)
  (abs (- x y)))

(#%provide rel-error)
(define (rel-error x y)
  (/ (abs-error x y)
     (average (abs x) (abs y))))

(#%provide close-in-abs-error?)
(define (close-in-abs-error? x y abs-tol)
  (<= (abs-error x y) abs-tol))

(#%provide close-in-rel-error?)
(define (close-in-rel-error? x y rel-tol)
  (<= (rel-error x y) rel-tol))

(#%provide close-in-joint-error-or?)
(define (close-in-joint-error-or? x y abs-tol rel-tol)
  (or (close-in-abs-error? x y abs-tol)
      (close-in-rel-error? x y rel-tol)))

(#%provide sum)
(define (sum lis)
  (fold + 0 lis))

(#%provide product)
(define (product lis)
  (fold * 1 lis))
