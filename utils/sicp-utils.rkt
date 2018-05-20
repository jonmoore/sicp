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
(define (list->r/list-iter accum rest)
  (if (null? rest)
      accum
      (list->r/list-iter (r/cons (car rest) accum) (cdr rest))))

(#%provide list->r/list)
(define (list->r/list lis)
;;  "Map a sicp list to a racket list"
  (list->r/list-iter (r/list) lis))

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

(#%provide mean)
(define (mean lis)
  (/ (sum lis) (length lis) 1.0))

(#%provide product)
(define (product lis)
  (fold * 1 lis))

(#%provide transpose)
(define (transpose m) (apply zip m))

(#%provide dot)
(define (dot a b)
  (sum (map product (zip a b))))


(#%provide square)
(define (square x)
  (* x x))


(#%provide estimate-order-gen-data-points)
(define (estimate-order-gen-data-points fn x0 x-limit y-limit next-x)
  (define (iter x pairs)
    (if (> x x-limit)
        pairs
        (let ((y (fn x)))
          (if (> y y-limit)
              (cons (list x y) pairs)
              (iter (next-x x)
                    (cons (list x y) pairs))))))
  (iter x0 '()))

(#%provide estimate-order-ex)
(define (estimate-order-ex fn x0 x-limit y-limit next-x)
  (define (dataf->order dataf)
    (let* ((samples (take dataf 2))
           (x1 (car (cadr samples)))
           (y1 (cadr (cadr samples)))
           (x2 (car (car samples)))
           (y2 (cadr (car samples))))
      (/ (- (log y1) (log y2))
         (- (log x1) (log x2)))))
  (let* ((data (estimate-order-gen-data-points fn x0 x-limit y-limit next-x))
         (dataf (map-tree exact->inexact data)))
    (dataf->order dataf)))

(#%provide estimate-order)
(define (estimate-order fn x0 x-limit y-limit)
  (define (next-x x)
    (floor (* 1.2 x)))
  (estimate-order-ex fn x0 x-limit y-limit next-x))

(#%provide and-proc)
(define (and-proc . lis)
  (every values lis))

(module+ test
  (define (id x) x)
  (test-case
   "testing and-proc"
   (check-equal? #t (and-proc))
   (check-equal? 1 (and-proc 2 1))
   (check-equal? #f (and-proc 2 #f 1))))

(#%provide or-proc)
(define (or-proc . lis)
  (any values lis))

(module+ test
  (test-case
   "testing or-proc"
   (check-equal? #f (or-proc))
   (check-equal? 2 (or-proc 2 1))
   (check-equal? 2 (or-proc #f 2 1))
   (check-equal? #t (or-proc #t 2 1))))

(#%provide add)
(define (add x)
  (lambda (y)
    (+ x y)))

(#%provide diff)
(define (diff xs)
  (srfi-map - (cdr xs) xs))

(#%provide partial-sums)
(define (partial-sums xs)
  (reverse
   (fold
    (lambda (el accum)
      (cons (+ el (car accum)) accum))
    '(0)
    xs)))

(module+ test
  (test-case
   "diff of partial-sums is the identity"
   (for-each
    (lambda (lis)
      (with-check-info
       (('lis lis))
       (check-equal?
        (diff (partial-sums lis))
        lis)))
    (map (lambda (n) (iota n 3 2)) (iota 5)))))
