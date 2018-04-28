#lang sicp

(#%require (only racket module+) rackunit)

;; This creates a side-effect when this file is imported, making
;; racket output use "()" rather than "{}" to print lists created in
;; the sicp language.  We want this side-effect.  It's a subset of
;; r5rs/init.
(#%require (only racket print-mpair-curly-braces))
(print-mpair-curly-braces #f)

(define (disp text)
  (display text (current-output-port)))

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
