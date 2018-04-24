#lang sicp

(#%provide begin-example nth !=)

(define (disp text)
  (display text (current-output-port)))

(define (begin-example text)
  (newline)
  (disp "Example ") (disp text) (newline)
  (disp "-----------------------------------------------------------")
  (newline)
  )

(define nth list-ref)

(define (!= a b)
  (not (= a b)))
