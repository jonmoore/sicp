#lang sicp

(#%provide begin-example)

(define (disp text)
  (display text (current-output-port)))

(define (begin-example text)
  (disp "--------------------------------------------------------------------------------")  (newline)
  (disp "Example ") (disp text) (newline)
  (disp "--------------------------------------------------------------------------------")  (newline)
  )

