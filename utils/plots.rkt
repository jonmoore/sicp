#lang sicp

(#%require plot)
(#%require "../utils/srfi-1.rkt")

(define (loglog-valid-pairs xy-pairs)
  (filter
   (lambda (list-xy)
     (and (> (car list-xy) 0)
          (> (cadr list-xy) 0)))
   xy-pairs))

(define (loglog-map base xy-pairs)
  (map (lambda (xy)
         (list (log (car xy) base)
               (log (cadr xy) base)))
       xy-pairs))

(#%provide plot-points-loglog)
(define (plot-points-loglog base data)
  (plot
   (points
    (loglog-map base
                (loglog-valid-pairs data)))))
