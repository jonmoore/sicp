#lang sicp

(#%require plot)
(#%require "../utils/srfi-1.rkt")
(#%require "../utils/sicp-utils.rkt")

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

(#%provide loglog-points)
(define (loglog-points base data)
  (loglog-map base
              (loglog-valid-pairs data)))

(#%provide plot-points-loglog)
(define (plot-points-loglog base data)
  (plot
   (points
    (loglog-points base data))))

(#%provide pairs->plottable)
(define (pairs->plottable pairs)
  (points (list->r/list pairs)))

(#%provide plottables->plottable)
(define (plottables->plottable plottables)
  (list->r/list plottables))
