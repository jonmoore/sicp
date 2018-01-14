#lang sicp

(#%require "utils.rkt")
(#%require plot)

(#%provide plot-points)
(define (plot-points data)
  (plot (points data)))

(#%provide plot-points-ex-pairs)
(define (plot-points-ex-pairs tfm-x tfm-y data)
  (plot
   (points
    (map (Î» (list-xy)
           (list (tfm-x (car list-xy))
                 (tfm-y (cadr list-xy))))
         data))))

(#%provide plot-points-loglog)
(define (plot-points-loglog base data)
  (let ((log-fn (lambda (x) (logx-basey x base))))
    (plot-points-ex-pairs log-fn log-fn
                          (filter
                           (Î» (list-xy)
                             (and (> (car list-xy) 0)
                                  (> (cadr list-xy) 0)))
                           data))))
