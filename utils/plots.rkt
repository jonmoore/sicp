#lang sicp

(#%require plot)
(#%require "../utils/srfi-1.rkt")
(#%require "../utils/sicp-utils.rkt")
(#%require debug/repl)
(#%require (only racket keyword-apply))

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

(#%provide pairs->renderer-ex)
(define (pairs->renderer-ex pairs points-args points-keys points-vals)
  ;; pairs should be a SICP list of pairs.
  ;; See points from plot
  ;; https://docs.racket-lang.org/plot/renderer2d.html#%28def._%28%28lib._plot%2Fmain..rkt%29._points%29%29
  (keyword-apply lines
                 (list->r/list points-keys)
                 (list->r/list points-vals)
                 (list->r/list pairs)
                 (list->r/list points-args)))

(#%provide pairs->renderer)
(define (pairs->renderer pairs)
  ;; pairs should be a SICP list of pairs.
  ;; See points from plot
  ;; https://docs.racket-lang.org/plot/renderer2d.html#%28def._%28%28lib._plot%2Fmain..rkt%29._points%29%29
  (pairs->renderer-ex pairs '() '() '()))

(#%provide plottables->plottable)
(define (plottables->plottable plottables)
  (list->r/list plottables))
