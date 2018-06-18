#lang sicp
(#%require (only racket module+) sicp-pict)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

(define ein2 (beside einstein (flip-vert einstein)))
(define ein4 (below ein2 ein2))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

;; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                                  (- n 1))))
        (below painter 
                (beside smaller smaller)))))

(module+ main
  (begin-example "2.44")
  (paint (up-split einstein 3)))

;; 2.45
(define (split outer inner)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split outer inner) painter (- n 1))))
          (outer painter 
                 (inner smaller smaller))))))

(module+ main
  (begin-example "2.45")
  (paint ((split below beside) einstein 3))
  (paint ((split beside below) einstein 3)))

;; 2.46

(define simple-frame
  (make-frame (make-vect 2 2)
              (make-vect 1 0)
              (make-vect 0 1)))

(define (-make-vect x y)
  (list x y))
(define (-xcor-vect v)
  (car v))
(define (-ycor-vect v)
  (cadr v))
(define (-add-vect v1 v2)
  (-make-vect (+ (-xcor-vect v1) (-xcor-vect v2))
              (+ (-ycor-vect v1) (-ycor-vect v2))))
(define (-sub-vect v1 v2)
  (-make-vect (- (-xcor-vect v1) (-xcor-vect v2))
              (- (-ycor-vect v1) (-ycor-vect v2))))
(define (-scale-vect s v)
  (-make-vect (* (-xcor-vect v) s)
              (* (-ycor-vect v) s)))

(module+ main
  (begin-example "2.46")
  ((frame-coord-map simple-frame) (make-vect 0 0)))

;; 2.47

(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-1-origin f)
  (car f))
(define (frame-1-edge1 f)
  (cadr f))
(define (frame-1-edge2 f)
  (caddr f))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frame-2-origin f)
  (car f))
(define (frame-2-edge1 f)
  (cadr f))
(define (frame-2-edge2 f)
  (cddr f))

;; 2.48

;; Omit - messes up sicp-pict

;; (define (make-segment v1 v2)
;;   (list v1 v2))
;; (define (start-segment s)
;;   (car s))
;; (define (end-segment s)
;;   (cadr s))

;; 2.49
(define (points->poly-segments points)
  ;; convert list of points to list of segments forming closed polygon
  (map
   (lambda (point-pair)
     (apply make-segment point-pair))
   (zip
    points
    (append (cdr points) (list (car points))))))

(define (pairs->points pairs)
  (map (lambda (pair) (apply make-vect pair)) pairs))

(define (pairs->painter pairs)
  (segments->painter
   (points->poly-segments
    (pairs->points
     pairs))))

(define draw-outline
  (pairs->painter
   '((0 0) (.99 0) (.99 .99) (0 .99))))

(define draw-diamond
  (pairs->painter
   '((0.5 0.0) (1.0 0.5) (.5 1.0) (0 .5))))

(define draw-x
  (segments->painter
    (list (make-segment (make-vect 0.02 0.02) (make-vect 0.98 0.98))
          (make-segment (make-vect 0.02 0.98) (make-vect 0.98 0.02)))))

(module+ main
  (begin-example "2.49")
  (paint draw-outline)
  (paint draw-x)
  (paint draw-diamond))

