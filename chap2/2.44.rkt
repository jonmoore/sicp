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

(define (apply-make-segment point-pair)
  (apply make-segment point-pair))
         
;; 2.49
(define (points->poly-segments points)
  ;; convert list of points to list of segments forming closed polygon
  (map
   apply-make-segment
   (zip
    points
    (append (cdr points) (list (car points))))))

(define (pairs->points pairs)
  (map (lambda (pair) (apply make-vect pair)) pairs))

(define (make-pairs->painter points->segments)
  (lambda (pairs)
    (segments->painter
     (points->segments
      (pairs->points
       pairs)))))

(define pairs->painter
  (make-pairs->painter points->poly-segments))


(define draw-outline
  (pairs->painter
   '((0 0) (.99 0) (.99 .99) (0 .99))))

(define draw-diamond
  (pairs->painter
   '((0.5 0.0) (1.0 0.5) (.5 1.0) (0 .5))))

(define draw-arrow
  (pairs->painter
   '((1.0 0.0) (0.5 0.0) (1.0 0.5) (.5 1.0) (0 .5))))

(define draw-x
  (segments->painter
    (list (make-segment (make-vect 0.02 0.02) (make-vect 0.98 0.98))
          (make-segment (make-vect 0.02 0.98) (make-vect 0.98 0.02)))))

(module+ main
  (begin-example "2.49")
  (paint draw-outline)
  (paint draw-x)
  (paint draw-diamond))

;; 2.50
(define (points->poly-segments-open points)
  ;; convert list of points to list of segments forming open polygon
  (map
   apply-make-segment
   (zip points (cdr points))))

(define pairs->painter-open
  (make-pairs->painter points->poly-segments-open))

(define draw-half-arrow
  (pairs->painter-open
   '((1.0 0.0) (0.75 0.0) (.75 0.5) (.5 .5) (1.0 1.0))))

;; Define the name SICP uses.  sicp-pict, for no good reason, choses
;; different names for its vector functions
(define sub-vect vector-sub)

;; modified from the SICP version to work with 
(define (transform-painter
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1)
                                       new-origin)
                             (sub-vect (m corner2)
                                       new-origin)))))))

(define (flip-horiz painter)
  (transform-painter 
   painter
   (make-vect 1.0 0.0)   ; new origin
   (make-vect 0.0 0.0)   ; new end of edge1
   (make-vect 1.0 1.0))) ; new end of edge2

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (rotate180 (rotate90 painter)))

(define arrow^
  (beside draw-half-arrow (flip-horiz draw-half-arrow)))

(module+ main
  (begin-example "2.50")
  (displn "Use flip-horiz and beside to draw an arrow from two half-arrows")
  (paint arrow^)
  (displn "half-arrow rotations 0, 90, 180, 270")
  (map
   (lambda (transform)
     (paint (transform draw-half-arrow)))
   (list values rotate90 rotate180 rotate270)))

;; Sign: sicp-pict provides definitions that clash with those to be
;; implemented (like "below") but fails to provide definitions that
;; should be available.  You would think someone writing a teaching
;; library to accompany *a specific text* would match that.
(define (-below painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-bottom (transform-painter 
                         painter1
                         (make-vect 0.0 0.0)
                         (make-vect 1.0 0.0)
                         (make-vect 0.0 0.5)))
          (paint-top (transform-painter
                      painter2
                      (make-vect 0.0 0.5)
                      (make-vect 1.0 0.5)
                      (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (-below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(module+ main
  (begin-example "2.51")
  (displn "Use of below: half-arrow below diamond")
  (paint (-below draw-half-arrow draw-diamond))
  (displn "Use of -below2: written in terms of beside")
  (paint (-below2 draw-half-arrow draw-diamond)))

(define (nothing frame)
  '#f)

(define (x-scale scale)
  (lambda (painter)
    (transform-painter painter
                       (make-vect 0.0 0.0)
                       (make-vect scale 0.0)
                       (make-vect 0.0 1.0))))

(define (y-scale scale)
  (lambda (painter) 
    (transform-painter painter
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 0.0)
                       (make-vect 0.0 scale))))

(define (translate x-trans y-trans)
  (lambda (painter) 
    (transform-painter painter
                       (make-vect x-trans y-trans)
                       (make-vect (inc x-trans) y-trans)
                       (make-vect x-trans (inc y-trans)))))
  
(define (at-sub-plot x-index y-index nx ny)
  (lambda (painter)
    (let ((x-spacing (/ 1.0 nx))
          (y-spacing (/ 1.0 ny)))
      ((translate (* x-index x-spacing) (* y-index y-spacing))
       ((y-scale y-spacing)
        ((x-scale x-spacing)
         painter))))))

(define (painters->painter painters)
  (lambda (frame)
    (map
     (lambda (painter)
       (painter frame))
     painters)))

(define (layout-by-index at-index painters)
  (map (lambda (index painter)
         ((at-index index (length painters)) painter))
       (iota (length painters))
       painters))

(define (in-row . painters)
  (define (at-row-index index len)
    (at-sub-plot index 0 len 1))
  (painters->painter
   (layout-by-index at-row-index painters)))

(define (in-col . painters)
  (define (at-col-index index len)
    (at-sub-plot 0 index 1 len))
  (painters->painter
   (layout-by-index at-col-index (reverse painters))))

(define (in-+ painter)
  (in-col
   (in-row nothing painter nothing)
   (in-row painter painter painter)
   (in-row nothing painter nothing)))

(define (rot-+ painter)
  (in-col
   (in-row nothing painter nothing)
   (in-row (rotate90 painter) nothing (rotate270 painter))
   (in-row nothing (rotate180 painter) nothing)))

(define (in-x painter)
  (in-col
   (in-row painter nothing painter)
   (in-row nothing painter nothing)
   (in-row painter nothing painter)))

(define (rot-x painter)
  (in-col
   (in-row (rotate90 painter) nothing painter)
   (in-row nothing painter nothing)
   (in-row (rotate180 painter) nothing (rotate270 painter))))

(define (2-iters iter painter)
  (list painter (iter painter) (iter (iter painter))))

(define (map-cross-product row-els col-els f)
  ;; create a matrix from row-els and col-els, by applying f to each
  ;; pair from the cross-product
  (map (lambda (row-el)
         (map (lambda (col-el)
                (f row-el col-el))
              col-els))
       row-els))

(define (matrix-painter tfm-matrix painter)
  (define (row-painter tfm-row)
    (apply in-row
           (map (lambda (tfm) (tfm painter)) tfm-row)))
  (apply
   in-col
   (map row-painter tfm-matrix)))

(module+ main
  (displn "Some more elaborate examples")
  (paint (in-+ arrow^))
  (paint (rot-+ arrow^))
  (paint (rot-+ (in-+ arrow^)))
  (displn "looping")
  (map
   (lambda (tfm)
     (map paint (2-iters tfm arrow^)))
   (list rot-+
         rot-x
         (compose rot-x rot-+)
         (compose rot-+ rot-x)))
  (displn "matrix")
  
  (let* ((tfms (list in-+ in-x rot-+ rot-x))
         (tfm-mat (map-cross-product tfms tfms compose)))
    (paint (matrix-painter tfm-mat arrow^))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside
         (below painter (beside up up))
         (below (below right right) corner)))))

(define (corner-split-2 painter n)
  (if (= n 0)
      painter
      (let ((up (rotate90 (up-split painter (- n 1))))
            (right (right-split painter (- n 1)))
            (corner (corner-split-2 painter (- n 1))))
        (beside
         (below painter (beside up up))
         (below (below right right) corner)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (matrix-painter
     (list
      (list tl tr)
      (list bl br))
     painter)))

(define (square-limit painter n)
  (let ((combine4 
         (square-of-four
          flip-horiz identity
          rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (square-limit-2 painter n)
  (let ((combine4 
         (apply square-of-four
                (map
                 (lambda (tr)
                   (compose rotate180 tr))
                 (list 
                  flip-horiz identity
                  rotate180 flip-vert)))))
    (combine4 (corner-split painter n))))
  
(module+ main
  (begin-example "2.52")
  (paint (corner-split-2 arrow^ 3))
  (paint (square-limit arrow^ 3))
  (paint (square-limit-2 arrow^ 3)))
