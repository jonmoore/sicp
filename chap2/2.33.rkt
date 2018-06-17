#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (-map p sequence)
  (accumulate
   (lambda (x y) (cons (p x) y)) nil sequence))

(define (-append seq1 seq2)
  (accumulate cons
              seq2 seq1))

(define (-length sequence)
  (accumulate (lambda (el accum) (inc accum)) 0 sequence))

(module+ main
  (begin-example "2.33")
  (displn-eval '(-map square (iota 5)) ns)
  (displn-eval '(-append (iota 4) (iota 2)) ns)
  (displn-eval '(-length (iota 4)) ns)
  (displn-eval '(-length (iota 6)) ns))

;; 2.34

(define (horner-eval x coeffs)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coeffs))

(module+ main
  (begin-example "2.34")
  (displn-eval '(horner-eval 2 (list 1 1 0 1)) ns))

;; 2.35

(define (count-leaves t)
  (define (count-self-or-leaves t)
    (if (atom? t) 1 (count-leaves t)))
  (accumulate + 0
              (map count-self-or-leaves t)))

(module+ main
  (begin-example "2.35")
  (displn-eval '(count-leaves (list (list 1 1) (list 1 1) 1)) ns))

;; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(module+ main
  (begin-example "2.36")
  (displn-eval '(accumulate-n + 0 s) ns))

;; 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (trans mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m1 m2)
  (let ((cols (trans m2)))
    (map (lambda (row)
           (map (lambda (col) (dot-product row col)) cols))
         m1)))

(module+ main
  (begin-example "2.37")

  (displn-eval '(matrix-*-vector '((1 2) (3 4)) '(1 0)) ns)

  (displn-eval '(trans '((1 2) (3 4))) ns)

  (displn-eval '(matrix-*-matrix '((1 2) (3 4)) '((1 2) (3 4))) ns))

;; 2.38

;; below, we use a version of fold-left where op takes the accumulated
;; result as its second argument, to match cons and accumulate
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (/-swap x y) (/ y x))

(module+ main
  (begin-example "2.38")

  (displn "Replacing (1 2 3) with (2 3 5) for clarity")
  
  (displn-eval '(fold-right / 1 (list 2 3 5)) ns)
  (displn-eval '(fold-left / 1 (list 2 3 5)) ns)

  (displn-eval '(fold-right /-swap 1 (list 2 3 5)) ns)
  (displn-eval '(fold-left /-swap 1 (list 2 3 5)) ns)

  (displn-eval '(fold-right cons nil (list 2 3 5)) ns)
  (displn-eval '(fold-left cons nil (list 2 3 5)) ns)

  (displn-eval '(fold-right list nil (list 2 3 5)) ns)
  (displn-eval '(fold-left list nil (list 2 3 5)) ns)
  )

;; required property - order of providing elements for combination
;; into sum does not matter
;; (op (op nil e1) e2) fold-left
;; (op (op;; nil e2) e1) fold-right

;; associativity+comutativity suffices

;; fold-right is not a good choice here
(define (reverse-right sequence)
  (fold-right (lambda (el accum) (append accum (list el))) nil sequence))

(define (reverse-left sequence)
  (fold-left (lambda (el accum) (cons el accum)) nil sequence))

(module+ main
  (begin-example "2.39")
  (displn-eval '(reverse-right (iota 5)) ns)
  (displn-eval '(reverse-left (iota 5)) ns))

