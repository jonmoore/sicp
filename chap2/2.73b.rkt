#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (install-rectangular-package)
  ;; rectangular representation
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
     (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (install-polar-package)
  ;; polar representation
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (make-from-mag-ang r a)
    (cons r a))

  (define (tag x) (attach-tag 'polar x))

  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these type -- APPLY-GENERIC"
                 (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operand exp) (cdr exp))

;; a.  We've now got lookup of how to differentiate based on operator.
;; number? and same-variable? act on expressions for which operator is
;; not defined, at least in this representation.

(define (install-sum-deriv)

  (define (addend operands)
    (car operands))
  (define (augend operands)
    (cadr operands))
  (define (make-sum x y) (list '+ x y))
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands))
              (deriv (augend operands))))
  (put 'deriv '+ deriv-sum)
  (put 'make '+ make-sum)
  'done)

(define (install-prod-deriv)

  (define (multiplicand operands)
    (car operands))
  (define (multiplier operands)
    (cadr operands))
  (define (make-prod x y) (list '* x y))
  (define (deriv-prod operands var)
    ((get 'make '+)
     (make-prod (deriv (multiplicand operands) var)
                (multiplier operands))
     (make-prod (multiplicand operands)
                (deriv (multiplier operands) var))))
  (put 'deriv '+ deriv-prod)
  (put 'make '+ make-prod)
  'done)

(define (install-exp-deriv)

  (define (base operands)
    (car operands))
  (define (power operands)
    (cadr operands))
  (define (make-exp b p) (list '** b p))

  (define (deriv-exp operands var)
    ((get 'make '*)
     (power operands)
     ((get 'make '*)
      (deriv (base operands) var)
      (make-exp (base operands) (- (power operands) 1)))))
  (put 'deriv '** deriv-exp)
  (put 'make '** make-exp)
  'done)


;; d changes are tiny
