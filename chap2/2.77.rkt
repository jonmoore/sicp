#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(#%require "alg-1.rkt" "numeric-packages.rkt" "equality-package.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;; We follow this chapter but instead of using put, which uses
;; mutation (and mutation itself hasn't been introduced yet) we'll
;; have those blocks return lists of rules

(define (apply-generic alg op . args)
  ;; (displn (cons "apply-generic" (cons op args)))
  (define (apply-2 a1 a2)
    (let ((type1 (type-tag a1))
          (type2 (type-tag a2)))
      (if (not (eq? type1 type2))
          (let ((t1->t2 (get alg 'coerce (list type1 type2)))
                (t2->t1 (get alg 'coerce (list type2 type1))))
            (cond (t1->t2 (apply-generic alg op (t1->t2 a1) a2))
                  (t2->t1 (apply-generic alg op a1 (t2->t1 a2)))
                  (else
                   (error "No generic method (#1) for unequal types" (list op 'type1 'type2)))))
          (error "No generic method (#2) for equal types" (list op 'type1 'type2)))))

  (let* ((type-tags (map type-tag args))
         (proc (get alg op type-tags)))
    (if proc 
        (apply proc args)
        (if (= (length args) 2)
            (apply apply-2 args)
            (error "No generic method (#3) for !=2 types" (list op type-tags))))))

(define (add alg x y) (apply-generic alg 'add x y))
(define (sub alg x y) (apply-generic alg 'sub x y))
(define (mul alg x y) (apply-generic alg 'mul x y))
(define (div alg x y) (apply-generic alg 'div x y))
(define (numer alg x) (apply-generic alg 'numer x))
(define (denom alg x) (apply-generic alg 'denom x))
(define (mag alg x) (apply-generic alg 'magnitude x))
(define (equ? alg x y) (apply-generic alg 'equ? x y))
(define (re-part alg x) (apply-generic alg 're-part x))
(define (im-part alg x) (apply-generic alg 'im-part x))
(define (=zero? alg x) (apply-generic alg '=zero? x))
(define (raise alg x) (apply-generic alg 'raise x))

(define (apply-installers alg . installers)
  (if (null? installers)
      alg
      (let ((installer (car installers))
            (rest (cdr installers)))
        (apply apply-installers (installer alg) rest))))

(define (install-coercions alg)
  (define (rational->scheme-number alg q)
    (/ (numer alg q) (denom alg q) 1.0))
  (define (scheme-number->complex alg n)
    (make-complex-from-real-imag alg (contents n) 0))
  (put alg
       (make-rec-rule 'coerce '(rational scheme-number) rational->scheme-number)
       (make-rec-rule 'coerce '(scheme-number complex) scheme-number->complex)))

(define (coerce alg x to-type)
  ((get alg 'coerce (list (type-tag x) to-type))
   x))

(define -wn (apply-installers
             (empty-alg)
             install-complex-package install-rectangular-package install-polar-package
             install-rational-package install-scheme-number-package))
(module+ main
  (displn-eval '((get -wn 'make 'scheme-number) 23) ns)
  (displn-eval '((get -wn 'make 'rational) 3 4) ns)
  (displn-eval '(make-rational -wn 3 4) ns)
  (displn-eval '(make-rectangular-from-real-imag -wn 3 4) ns)
  (displn-eval '(make-complex-from-real-imag -wn 3 4) ns)

  (displn-eval '(mag -wn (make-complex-from-real-imag -wn 3 4)) ns)
  (displn-eval '(re-part -wn (make-complex-from-mag-ang -wn 1 (atan 1 1))) ns)
  )

(define -wc (apply-installers
             -wn install-equ (make-install-=zero? apply-generic) install-coercions))
(module+ main

  (displn "equ? examples")
  (displn-eval '(equ? -wc 1 1) ns )
  (displn-eval '(equ? -wc 1 (make-rational -wc 3 4)) ns )
  (displn-eval '(equ? -wc (make-rational -wc 3 4) (make-rational -wc 3 4)) ns )
  
  (displn "=zero? examples")
  (displn-eval '(=zero? -wc 1) ns )
  (displn-eval '(=zero? -wc (make-rational -wc 0 4)) ns )
  (displn-eval '(=zero? -wc (make-rectangular-from-real-imag -wc 3 4)) ns)

  (displn "coercion examples")
  (displn-eval '(coerce -wc 3 'complex) ns)
  (displn-eval '(coerce -wc (make-rational -wc 3 7) 'scheme-number) ns)
  (displn-eval '(let ((z (make-complex-from-real-imag -wc 3 0)))
                  (list (add -wc z 4)
                        (add -wc 4 z)))
               ns)
  )

;; 2.77 - there is no function magnitude in the text - need an earlier
;; version.  In the above I've made all the functions operate on typed
;; variables for clarity (types are always present) and brevity (I
;; don't want to constantly strip and add tags ad hoc)
;;
;; 2.78 - adjusted above
;;
;; 2.79 - see install-equ
;;
;; 2.80 - see install-=zero?
;;
;; 2.81 - 1. works through testing type conversions.  2. should work
;; correctly as is.  3. see above where we check the tags
;;
;; 2.82 - thought - for each type generate the set of types that it
;; can be converted to.  For an n-argument function this defines an
;; set of n-tuples of compatible signatures.  Issue: how to choose
;; from functions with matching signatures if there is more than one.
;; Issue: size of set of candidate signatures.
;;
;; 2.83

;; integer, rational, real, complex.  Here we just do
;; rational->scheme-number(real)->complex, ignoring that scheme-number
;; has its own rationals, at least in Racket

(define (install-raise alg)
  (define (raising-rule from-type to-type)
    (make-rec-rule 'raise (list from-type)
                   (lambda (alg from)
                     (coerce alg from to-type))))
  (define (raising-edge from-type to-type)
    (make-rec-rule 'raise-edge (list from-type to-type)
                   (lambda (alg from to)
                     (error "This is a placeholder that should not be called"))))
  (apply put alg
         (append-map
          (lambda (raising)
            (list (raising-rule (car raising) (cdr raising))
                  (raising-edge (car raising) (cdr raising))))
          (list (cons 'rational 'scheme-number)
                (cons 'scheme-number 'complex)))))

(define -wr (apply-installers -wc install-raise))

(module+ main
  (displn-eval '(raise -wr (make-rational -wr 3 4)) ns)
  (displn-eval '(raise -wr .75) ns)
  (displn-eval '(op-types -wr 'raise) ns)
  (displn-eval '(op-types -wr 'raise-edge) ns)

  )

;; Exercise 2.84: Using the raise operation of Exercise 2.83, modify
;; the apply-generic procedure so that it coerces its arguments to
;; have the same type by the method of successive raising, as
;; discussed in this section. You will need to devise a way to test
;; which of two types is higher in the tower. Do this in a manner that
;; is "compatible" with the rest of the system and will not lead to
;; problems in adding new levels to the tower.



