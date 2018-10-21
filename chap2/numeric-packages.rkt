#lang sicp
(#%require (only racket module+))
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt"  "alg-1.rkt")

(define (apply-generic-fixed alg op . args)
  ;; Simple version of apply-generic with no coercion
  (let* ((type-tags (map type-tag args))
         (proc (get alg op type-tags)))
    (if proc 
        (apply proc args)
        (error "No generic method (#3) for !=2 types" (list op type-tags)))))

(define (mag alg x) (apply-generic-fixed alg 'magnitude x))
(define (re-part alg x) (apply-generic-fixed alg 're-part x))
(define (im-part alg x) (apply-generic-fixed alg 'im-part x))

(#%provide install-scheme-number-package)
(define (install-scheme-number-package alg)
  (define (tagging g)
    (lambda args
      (attach-tag 'scheme-number (apply g args))))
  (put alg
       (make-rule 'add '(scheme-number scheme-number) (tagging +))
       (make-rule 'sub '(scheme-number scheme-number) (tagging -))
       (make-rule 'mul '(scheme-number scheme-number) (tagging *))
       (make-rule 'div '(scheme-number scheme-number) (tagging /))
       (make-rule 'make 'scheme-number (lambda (x) (attach-tag 'scheme-number x)))))
(define (make-scheme-number alg n)
  ((get alg 'make 'scheme-number) n))

(#%provide install-rational-package)
(define (install-rational-package alg)
  ;; internal procedures
  (define (numer x) (cadr x))
  (define (denom x) (caddr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (list 'rational (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of the system
  (put alg
       (make-rule 'denom '(rational) denom)
       (make-rule 'numer '(rational) numer)
       (make-rule 'add '(rational rational) add-rat)
       (make-rule 'sub '(rational rational) sub-rat)
       (make-rule 'mul '(rational rational) mul-rat)
       (make-rule 'div '(rational rational) div-rat)
       (make-rule 'make 'rational make-rat)))
(#%provide make-rational)
(define (make-rational alg n d)
  ((get alg 'make 'rational) n d))

(#%provide install-complex-package)
(define (install-complex-package alg)
  ;; internal procedures
  (define (add-complex alg z1 z2)
    (make-from-real-imag alg
     (+ (re-part alg z1) (re-part alg z2))
     (+ (im-part alg z1) (im-part alg z2))))
  (define (sub-complex alg z1 z2)
    (make-from-real-imag alg 
     (- (re-part alg z1) (re-part alg z2))
     (- (im-part alg z1) (im-part alg z2))))
  (define (mul-complex alg z1 z2)
    (make-from-mag-ang 
     (* (magnitude alg z1) (magnitude alg z2))
     (+ (angle alg z1) (angle alg z2))))
  (define (div-complex alg z1 z2)
    (make-from-mag-ang 
     (/ (magnitude alg z1) (magnitude alg z2))
     (- (angle alg z1) (angle alg z2))))
  ;; interface to rest of the system

  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag alg x y)
    (let ((rectangular ((get alg 'make-from-real-imag 'rectangular)
                        x y)))
      (cons 'complex rectangular)))
  (define (make-from-mag-ang alg r a)
    (let ((polar ((get alg 'make-from-mag-ang 'polar) 
                  r a)))
      (cons 'complex polar)))

  (define (dispatch-to-contents op)
    (lambda (alg z)
      (apply-generic-fixed alg op (contents z))))
  
  (put alg
       (make-rec-rule 'add '(complex complex) add-complex)
       (make-rec-rule 'sub '(complex complex) sub-complex)
       (make-rec-rule 'mul '(complex complex) mul-complex)
       (make-rec-rule 'div '(complex complex) div-complex)
       (make-rec-rule 'make-from-real-imag 'complex make-from-real-imag)
       (make-rec-rule 'make-from-mag-ang 'complex make-from-mag-ang)
       (make-rec-rule 'angle 'complex (dispatch-to-contents 'angle))
       (make-rec-rule 'magnitude 'complex (dispatch-to-contents 'magnitude))
       (make-rec-rule 're-part 'complex (dispatch-to-contents 're-part))
       (make-rec-rule 'im-part 'complex (dispatch-to-contents 'im-part))))

(#%provide install-rectangular-package)
(define (install-rectangular-package alg)
  ;; internal procedures
  (define (re-part z) (cadr z))
  (define (im-part z) (caddr z))
  (define (magnitude z)
    (sqrt (+ (square (re-part z)) (square (im-part z)))))
  (define (angle z)
    (atan (im-part z) (re-part z)))
  (define (make-from-real-imag x y) (list 'rectangular x y))
  
  ;; interface to rest of the system
  (put alg
       (make-rule 're-part 'rectangular re-part)
       (make-rule 'im-part 'rectangular im-part)
       (make-rule 'magnitude 'rectangular magnitude)
       (make-rule 'angle 'rectangular angle)
       (make-rule 'make-from-real-imag 'rectangular make-from-real-imag)))

(#%provide make-rectangular-from-real-imag)
(define (make-rectangular-from-real-imag alg x y)
  ((get alg 'make-from-real-imag 'rectangular) x y))

(#%provide install-polar-package)
(define (install-polar-package alg)
  ;; internal procedures
  (define (magnitude z) (cadr z))
  (define (angle z) (caddr z))
  (define (make-from-mag-ang m a) (list 'polar m a))
  (define (re-part z)
    (* (magnitude z) (cos (angle z))))
  (define (im-part z)
    (* (magnitude z) (sin (angle z))))
  
  ;; interface to rest of the system
  (put alg
       (make-rule 'magnitude         'polar magnitude)
       (make-rule 'angle             'polar angle)
       (make-rule 're-part           'polar re-part)
       (make-rule 'im-part           'polar im-part)
       (make-rule 'make-from-mag-ang 'polar make-from-mag-ang)))

(#%provide make-complex-from-real-imag)
(define (make-complex-from-real-imag alg x y)
  ((get alg 'make-from-real-imag 'complex) x y))

(#%provide make-complex-from-mag-ang)
(define (make-complex-from-mag-ang alg r a)
  ((get alg 'make-from-mag-ang 'complex) r a))

