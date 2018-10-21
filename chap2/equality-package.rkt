#lang sicp
(#%require (only racket module+))
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt"  "alg-1.rkt")

(#%provide install-equ)
(define (install-equ alg)
  ;; installs under equ? an equality operator that compared equal only
  ;; when the types are equal.  We rely on equal? so this is not quite
  ;; right for rationals but could be fixed easily, e.g. through
  ;; dispatching
  (define (equ? a b)
    (let ((type-a (type-tag a))
          (type-b (type-tag b)))
      (and (eq? type-a type-b)
           (equal? (contents a) (contents b)))))

  (let* ((type-tags '(polar rectangular complex scheme-number rational))
         (rules-list-of-lists (map (lambda (type1) 
                                     (map (lambda (type2)
                                            (make-rule 'equ? (list type1 type2) equ?))
                                          type-tags))
                                   type-tags))
         (rules-list (apply append rules-list-of-lists)))
    (apply put alg rules-list)))

(#%provide make-install-=zero?)
(define (make-install-=zero? apply-generic)
  (define (install-=zero? alg)
    ;; alternatively could use the equ that we have above
    (define (is-magnitude-zero alg x)
      (= (apply-generic alg 'magnitude x) 0))
    
    (put alg
         (make-rule '=zero? '(scheme-number) (lambda (x) (= x 0)))
         (make-rec-rule '=zero? '(rational)
                        (lambda (alg x)
                          (= (apply-generic alg 'numer x) 0)))
         (make-rec-rule '=zero? '(complex) is-magnitude-zero)
         (make-rec-rule '=zero? '(rectangular) is-magnitude-zero)
         (make-rec-rule '=zero? '(polar) is-magnitude-zero)))
  install-=zero?)
