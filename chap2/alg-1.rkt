#lang sicp
(#%require (only racket module+))
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

;; basics for an algebra to support multiple dispatch / data-directed
;; programming.  apply-generic is defined elsewhere.
;;

(#%provide type-tag)
(define (type-tag obj)
  (cond ((number? obj) 'scheme-number)
        ((not (pair? obj)) (error "obj must be a number or a pair" obj))
        ((not (symbol? (car obj))) (error "(car obj) must be a symbol" obj))
        (else (car obj))))
(#%provide contents)
(define (contents obj)
  (cond
   ((number? obj) obj)
   ((not (pair? obj)) (error "obj must be a number or a pair" obj))
   (else (cdr obj))))
(#%provide attach-tag)
(define (attach-tag tag obj)
  (cond ((eq? tag 'scheme-number)
         (if (number? obj)
             obj
             (error "trying to attach scheme-number to non-number -- attach-tag " tag obj)))
        (else
         (cons tag obj))))

(define (canonical-types types)
  (cond ((symbol? types) (list types))
        ((list? types) types)
        (error "Expected symbol or list -- CANONICAL-TYPES" types)))

(define (rule-class rule)
  (car rule))
(define (rule-op rule)
  (cadr rule))
(define (rule-types rule)
  (canonical-types (caddr rule)))
(define (rule-function rule alg)
  (define (bind-alg-arg fun alg)
    (lambda args
      (apply fun alg args)))
  ;; bind alg to the stored function
  (bind-alg-arg (cadddr rule) alg))

(#%provide make-rec-rule)
(define (make-rec-rule op types function)
  ;; function should take alg as its first argument
  (list 'rec-rule op types function))

(#%provide make-rule)
(define (make-rule op types function)
  (define (taking-alg-arg fun)
    ;; Add an ignored alg argument to fun
    (lambda (alg . rest)
      (apply fun rest)))
  (make-rec-rule op types (taking-alg-arg function)))

(#%provide put)
(define (put alg . rules)
  (append rules alg))

(#%provide get)
(define (get alg op types)
  (define (matches rule)
    (and (eq? (rule-op rule) op)
         (equal? (rule-types rule) (canonical-types types))))
  (let ((matched (find-tail matches alg)))
    (cond (matched (rule-function (car matched) alg))
          (else #f))))

(#%provide op-types)
(define (op-types alg op)
  "Return a list of type lists supported for the given operator"
  (map rule-types
       (filter
        (lambda (rule)
          (eq? (rule-op rule) op))
        alg)))

(#%provide empty-alg)
(define (empty-alg) '())

