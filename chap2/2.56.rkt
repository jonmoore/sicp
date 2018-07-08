#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))


;; define the core algebra that's taken as assumed later -- we won't
;; bother customizing this.
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; define a customizable algebra for sums
(define (sum-alg-basic sym)
  (define (make-sum a1 a2)
    (list '+ a1 a2))
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (cdr (assoc sym
              (list (cons 'make make-sum)
                    (cons '? sum?)
                    (cons 'addend addend)
                    (cons 'augend augend)))))

(define (sum-alg-reducing sym)
  (define (make-sum-reducing a1 a2)
    ;; A smell here is that the result of make-sum-reducing is not
    ;; itself a sum according to sum?
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (if (eq? sym 'make)
      make-sum-reducing
      (sum-alg-basic sym)))

;; similarly derive a customizable algebra for products
(define (prod-alg-basic sym)
  (define (make-product m1 m2)
    (list '* m1 m2))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (cdr (assoc sym
              (list (cons 'make make-product)
                    (cons '? product?)
                    (cons 'multiplier multiplier)
                    (cons 'multiplicand multiplicand)))))

(define (prod-alg-reducing sym)
  (define (make-product-reducing m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else
           (list '* m1 m2))))
  (if (eq? sym 'make)
      make-product-reducing
      (prod-alg-basic sym)))

;; let's generalize deriv to work with a list of rules to handle
;; different kinds of expressions
(define (make-deriv-rule test deriver) (cons test deriver))
(define (deriv-rule-test rule) (car rule))
(define (deriv-rule-deriver rule) (cdr rule))

(define (make-deriver rules)
  (define (deriv exp var)
    (define (deriv-using rules)
      (if (null? rules)
          (error "unknown expression type -- DERIV" exp)
          (let ((rule (car rules)))
            (if ((deriv-rule-test rule) exp)
                ((deriv-rule-deriver rule) exp var deriv)
                (deriv-using (cdr rules))))))
    (deriv-using rules))
  deriv)

;; create some rules to differentiate with.  These use expression
;; algebras and the generalized deriv framework, both defined above.
(define number-rule (make-deriv-rule
                     number?
                     (lambda (exp var deriv) 0)))
(define variable-rule (make-deriv-rule
                       variable?
                       (lambda (exp var deriv)
                         (if (same-variable? exp var) 1 0))))
(define (make-sum-rule alg)
  (let ((sum? (alg '?))
        (make-sum (alg 'make))
        (addend (alg 'addend))
        (augend (alg 'augend)))
    (make-deriv-rule
     sum?
     (lambda (exp var deriv)
       (make-sum (deriv (addend exp) var)
                 (deriv (augend exp) var))))))

(define (make-product-rule sum-alg prod-alg)
  (let ((make-sum (sum-alg 'make))
        (product? (prod-alg '?))
        (make-product (prod-alg 'make))
        (multiplier (prod-alg 'multiplier))
        (multiplicand (prod-alg 'multiplicand)))
    (make-deriv-rule
     product?
     (lambda (exp var deriv)
       (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp)))))))

;; now make some different versions of deriv
(define deriv-1 (make-deriver
                 (list number-rule
                       variable-rule
                       (make-sum-rule sum-alg-basic)
                       (make-product-rule sum-alg-basic prod-alg-basic))))

(define deriv-2 (make-deriver
                 (list number-rule
                       variable-rule
                       (make-sum-rule sum-alg-reducing)
                       (make-product-rule sum-alg-reducing prod-alg-basic))))

(define deriv-3 (make-deriver
                 (list number-rule
                       variable-rule
                       (make-sum-rule sum-alg-reducing)
                       (make-product-rule sum-alg-reducing prod-alg-reducing))))

(module+ main
  (displn-eval '(deriv-1 '(+ x 3) 'x) ns)
  (displn-eval '(deriv-1 '(* x 3) 'x) ns)
  (displn-eval '(deriv-1 '(* x y) 'x) ns)

  (displn-eval '(deriv-2 '(+ x 3) 'x) ns)
  (displn-eval '(deriv-2 '(* x 3) 'x) ns)
  (displn-eval '(deriv-2 '(* x y) 'x) ns)

  (displn-eval '(deriv-3 '(+ x 3) 'x) ns)
  (displn-eval '(deriv-3 '(* x 3) 'x) ns)
  (displn-eval '(deriv-3 '(* x y) 'x) ns))

;; not for 2.56. Show how to extend the basic differentiator to handle
;; more kinds of expressions. For instance, implement the
;; differentiation rule
;;
;; d(u^n)dx=n u^(n-1)du/dx

;; by adding a new clause to the deriv program and defining
;; appropriate procedures exponentiation?, base, exponent, and
;; make-exponentiation. (You may use the symbol ** to denote
;; exponentiation.) Build in the rules that anything raised to the
;; power 0 is 1 and anything raised to the power 1 is the thing
;; itself.

;; proceed as for products, starting with an exponent algebra
(define (exp-alg-basic sym)
  (define (make-exponent base exponent)
    (list '** base exponent))
  (define (exponent? x)
    (and (pair? x) (eq? (car x) '**)))
  (define (base e) (cadr e))
  (define (exponent e) (caddr e))
  (cdr (assoc sym
              (list (cons 'make make-exponent)
                    (cons '? exponent?)
                    (cons 'base base)
                    (cons 'exponent exponent)))))

(define (exp-alg-reducing sym)
  (define (make-exponent-reducing base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else
           ((exp-alg-basic 'make) base exponent))))
  (if (eq? sym 'make)
      make-exponent-reducing
      (exp-alg-basic sym)))

(define (make-exponent-rule prod-alg exp-alg)
  (let ((exponent? (exp-alg '?))
        (exponent (exp-alg 'exponent))
        (base (exp-alg 'base))
        (make-exponent (exp-alg 'make))
        (make-product (prod-alg 'make)))
    (make-deriv-rule
     exponent?
     (lambda (exp var deriv)
       (make-product
        (exponent exp) ;; n
        (make-product
         (make-exponent (base exp) (dec (exponent exp))) ;; u^(n-1)
         (deriv (base exp) var) ;; du/dx
         ))))))
;; the above only works for numerical exponents (note the use of dec,
;; but see also below which shows how we could extend this)
;;
;; D (x ^ y)
;; = D ( exp (logx * y))
;; = exp(logx *y) D(logx *y)
;; = x^y * (D(x)/x*y


(define deriv-exp (make-deriver
                   (list number-rule
                         variable-rule
                         (make-sum-rule sum-alg-reducing)
                         (make-product-rule sum-alg-reducing prod-alg-reducing)
                         (make-exponent-rule prod-alg-reducing exp-alg-reducing))))

(module+ main
  (begin-example "2.56")
  (displn-eval '(deriv-exp '(** x 3) 'x) ns)
  (displn-eval '(deriv-exp '(+ (* 4 (** x 2)) (** x 3)) 'x) ns))


;; Exercise 2.57: Extend the differentiation program to handle sums
;; and products of arbitrary numbers of (two or more) terms. Then the
;; last example above could be expressed as

;; (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and
;; products, without changing the deriv procedure at all. For example,
;; the addend of a sum would be the first term, and the augend would
;; be the sum of the rest of the terms.

(define (sum-alg-n-ary sym)
  (define (make-sum . addends)
    (define (partition-numeric) (partition number? addends))
    (define (process-partitioned numeric non-numeric)
      (let ((numeric-sum (sum numeric)))
        (if (= numeric-sum 0)
            non-numeric
            (cons numeric-sum non-numeric))))
    (let ((processed-addends (call-with-values partition-numeric process-partitioned)))
      (cond
       ((= (length processed-addends) 0) 0)
       ((= (length processed-addends) 1) (car processed-addends))
       (else (cons '+ processed-addends)))))
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (addend s) (cadr s))
  (define (augend s)
    (cond
     ((not (sum? s)) (error "augend only works with sum expressions: received " s))
     ((= (length s) 2) 0)
     ((= (length s) 3) (caddr s))
     ((> (length s) 3)
      (apply make-sum (cddr s)))
     (else (error "bad expression passed to augend"))))
  (cdr (assoc sym
              (list (cons 'make make-sum)
                    (cons '? sum?)
                    (cons 'addend addend)
                    (cons 'augend augend)))))

(define (prod-alg-n-ary sym)
  (define (make-prod . multiplicands)
    (define (partition-numeric) (partition number? multiplicands))
    (define (process-partitioned numeric non-numeric)
      (let ((numeric-product (product numeric)))
        (cond ((= numeric-product 0) '(0))
              ((= numeric-product 1) non-numeric)
              (else
               (cons numeric-product non-numeric)))))
    (let ((processed-multiplicands (call-with-values partition-numeric process-partitioned)))
      (cond
       ((= (length processed-multiplicands) 0) 1)
       ((= (length processed-multiplicands) 1) (car processed-multiplicands))
       (else (cons '* processed-multiplicands)))))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (multiplier s) (cadr s))
  (define (multiplicand s)
    (cond
     ((not (product? s)) (error "multiplicand only works with product expressions: received " s))
     ((= (length s) 2) 1)
     ((= (length s) 3) (caddr s))
     ((> (length s) 3)
      (apply make-prod (cddr s)))
     (else (error "bad expression passed to multiplicand"))))
  (cdr (assoc sym
              (list (cons 'make make-prod)
                    (cons '? product?)
                    (cons 'multiplier multiplier)
                    (cons 'multiplicand multiplicand)))))

(module+ main
  (displn-eval '((sum-alg-n-ary 'make) 1 2 3 'x) ns)
  (displn-eval '((sum-alg-n-ary 'augend) ((sum-alg-n-ary 'make) 1 2 3 'x)) ns)
  (displn-eval '((sum-alg-n-ary 'make) 1 2 3) ns)
  (displn-eval '((sum-alg-n-ary 'make) 'a 'b 'c) ns)
  (displn-eval '((sum-alg-n-ary 'augend) ((sum-alg-n-ary 'make) 'a 'b 'c)) ns)
  (displn-eval '((sum-alg-n-ary 'augend) ((sum-alg-n-ary 'make) 'b 'c)) ns)
  )

(define deriv-n-ary (make-deriver
                     (list number-rule
                           variable-rule
                           (make-sum-rule sum-alg-n-ary)
                           (make-product-rule sum-alg-n-ary prod-alg-n-ary)
                           (make-exponent-rule prod-alg-n-ary exp-alg-reducing)
                           )))

(module+ main
  (begin-example "2.57")
  (displn-eval '(deriv-n-ary '(** x 3) 'x) ns)
  (displn-eval '(deriv-n-ary '(+ (* (+ 1 1) x) (* 2 x) (* 4 x)) 'x) ns)
  (displn-eval '(deriv-n-ary '(* x y (+ x 3)) 'x) ns)
  )

;; define a customizable algebra for infix sums
(define (sum-alg-basic-infix sym)
  (define (make-sum a1 a2)
    (list a1 '+ a2))
  (define (sum? x)
    (and (pair? x) (eq? (cadr x) '+)))
  (define (addend s) (car s))
  (define (augend s) (caddr s))
  (cdr (assoc sym
              (list (cons 'make make-sum)
                    (cons '? sum?)
                    (cons 'addend addend)
                    (cons 'augend augend)))))

;; similarly derive a customizable algebra for infix products
(define (prod-alg-basic-infix sym)
  (define (make-product m1 m2)
    (list m1 '* m2))
  (define (product? x)
    (and (pair? x) (eq? (cadr x) '*)))
  (define (multiplier p) (car p))
  (define (multiplicand p) (caddr p))
  (cdr (assoc sym
              (list (cons 'make make-product)
                    (cons '? product?)
                    (cons 'multiplier multiplier)
                    (cons 'multiplicand multiplicand)))))

(define deriv-basic-infix
  (make-deriver
   (list number-rule
         variable-rule
         (make-sum-rule sum-alg-basic-infix)
         (make-product-rule sum-alg-basic-infix prod-alg-basic-infix))))

(define (maybe-make-sum-numbers a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else #f)))

(define (first-passing fns)
  (lambda args
    (if (null? fns)
        #f
        (or (apply (car fns) args)
            (apply (first-passing (cdr fns)) args)))))

(define (sum-alg-reducing-infix sym)
  (if (eq? sym 'make)
      (first-passing (list maybe-make-sum-numbers (sum-alg-basic-infix sym)))
      (sum-alg-basic-infix sym)))

(define (maybe-make-prod-numbers m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else #f)))

(define (prod-alg-reducing-infix sym)
  (if (eq? sym 'make)
      (first-passing (list maybe-make-prod-numbers (prod-alg-basic-infix sym)))
      (prod-alg-basic-infix sym)))

(define deriv-reducing-infix
  (make-deriver
   (list number-rule
         variable-rule
         (make-sum-rule sum-alg-reducing-infix)
         (make-product-rule sum-alg-reducing-infix prod-alg-reducing-infix))))

(module+ main
  (begin-example "2.58 1")
  (displn-eval '(deriv-basic-infix '(x + (3 * (x + (y + 2)))) 'x) ns)
  (displn-eval '(deriv-reducing-infix '(x + (3 * (x + (y + 2)))) 'x) ns))

(define (plus? term) (eq? term '+))
(define (times? term) (eq? term '*))

;; define a customizable algebra for infix sums
(define (remove-parens exp)
  (if (and (pair? exp) (= (length exp) 1))
      (remove-parens (car exp))
      exp))

(define number-rule-2 (make-deriv-rule
                       (lambda (exp) (number? (remove-parens exp)))
                       (lambda (exp var deriv) 0)))
(define variable-rule-2 (make-deriv-rule
                         (lambda (exp) (variable? (remove-parens exp)))
                         (lambda (exp var deriv)
                           (if (same-variable? (remove-parens exp) var) 1 0))))

(define (sum-alg-extended-infix sym)
  (define (make-sum a1 a2)
    (list a1 '+ a2))
  (define (sum? x) (and (pair? x) (any plus? x)))
  (define (addend s) (take-while (compose not plus?) s))
  (define (augend s) (cdr (memq '+ s)))
  (cdr (assoc sym
              (list (cons 'make make-sum)
                    (cons '? (compose sum? remove-parens))
                    (cons 'addend (compose addend remove-parens))
                    (cons 'augend (compose augend remove-parens))))))

(define (prod-alg-extended-infix sym)
  (define (make-product a1 a2)
    (list a1 '* a2))
  (define (times? term) (eq? term '*))
  (define (product? x) (and (pair? x)
                            (not (any plus? x))
                            (any times? x)))
  (define (multiplier p) (take-while (compose not times?) p))
  (define (multiplicand p) (cdr (memq '* p)))
  (cdr (assoc sym
              (list (cons 'make make-product)
                    (cons '? (compose product? remove-parens))
                    (cons 'multiplier (compose multiplier remove-parens))
                    (cons 'multiplicand (compose multiplicand remove-parens))))))

(define (sum-alg-extended-reducing-infix sym)
  (if (eq? sym 'make)
      (first-passing (list maybe-make-sum-numbers (sum-alg-extended-infix sym)))
      (sum-alg-extended-infix sym)))

(define (prod-alg-extended-reducing-infix sym)
  (if (eq? sym 'make)
      (first-passing (list maybe-make-prod-numbers (prod-alg-extended-infix sym)))
      (prod-alg-extended-infix sym)))

(define deriv-extended-infix
  (make-deriver
   (list number-rule-2
         variable-rule-2
         (make-sum-rule sum-alg-extended-reducing-infix)
         (make-product-rule sum-alg-extended-reducing-infix prod-alg-extended-reducing-infix))))

(module+ main
  (begin-example "2.58")
  (displn-eval '(deriv-extended-infix '(y + 2) 'x) ns)
  (displn-eval '(deriv-extended-infix '(x + 2) 'x) ns)
  (displn-eval '(deriv-extended-infix '(y + x + 2) 'x) ns)
  (displn-eval '(deriv-extended-infix '(( x  * y ) + 2) 'x) ns)
  (displn-eval '(deriv-extended-infix '(z + y + ( x  * y ) + 2) 'x) ns)
  (displn-eval '(deriv-extended-infix '(z + y + ( x  * y * y ) + 2) 'x) ns)
  (displn-eval '(deriv-extended-infix '(z + y + ( x  * y * y ) + 2) 'y) ns)
  )

;; Clearly there is further to go here, e.g. (1) make the treatment of
;; parenthesised expressions systematic by adding them as an extra
;; expression type rather than removing them ad-hoc (2) having
;; systematic rules for applying simplifications in the make phases,
;; e.g. using associativity, commutativity and distributivity.
