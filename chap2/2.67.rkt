#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (type-tag object)
  (car object))

(define (has-type-tag object tag)
  (if (not (symbol? tag))
      (error "Tag must be a symbol. Got" tag)
      (eq? (type-tag object) tag)))

(define (require-type-tag object tag)
  (or (has-type-tag object tag)
      (error "Object does not have expected type tag" object tag)))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (has-type-tag object 'leaf))

(define (symbol-leaf x)
  (and (require-type-tag x 'leaf)
       (cadr x)))

(define (weight-leaf x)
  (and (require-type-tag x 'leaf)
       (caddr x)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf
                     (car pair)         ; symbol
                     (cadr pair))       ; weight
                    (make-leaf-set (cdr pairs))))))

;; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(module+ main
  (begin-example "2.67")
  (displn-eval '(decode sample-message sample-tree) ns)
  (displn-eval 'sample-tree ns))

;; 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (in-symbols? symbol tree)
  (memq symbol (symbols tree)))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      (if (eq? (symbol-leaf tree) symbol)
          '()
          (error "Could not encode symbol given tree" symbol tree))
      (cond ((in-symbols? symbol (left-branch tree))
             (cons 0 (encode-symbol symbol (left-branch tree))))
            ((in-symbols? symbol (right-branch tree))
             (cons 1 (encode-symbol symbol (right-branch tree))))
            (else (error "could not encode symbol given tree" symbol tree)))))

(define (round-trip message tree)
  (decode (encode message tree) tree))

(module+ main
  (begin-example "2.68")
  (displn-eval '(encode '(A D A B B C A) sample-tree) ns)
  (displn-eval '(round-trip '(A D A B B C A) sample-tree) ns))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (lightest tree) (car tree))
(define (next-lightest tree) (cadr tree))
(define (heavy-elements tree) (cddr tree))

;; leaf-set is a set of leaves, thus also a set of trees
(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (lightest leaf-set)
      (successive-merge (adjoin-set
                         (make-code-tree (lightest leaf-set) (next-lightest leaf-set))
                         (heavy-elements leaf-set)))))

(module+ main
  (begin-example "2.69")
  (generate-huffman-tree '((A 4) (B 2) (D 1) (C 1))))

;; 2.70
(define 50s-pairs
   '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define 50s-tree
  (generate-huffman-tree 50s-pairs))

(define 50s-lyrics
     '(GET A JOB
         SHA NA NA NA NA NA NA NA
         GET A JOB 
         SHA NA NA NA NA NA NA NA
         WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
         SHA BOOM))
(module+ main
  (begin-example "2.70")
  (encode 50s-lyrics 50s-tree)
  (displn-eval '(length (encode 50s-lyrics 50s-tree)) ns)
  (displn-eval '(length 50s-lyrics) ns)
  (displn-eval '(length 50s-pairs) ns)
  )

;; 82 bits are required.  There are 34 words to encode, which at 3
;; bits per word would take 102 bits rather than 82

;; 2.71

;; Each level of encoding will have the most common symbol as the sole
;; symbol in the left tree and the rest in the right tree.  First
;; symbol uses 1 bit, 2nd 2, ... (n-1)th symbol n-1 bits, nth symbol
;; n-1 bits.


;; 2.72

;; under somewhat optimistic assumptions (balanced trees at each node)
;; we'd expect a symbol of frequency 1/2^k to be located k steps down,
;; with a very optimistic estimate (equal frequencies) of O(n)
;; comparisons to be made in total as number of symbols at each level
;; goes down by 1/2 each level.  Very pessimistically we could assume
;; kn comparisons.  The case for 2.71 is closer to this since the
;; number of comparisons at each level goes n-k+1, n-k, n-k-1, ....
;; Expect O(n) comparisons on average in this case (sum of k/2^k =
;; O(1))

