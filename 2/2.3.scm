#lang sicp

; (#%require "../1/1.1.scm")
; (#%require "../1/1.2.scm")
; (#%require "./2.1.scm")
; (#%require "./2.2.scm")

;;;
(define (memq item x)
  (cond
    [(null? item) false]
    [(eq? item (car x)) x]
    [else (memq item (cdr x))]))

;;; 2.54
(define (equal? list1 list2)
  (cond
    [(and (null? list1) (null? list2)) true]
    [(or (null? list1)
         (null? list2)
         (not (symbol? (car list1)))
         (not (symbol? (car list2)))
         (not (eq? (car list1) (car list2))))
     false]
    [else (equal? (cdr list1) (cdr list2))]))

;;; 2.55
;;; ''abracadabra can be rewritten as (quote (quote abracadabra))
;;; which when we apply car returns 'quote

;;; 2.56
(define (deriv expr var)
  (cond
    [(number? expr) 0]
    [(variable? expr) (if (same-variable? expr var) 1 0)]
    [(exponentiation? expr)
     (make-product-infix (make-product-infix (exponent expr)
                                             (make-exponentiation (base expr) (- expr 1)))
                         (deriv base))]
    [(sum-infix? expr)
     (make-sum-infix (deriv (addend-infix expr) var) (deriv (augend-infix expr) var))]
    [(product-infix? expr)
     (make-sum-infix
      (make-product-infix (multiplier-infix expr) (deriv (mulitplicand-infix expr) var))
      (make-product-infix (mulitplicand-infix expr) (deriv (multiplier-infix expr) var)))]
    [else (error "unknown expression type: DERIV" exp)]))

(define (=number? expr n)
  (and (number? expr) (number? n) (= expr n)))
(define (variable? expr)
  (symbol? expr))
(define (same-variable? expr var)
  (eq? expr var))

(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))
(define (base expr)
  (cadr expr))
(define (exponent expr)
  (caddr expr))
(define (make-exponentiation a b)
  (cond
    [(=number? a 1) 1]
    [(=number? b 0) 1]
    [(=number? b 1) a]
    [(and (number? a) (make-exponentiation (* a a) (- b 1)))]
    [else (list '** a b)]))

;;; 2.57
(define (sum? expr)
  (and (pair? expr) (eq? (car expr) '+)))
(define (product? expr)
  (and (pair? expr) (eq? (car expr) '*)))
(define (make-sum a b)
  (cond
    [(=number? a 0) b]
    [(=number? b 0) a]
    [(and (number? a) (number? b)) (+ a b)]
    [else (list '+ a b)]))
(define (make-product a b)
  (cond
    [(or (=number? a 0) (=number? b 0)) 0]
    [(=number? a 1) b]
    [(=number? b 1) a]
    [(and (number? a) (number? b)) (* a b)]
    [else (list '* a b)]))
(define (addend expr)
  (cadr expr))
(define (augend expr)
  (if (> 3 (length expr)) (make-sum (addend (cdr expr) (augend (cdr expr)))) (caddr expr)))
(define (multiplier expr)
  (cadr expr))
(define (mulitplicand expr)
  (if (> (length expr) 3)
      (make-product (multiplier (cdr expr)) (mulitplicand (cdr expr)))
      (caddr expr)))

;;; 2.58
(define (sum-infix? expr)
  (and (pair? expr) (eq? (cadr expr) '+)))
(define (product-infix? expr)
  (and (pair? expr) (eq? (cadr expr) '*)))
(define (make-sum-infix a b)
  (cond
    [(=number? a 0) b]
    [(=number? b 0) a]
    [(and (number? a) (number? b)) (+ a b)]
    [else (list a '+ b)]))
(define (make-product-infix a b)
  (cond
    [(or (=number? a 0) (=number? b 0)) 0]
    [(=number? a 1) b]
    [(=number? b 1) a]
    [(and (number? a) (number? b)) (* a b)]
    [else (list a '* b)]))
(define (addend-infix expr)
  (car expr))
(define (augend-infix expr)
  (if (> 3 (length expr)) (make-sum (addend (cddr expr) (augend (cddr expr)))) (caddr expr)))
(define (multiplier-infix expr)
  (car expr))
(define (mulitplicand-infix expr)
  (if (> (length expr) 3)
      (make-product (multiplier (cddr expr)) (mulitplicand (cddr expr)))
      (caddr expr)))

;; common
(define (element-of-set-unordered? x set)
  (cond
    [(null? set) false]
    [(equal? x (car set)) true]
    [else (element-of-set-unordered? x (cdr set))]))

(define (adjoin-set-unordered x set)
  (if (element-of-set-unordered? x set) set (cons x set)))
(define (intersection-set-unordered set1 set2)
  (cond
    [(or (null? set1) (null? set2)) '()]
    [(element-of-set-unordered? (car set1) set2)
     (cons (car set1) (intersection-set-unordered (cdr set1) set2))]
    [else (intersection-set-unordered (cdr set1) set2)]))

;; 2.59
(define (union-set-unordered set1 set2)
  (cond
    [(or (null? set1) (null? set2)) '()]
    [(element-of-set-unordered? (car set1) set2) (union-set-unordered (cdr set1) set2)]
    [else (cons (car set1) (union-set-unordered (cdr set1) set2))]))

;;; 2.60
(define (adjoin-set-duplicate x set)
  (cons x set))
(define (intersection-set-duplicate set1 set2)
  (cond
    [(or (null? set1) (null? set2)) '()]
    [else (cons (car set1) (intersection-set-duplicate (cdr set1) set2))]))
(define (union-set-duplicate set1 set2)
  (cond
    [(or (null? set1) (null? set2)) '()]
    [else (cons (car set1) (union-set-duplicate (cdr set1) set2))]))

;; common
(define (element-of-set-ordered? x set)
  (cond
    [(null? set) false]
    [(= x (car set)) true]
    [(< x (car set)) false]
    [else (element-of-set-ordered? x (cdr set))]))

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ([x1 (car set1)]
            [x2 (car set2)])
        (cond
          [(= x1 x2) (cons x1 (intersection-set-ordered (cdr set1) (cdr set2)))]
          [(< x1 x2) (intersection-set-ordered (cdr set1) set2)]
          [(< x2 x1) (intersection-set-ordered set1 (cdr set2))]))))

;; 2.61
(define (adjoin-set-ordered x set)
  (cond
    [(null? set) '(x)]
    [(= x (car set)) set]
    [(< x (car set)) (cons (car set) (adjoin-set-ordered x (cdr set)))]
    [(> x (car set)) (cons (car set) x (cdr set))]))

;; 2.62
(define (union-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ([x1 (car set1)]
            [x2 (car set2)])
        (cond
          [(= x1 x2) (cons x1 (union-set-ordered (cdr set1) (cdr set2)))]
          [(< x1 x2) (cons x1 (union-set-ordered (cdr set1) set2))]
          [(< x2 x1) (cons x2 (union-set-ordered set1 (cdr set2)))]))))

;; common
(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond
    [(null? set) false]
    [(= x (entry set)) true]
    [(< x (entry set)) (element-of-set? x (left-branch set))]
    [(> x (entry set)) (element-of-set? x (right-branch set))]))

(define (adjoin-set x set)
  (cond
    [(null? set) (make-tree x '() '())]
    [(= x (entry set)) set]
    [(< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set))]
    [(> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set)))]))

;;; 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

;; a - yes. Inorder traversal
;; b - no. both are o(n)

;;; 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ([left-size (quotient (- n 1) 2)])
        (let ([left-result (partial-tree elts left-size)])
          (let ([left-tree (car left-result)]
                [non-left-elts (cdr left-result)]
                [right-size (- n (+ left-size 1))])
            (let ([this-entry (car non-left-elts)]
                  [right-result (partial-tree (cdr non-left-elts) right-size)])
              (let ([right-tree (car right-result)]
                    [remaining-elts (cdr right-result)])
                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

;; a - The partial-tree function works by dividing the ordered list into two
;;     list the left list and the right list. This is done by making the left list's
;;     size (n-1)/2 and the right lists size n - ((n-1)/2 - 1). The -1 is needed for
;;     the actual entry. Because there is no way to return the elements that did not
;;     make it to the actual(left or right) tree we need to return a pair containg the
;;     result and remaining elements.
;;     for the ordered list (1 3 5 7 9 11) the function makes
;;                          5
;;                      1     9
;;                       3  7   11

;; b - o(n)

;;; 2.65
(define (intersection-set-balanced set1 set2)
  (list->tree (intersection-set-ordered (tree->list-1 set1) (tree->list-2 set2))))
(define (union-set-balanced set1 set2)
  (list->tree (union-set-ordered (tree->list-1 set1) (tree->list-2 set2))))

;;; 2.66
; (define (lookup given-key set-of-records)
;   (cond
;     [(null? set-of-records) false]
;     [(= given-key (key (entry set-of-records))) (entry set-of-records)]
;     [(< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records))]
;     [(= given-key (key (entry set-of-records))) (lookup given-key (right-branch set-of-records))]))

;; common
;; huffman encoding

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x)
  (cadr x))
(define (weight-leaf x)
  (caddr x))

(define (left-branch-huffman tree)
  (car tree))
(define (right-branch-huffman tree)
  (cadr tree))
(define (symbols tree)
  (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))
(define (weight tree)
  (if (leaf? tree) (weight-leaf tree) (cadddr tree)))

(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))

(define (choose-branch bit branch)
  (cond
    [(= bit 0) (left-branch-huffman branch)]
    [(= bit 1) (right-branch-huffman branch)]
    [else (error "bad bit: CHOOSE-BRANCH" bit)]))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set-huffman x set)
  (cond
    [(null? set) (list x)]
    [(< (weight x) (weight (car set))) (cons x set)]
    [else (cons (car set) (adjoin-set-huffman x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set-huffman (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs))))))

;; from https://eli.thegreenplace.net/2007/09/12/sicp-section-234
(define (display-huffman-tree tree)
  (define (space-offset offset)
    (if (<= offset 0) "" (string-append " " (space-offset (- offset 1)))))
  (define (print-node node offset)
    (display (space-offset offset))
    (if (leaf? node)
        (begin
          (display (symbol-leaf node))
          (display " ")
          (display (weight-leaf node))
          (newline))
        (begin
          (display (symbols node))
          (display " ")
          (display (weight node))
          (newline)
          (print-node (left-branch-huffman node) (+ 2 offset))
          (print-node (right-branch-huffman node) (+ 2 offset)))))

  (print-node tree 0))

;; 2.67

#|
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) = '(A D A B B C A)
|#

;; 2.68
(define (encode message tree)
  (if (null? message) '() (append (encode-symbol (car message) tree) (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (define (includes-symbol? symbol set)
    (cond
      [(null? set) false]
      [(eq? symbol (car set)) true]
      [else (includes-symbol? symbol (cdr set))]))
  (cond
    [(leaf? tree) '()]
    [(includes-symbol? symbol (symbols (left-branch-huffman tree)))
     (cons 0 (encode-symbol symbol (left-branch-huffman tree)))]
    [(includes-symbol? symbol (symbols (right-branch-huffman tree)))
     (cons 1 (encode-symbol symbol (right-branch-huffman tree)))]
    [else (error "bad symbol: not in tree" symbol)]))

;; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge pairs)
  (if (< (length pairs) 2)
      (car pairs)
      (successive-merge (adjoin-set-huffman (make-code-tree (car pairs) (cadr pairs)) (cddr pairs)))))

;; 2.70

#|
(define rock-50s-tree
  (generate-huffman-tree
    '((a 2) (boom 1) (get 2) (job 2)
      (na 16) (sha 3) (yip 9) (wah 1))))

(encode
    '(get a job sha na na na na na na na na
      get a job sha na na na na na na na na
      wah yip yip yip yip yip yip yip yip yip
      sha boom) rock-50s-tree) = (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
|#

;; 2.71

#|
(display-huffman-tree (generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16))))
(display-huffman-tree (generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512))))

for such a tree
1. to encode the most frequent symbol - 1 bit
2. to encode the least frequent symbol - n-1 bits
|#

;; 2.72

#|
for the most frequent symbol - O(n)
for the most least frequent symbol - O(n^2)
|#
