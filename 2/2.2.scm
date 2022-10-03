#lang sicp

(#%require "../1/1.1.scm")

;;; 2.17
(define (last-pair l)
  (if (null? (cdr l)) (car l) (last-pair (cdr l))))

;;; 2.18
(define (reverse l)
  (if (null? l) nil (append (reverse (cdr l)) (list (car l)))))

;;; 2.19
(define no-more? null?)
(define first-denomination car)
(define except-first-denomination cdr)
(define (cc amount coin-values)
  (cond
    [(= amount 0) 1]
    [(or (< amount 0) (no-more? coin-values)) 0]
    [else
     (+ (cc amount (except-first-denomination coin-values))
        (cc (- amount (first-denomination coin-values)) coin-values))]))

;;; 2.20
(define (same-parity . l)
  (define (impl l even ans)
    (if (null? l)
        ans
        (impl (cdr l) even (if (eq? (even? (car l)) even) (append ans (list (car l))) ans))))
  (impl (cdr l) (even? (car l)) (list (car l))))

;;; 2.21
(define (my-map proc items)
  (if (null? items) nil (cons (proc (car items)) (my-map proc (cdr items)))))

(define (square-list items)
  (if (null? items) nil (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (my-map square items))

;;; 2.22
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things) answer (iter (cdr things) (cons (square (car things)) answer))))
  (iter items nil))
;; this produces the answer list in the reverse order because we are adding the first items last
;; by making answer the second argument to cons
(define (square-list-iter-2 items)
  (define (iter things answer)
    (if (null? things) answer (iter (cdr things) (cons answer (square (car things))))))
  (iter items nil))
;; this doesn't produces a valid list because it cons nil at the first iteration
;; we can use append to overcome this (append answer (list (square (car things))))
;; which produces the correct answer

;;; 2.23
(define (for-each proc items)
  (cond
    [(null? items) nil]
    [else
     (proc (car items))
     (for-each proc (cdr items))]))

;; 2.24

;; (list 1 (list 2 (list 3 4)))
;; (1 (2 (3 4)))
;; 1,->2,->3,4

;;; 2.25

;;; (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))) = 7
;;; (car (car (list (list 7)))) = 7
;;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))) = 7

;;; 2.26

;; (define x (list 1 2 3))
;; (define y (list 4 5 6))

;; (append x y) = (list 1 2 3 4 5 6)
;; (cons x y)  = ((list 1 2 3) 4 5 6)
;; (list x y)  = ((list 1 2 3) (list 4 5 6))

;;; 2.27

(define (deep-reverse l)
  (if (null? l)
      nil
      (append (deep-reverse (cdr l))
              (let ([head (car l)]) (if (pair? head) (list (deep-reverse head)) (list head))))))

;;; 2.28

(define (fringe l)
  (if (null? l) nil (append (if (pair? (car l)) (fringe (car l)) (list (car l))) (fringe (cdr l)))))

;;; 2.29

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;; a

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr (car mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; b
(define (weight-structure structure)
  (if (pair? structure) (total-weight structure) structure))
(define (total-weight mobile)
  (+ (weight-structure (branch-structure (left-branch mobile)))
     (weight-structure (branch-structure (right-branch mobile)))))

;; c
(define (balanced? mobile)
  (let ([left (left-branch mobile)] [right (right-branch mobile)])
    (and (= (* (branch-length left) (weight-structure left))
            (* (branch-length right) (weight-structure right)))
         (if (pair? left) (balanced? left) #t)
         (if (pair? right) (balanced? right) #t))))

;; d

; if we change the constructors to
; (define (make-mobile left right) (cons left right))
; (define (make-branch length structure) (cons length structure))
; we don't have to reimplement anything since everything works

;;; 2.30

(define (square-tree items)
  (if (null? items)
      nil
      (let ([first (car items)])
        (cons (if (pair? first) (square-tree first) (square first)) (square-tree (cdr items))))))

(define (square-tree-map items)
  (my-map (lambda (item) (if (pair? item) (square-tree-map item) (square item))) items))

;;; 2.31

(define (tree-map proc tree)
  (if (null? tree)
      nil
      (let ([first (car tree)])
        (cons (if (pair? first) (tree-map proc first) (proc first)) (tree-map proc (cdr tree))))))

;;; 2.32

; this code starts at the end of the list and appends the new item to every result before it
; so for example let the set be (1 2 3)
; we begin at empty set (). (subsets nil) = nil
; the we go up the chain the set becomes (3). (subsets (3)) = ((3) nil) = (append (subsets nil) (append (car (3)) (subsets nil)))
; etc

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ([rest (subsets (cdr s))])
        (append rest (my-map (lambda (x) (append (list (car s)) x)) rest)))))

;;; 2.33

(define (accumulate op inital sequence)
  (if (null? sequence) inital (op (car sequence) (accumulate op inital (cdr sequence)))))

(define (filter pridicate? sequence)
  (cond
    [(null? sequence) nil]
    [(pridicate? (car sequence)) (cons (car sequence) (filter pridicate? (cdr sequence)))]
    [else (filter pridicate? (cdr sequence))]))

(define (map-accumulate p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append-accumulate seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-accumulate sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;;; 2.34

(define (horner-eval x sequence)
  (accumulate (lambda (current higher) (+ (* higher x) current)) 0 sequence))

;;; 2.36

; this is the most beautiful code i have ever written yet
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (my-map (lambda (x) (car x)) seqs))
            (accumulate-n op init (my-map (lambda (x) (cdr x)) seqs)))))

;;; 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)]) (map (lambda (x) (if (null? x) nil (matrix-*-vector cols x))) m)))

; (define v (list 1 2 3 4))
; (define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
; (dot-product v v) = 30
; (matrix-*-vector m v) = (30 56 80)
; (transpose m) = ((1 4 6) (2 5 7) (3 5 6) (4 6 9))
; (matrix-*-matrix m (transpose m)) = ((30    56    80) (56   113   161) (80   161   230))

;;; 2.38

(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

; (fold-right / 1 (list 1 2 3))  =  3/2
; (fold-left / 1 (list 1 2 3))  = 1/6
; (fold-right list nil (list 1 2 3)) = (1, (2, (3)))
; (fold-left list nil (list 1 2 3)) = (((nil, 1), 2), 3))

; for fold-right to be equal to fold-left op(a,b) must be equal to op(b,a).
; + and * satisfy this

;;; 2.39

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))
