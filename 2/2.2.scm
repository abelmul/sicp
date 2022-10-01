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
(define (map proc items)
  (if (null? items) nil (cons (proc (car items)) (map proc (cdr items)))))

(define (square-list items)
  (if (null? items) nil (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

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
