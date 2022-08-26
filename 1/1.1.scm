#lang scheme

;;; 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;;; 1.3
(define (square x)
  (* x x))

(define (square-of-large-numbers x y z)
  (cond
    [(and (> x y) (> y z)) (* (square x) (square y))]
    [(and (> x y) (> z y)) (* (square x) (square z))]
    [(and (> y x) (> z x)) (* (square y) (square z))]
    [(and (> y x) (> x z)) (* (square y) (square x))]))

;; 1.4

;; let's substitute the following procedure
;; first (if (> b 0) + -) returns + if b > 0 and - if b < 0
;; if we assume b = -1 and a=2 it will be -
;; which will make the procedure (- 2 -1)
;; which will be 3

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; 1.5

(define (p)
  (p))
(define (test x y)
  (if (= x 0) 0 y))

;; in the applicative order the interpreter can't fully
;; evaluate (p) because of it recursively returns itself
;; in the normal-order evaluation(expand and reduce) it doesn't reach (p) because (= x 0) evaluates #t
