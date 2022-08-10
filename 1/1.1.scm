#lang scheme

;; https://eli.thegreenplace.net/2007/06/21/sicp-section-11

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

;; 1.6

;; It would recursively run sqrt-iter and would never finish. This is because of applicative order execution

;; 1.7
(define (abs x)
  ((if (> x 0) + -) x))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess old-guess)
  (and (> (/ guess old-guess) 0.999) (< (/ guess old-guess) 1.001)))
(define (improve-guess x guess)
  (average guess (/ guess x)))
(define (sqrt-iter x guess old-guess)
  (if (good-enough? guess old-guess) guess (sqrt-iter x (improve-guess x guess) guess)))

(define (sqrt x)
  (sqrt-iter x 1.0 10))

;; 1.8
(define (good-enough-cube? guess old-guess)
  (and (> (/ guess old-guess) 0.999) (< (/ guess old-guess) 1.001)))
(define (improve-guess-cube x guess)
  (/ (+ (* 2 guess) (/ x (square guess))) 3))
(define (cube-iter x guess old-guess)
  (if (good-enough-cube? guess old-guess) guess (cube-iter x (improve-guess-cube x guess) guess)))
(define (cube x)
  (cube-iter x 1.0 10))
