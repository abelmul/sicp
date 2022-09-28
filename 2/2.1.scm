#lang sicp

(#%require "../1/1.1.scm")
(#%require "../1/1.2.scm")

;;; 2.1
(define (make-rat n d)
  (let ([g (gcd n d)]) (cons (/ (if (or (< n 0) (< d 0)) (- 0 abs (n)) n) g) (/ abs (d) g))))

;;; 2.2
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (mid-segment segment)
  (let ([segstart (start-segment segment)] [segend (end-segment segment)])
    (cons (average (x-point segstart) (x-point segend))
          (average (y-point segstart) (y-point segend)))))

;;; 2.3

(define (area rectangle)
  (* (width rectangle) (height rectangle)))

(define (perimeter rectangle)
  (* 2 (+ (width rectangle) (height rectangle))))

;;; using diagonal points

(define (make-rectangle diagonal-right diagonal-left)
  (cons diagonal-right diagonal-left))

(define (width rectangle)
  (abs (- (x-point (car rectangle)) (x-point (cdr rectangle)))))

(define (height rectangle)
  (abs (- (x-point (car rectangle)) (x-point (cdr rectangle)))))

;;; alternative
;;; using diagonal segment

; (define (make-rectangle diagonal-right diagonal-left)
;   (make-segment diagonal-right diagonal-left))
; (define (width rectangle)
;   (abs (- (x-point (start-segment rectangle))) (- (x-point (end-segment rectangle)))))
; (define (height rectangle)
;   (abs (- (y-point (start-segment rectangle))) (- (y-point (end-segment rectangle)))))

;;; 2.4
(define (my-cons x y)
  (lambda (m) (m x y)))
(define (my-car z)
  (z (lambda (p q) p)))
(define (my-cdr z)
  (z (lambda (p q) q)))

;;; 2.5

(define (cons-25 x y)
  (* (expt 2 x) (expt 3 y)))
(define (car-25 z)
  (define (impl n a)
    (if (divides? n 2) (impl (/ n 2) (+ a 1)) a))
  (impl z 0))
(define (cdr-25 z)
  (define (impl n b)
    (if (divides? n 3) (impl (/ n 3) (+ b 1)) b))
  (impl z 0))

;;; 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus num1 num2)
  (lambda (f) (lambda (x) ((num1 f) ((num2 f) x)))))

;;; 2.7
(define (make-interval a b)
  (cons a b))
(define (lower-bound z)
  (car z))
(define (upper-bound z)
  (cdr z))

;;; 2.8
(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b)) (- (lower-bound a) (upper-bound b))))

;;; 2.10
(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (>= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "You can't divide by zero")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))))
