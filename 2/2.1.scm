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
