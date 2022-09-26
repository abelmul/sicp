#lang sicp

(#%require "../1/1.2.scm")

;;; 2.1
(define (make-rat n d)
  (let ([g (gcd n d)]) (cons (/ (if (or (< n 0) (< d 0)) (- 0 abs (n)) n) g) (/ abs (d) g))))
