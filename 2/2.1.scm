#lang sicp

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

;;; 2.1
(define (make-rat n d)
  (let ([g (gcd n d)]) (cons (/ (if (or (< n 0) (< d 0)) (- 0 abs (n)) n) g) (/ abs (d) g))))
