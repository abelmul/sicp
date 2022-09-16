#lang sicp

(define (even? n)
  (= (remainder n 2) 0))
(define (sum term a next b)
  (if (> a b) 0 (+ (term a) (sum term (next a) next b))))
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;;; 1.29

;; h/3 [(y0+yn) + 4(y1+y3+y5+...+y(n-1)) + 2(y2+y4+y6+...+yn)]

(define (simpson-three-fourth f a b n)
  (define (compute-h)
    (/ (- b a) n))
  (define (compute k)
    (cond
      [(or (= k 0) (= k n)) (* (/ (compute-h) 3) (f a n (compute-h)))]
      [else (+ (* (/ (compute-h) 3) (if (even? n) 2 4) (f a n (compute-h))) (compute (- n 1)))]))
  (compute n))

;;; 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (+ result (term a)))))
  (iter a 0))

;;; 1.31
(define (product term a next b)
  (if (> a b) 1 (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial-product n)
  (define (identity n)
    n)
  (define (next n)
    (+ n 1))

  (product-iter identity 1 next n))

(define (wallis-pi n)
  (define (term k)
    (if (even? k) (/ (+ k 2) (+ k 1)) (/ (+ k 1) (+ k 2))))
  (define (next n)
    (+ n 1))
  (* 4.0 (product-iter term 1 next n)))
