#lang sicp

(define (even? n)
  (= (remainder n 2) 0))
(define (sum term a next b)
  (if (> a b) 0 (+ (term a) (sum term (next a) next b))))
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (identity n)
  n)
(define (add-one n)
  (+ n 1))

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

  (product-iter identity 1 add-one n))

(define (wallis-pi n)
  (define (term k)
    (if (even? k) (/ (+ k 2) (+ k 1)) (/ (+ k 1) (+ k 2))))
  (* 4.0 (product-iter term 1 add-one n)))

;;; 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;;; 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter))
          (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (filtered-accumulate-iter combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b) result (iter (next a) (if (filter a) (combiner (term a) result) result))))
  (iter a null-value))

; (define (sum-square-prime a b)
;   (define (combiner a b)
;     (+ a b))
;   (filtered-accumulate-iter combiner 0 identity a add-one b prime?))

;;; 1.34
;;; if f is defined as (define (f g) (g 2))
;;; (f f) -> (f 2) -> (2 2)
;;; since 2 is not a function this is an error.
