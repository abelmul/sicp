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

;;; 1.35

;;; phi is one of the roots of the function x^2 = x + 1
;;; which can be re written as x = 1 + 1/x. Which is the transformation in the question

;; (fixed-point (lambda (x) (+ 1 (/ 1 x)))  1)

;;; 1.36

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (display next)
      (newline)
      (if (close-enough? guess next) next (try next))))
  (try first-guess))

;; without dampening (34 calls)
; (fixed-point (lambda (x) (/ (log 1000) (log x)))  2)
;; with dampening (9 calls)
; (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2)

;;; 1.37
;; this is convulted see https://eli.thegreenplace.net/2007/07/13/sicp-sections-132-133
(define (cont-frac n d k)
  (define (cont-frac-impl i)
    (if (= i k) (/ (n i) (d i)) (/ (n i) (+ (d i) (cont-frac-impl n d (+ i 1))))))
  (cont-frac-impl n d 1))

;;; 1/phi
; (cont-frac (lambda (n) 1) (lambda (d) 1) 20)

(define (cont-frac-iter n d k)
  (define (iter result i)
    (if (= i 1) (/ (n 1) result) (iter (+ (d (- i 1)) (/ (n i) result)) (- i 1))))
  (iter (d k) k))

;;; 1.38

;;; e - 2
; (cont-frac-iter (lambda (i) 1.0) (lambda (i) (cond [(= i 2) i] [(= (remainder (+ i 1) 3) 0) (* 2 (/ (+ i 1) 3))] [else 1])) 20)

;;; 1.39

(define (tan-cf x n)
  (define (tan-n i)
    (/ (if (= i 1) x (* x x)) (- (+ (* 2 i) 1) (if (= i n) 0 (tan-n (+ i 1))))))
  (tan-n 1))

;;; newtons method
(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;; 1.40
(define (cubic a b c)
  (lambda (x) (+ c (* b x) (* a x x) (* x x x))))

;;; 1.41
(define (double f)
  (lambda (x) (f (f x))))
;; (((double (double double)) inc) 5) = 21

;;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;;; 1.43
(define (repeated f n)
  (if (= n 1) (lambda (x) (f x)) (compose f (repeated f (- n 1)))))
