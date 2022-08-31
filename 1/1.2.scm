#lang scheme

(define (factorial-rec n)
  (if (= n 0) 1 (* n (factorial-rec (- n 1)))))

(define (factorial n)
  (define (factorial-impl a count)
    (if (= count 0) a (factorial-impl (* a count) (- count 1))))
  (factorial-impl 1 n))

(define (fibonaci-rec n)
  (if (= n 1) 1 (+ n (factorial-rec (- n 1)))))
(define (fibonaci n)
  (define (fibonaci-impl a b count)
    (if (= count 0) b (fibonaci-impl b (+ a b) (- count 1))))
  (fibonaci-impl 0 1 n))

;;; Counting Change
(define (count-change-rec amount)
  (define (first-denomination kinds-of-coins)
    (cond
      [(= kinds-of-coins 1) 1]
      [(= kinds-of-coins 2) 5]
      [(= kinds-of-coins 3) 10]
      [(= kinds-of-coins 4) 25]
      [(= kinds-of-coins 5) 50]))
  (define (cc amount kinds)
    (cond
      [(= amount 0) 1]
      [(< amount 0) 0]
      [(= kinds 0) 0]
      [else (+ (cc amount (- kinds 1)) (cc (- amount (first-denomination kinds)) kinds))]))
  (cc amount 5))

;;; 1.11

(define (fun-111-rec n)
  (if (< n 3) n (+ (fun-111-rec (- n 1)) (* 2 (fun-111-rec (- n 2))) (* 3 (fun-111-rec (- n 3))))))

(define (fun-111-iter n)
  (define (fun-111-iter-impl a b c count)
    (if (< count 3) c (fun-111-iter-impl b c (+ c (* 2 b) (* 3 c)) (- count 1))))
  (fun-111-iter-impl 0 1 2 n))

;;; 1.12

(define (pascal-tri-rec row col)
  (if (or (= row 1) (= col 1) (= col row))
      1
      (+ (pascal-tri-rec (- row 1) (- col 1)) (pascal-tri-rec row (- col 1)))))

;;; 1.14
;; https://eli.thegreenplace.net/2007/06/28/sicp-section-123

;;; 1.15
(define (sine angle)
  (define (cube x)
    (* x x x))
  (define (p x)
    (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1)) angle (p (sine (/ angle 3.0)))))

;; for (sine 12.5) p is called 5 times 12.5/3^5 = 0.05
;; https://eli.thegreenplace.net/2007/06/28/sicp-section-123

;;; 1.16
(define (square x)
  (* x x))
(define (even? x)
  (= (remainder x 2) 0))

(define (fast-expt-rec b n)
  (cond
    [(= n 0) 1]
    [(even? n) (square (fast-expt-rec b (/ n 2)))]
    [else (* b (fast-expt-rec b (- n 1)))]))

(define (fast-expt b n)
  (define (fast-expt-iter b mul n)
    (if (= n 0) mul (fast-expt-iter (* b b) (if (even? n) mul (* mul b)) (quotient n 2))))
  (fast-expt-iter b 1 n))

;;; 1.17
(define (double a)
  (+ a a))
(define (halve a)
  (quotient a 2))

(define (fast-mul-rec a b)
  (cond
    [(= b 0) 0]
    [(even? b) (fast-mul-rec (double a) (halve b))]
    [else (+ a (fast-mul-rec (double a) (halve (- b 1))))]))

;; 1.18

(define (fast-mul a b)
  (define (fast-mul-iter a b mul)
    (if (= b 0) mul (fast-mul-iter (double a) (halve b) (if (even? b) mul (+ mul a)))))
  (fast-mul-iter a b 0))

;;; 1.18
(define (fib n)
  (define (fib-iter a b p q count)
    (cond
      [(= count 0) b]
      [(even? count)
       (fib-iter a
                 b
                 (+ (* p p) (* q q)) ; compute p'
                 (+ (* q q) (* 2 (* p q))) ; compute q'
                 (/ count 2))]
      [else
       (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b) p) (* a q))
       p
       q
       (- count 1)]))
  (fib-iter 1 0 0 1 n))
