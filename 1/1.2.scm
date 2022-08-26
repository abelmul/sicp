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

;;; 1.13 
#|
φ = phi = (1 + sqrt(5))/2 = phi^2 - 1
ψ = psi = (1 - sqrt(5))/2 = psi^2 - 1

prove

fib(n) = (phi^n - psi^n)/sqrt(5)


base case
fib(0) = (phi^0 - psi^0)/sqrt(5) = 0
fib(1) = (phi^1 - psi^1)/sqrt(5) = 1

assume
fib(n-1) = (phi^(n-1) - psi^(n-1))/sqrt(5)
fib(n-2) = (phi^(n-2) - psi^(n-2))/sqrt(5)

induction

fib(n) = fib(n-1) + fib(n-2)
       = (phi^(n-1) - psi^(n-1))/sqrt(5) - (phi^(n-2) - psi^(n-2))/sqrt(5)
       = (phi^(n-1) + phi^(n-2) - psi^(n-1) - psi^(n-2) )/sqrt(5)
       = (phi^(n-2)(phi+1) - psi^(n-1)(psi+1))/sqrt(5)
       = (phi^(n-2)*phi^2 - psi^(n-1)*psi^2)/sqrt(5)
       = (phi^n - psi^n)/sqrt(5)
|#
