#lang sicp

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

;;; 1.19
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

;;; 1.20
(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

;;; divisors
;;; 1.23
(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (define (next-divisor n)
      (if (= n 2) 3 (+ n 2)))
    (cond
      [(> (square test-divisor) n) n]
      [(divides? test-divisor n) test-divisor]
      [else (find-divisor n (next-divisor test-divisor))]))

  (find-divisor n 2))

;;; primes

(define (expmod base exp m)
  (cond
    [(= exp 0) 1]
    [(even? exp) (remainder (square (expmod base (/ exp 2) m)) m)]
    [else (remainder (* base (expmod base (- exp 1) m)) m)]))

(define (fermat-test n)
  (define (try-it a n)
    (= (expmod a n n) a))
  (try-it (random (- n 1)) n))
(define (fast-prime? n times)
  (cond
    [(= times 0) true]
    [fermat-test n]
    [fast-prime?
     n
     (- times 1)]
    [else false]))

;;; 1.22
(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  (define (start-prime-test n start-time)
    (if (prime? n) (report-prime (- (runtime) start-time))))
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes n d)
  (cond
    [(> n (+ d 1))
     (newline)
     (display n)]
    [(even? n) (search-for-primes (+ n 1) d)]
    [else
     (timed-prime-test n)
     (search-for-primes (+ n 1) d)]))

;;; 1.25

;;; if we did (fast-expt base exp) we will compute the value unnessesarily.
;;; It might also cause overflow on some platforms and languages if the computed number is very large.
;;; But we can use the fact that remainder(a x b, n) = remainder(a, n) x remainder(b, n) to compute less.

;;; 1.26

;;; if we do (* (expmod base (/ exp 2)) (expmod base (/ exp 2))) we compute the (expmod base (/ exp 2)) twice.
;;; Since expmod is recursive this computation will call another expmod ... etc.
;;; Which will make expmod tree recursive. But if we call (squre (expmod base (/ exp 2))) we compute it once.

;;; 1.27

(define (test-carmichael n)
  (define (test-fermat a)
    (if (= a n) #t (if (= (expmod a n n) a) (test-fermat n (+ a 1)) #f)))
  (test-fermat 2))

;;; 1.28
(define (fast-prime-miller-rabin n times)
  (define (remainder-if-not-zero number m)
    (if (= number 0) 0 (remainder number m)))

  (define (expmod base exp m)
    (cond
      [(= exp 0) 1]
      [(= (remainder (* base base) m) 0) 0]
      [(even? exp) (remainder (square (expmod base (/ exp 2) m)) m)]
      [else (remainder (* base (expmod base (- exp 1) m)) m)]))

  (define (try-it a n)
    (= (expmod a n n) a))

  (define (miller-rabit-test n)
    (try-it (random (- n 1)) n))

  (cond
    [(= times 0) true]
    [miller-rabit-test n]
    [fast-prime?
     n
     (- times 1)]
    [else false]))
