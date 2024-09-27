#lang sicp

; (#%require "../1/1.1.scm")
; (#%require "../1/1.2.scm")
; (#%require "./2.1.scm")
; (#%require "./2.2.scm")
(#%require "./2.3.scm")

;; common
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum) (car datum) (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum) (cdr datum) (error "Bad tagged datum: CONTENTS" datum)))

(define (put _ __ ___)
  (error "not implemented"))
(define (get _ __)
  (error "not implemented"))

;;; 2.73
#|
Operator | Operation
+        | add
*        | multiply
**       | exponentiation
|#

(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp) (if (same-variable? exp var) 1 0)]
    [else ((get 'deriv (operator exp)) (operands exp) var)]))

(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))

;; a. We can't assimilate the number? and variable? into the dispatch because
;;    we aren't dispatching anything in this case

;; b, c
(define (deriv-addition expr var)
  (let ([u (car expr)]
        [v (cdr expr)])
    (make-sum (deriv u var) (deriv v var))))

(define (deriv-product expr var)
  (let ([u (car expr)]
        [v (cdr expr)])
    (make-sum (make-product u (deriv v var)) (make-product (deriv u var) v))))

(define (deriv-exponentiation expr var)
  (let ([u (car expr)]
        [v (cdr expr)])
    (make-product (make-product v (make-exponentiation u (- v 1))) (deriv u var))))

#|
(put 'deriv + deriv-addition)
(put 'deriv * deriv-product)
(put 'deriv '** deriv-exponentiation)
|#

;; d. We have to change the position of the put function as well
;;    instade of having op, type, item as argument we have to swap op and type
;;    which becomes type, op, item

;;; 2.74

;; a. we can define each division file as tagged files that contain the division of the file

(define (make-employee-file division file)
  (cons division file))
(define (division-of-employee-file employee-file)
  (car employee-file))
(define (file-of-employee-file employee-file)
  (cdr employee-file))

(define (get-record employee employee-file)
  ((get 'get-record (division-of-employee-file employee-file)) employee
                                                               (file-of-employee-file employee-file)))

;; b. we can use the same structure for get-salary
(define (get-salary employee employee-file)
  ((get 'get-salary (division-of-employee-file employee-file)) employee
                                                               (file-of-employee-file employee-file)))

;; c
(define (find-employee-record employee employee-files)
  (define (find f l)
    (if (null? l) nil (let ([r (f (car l))]) (if (equal? r #f) (find f (cdr l)) r))))
  (find (lambda (employee-file)
          (let ([record (get-record employee employee-file)]) (if (null? record) #f record)))
        employee-files))

;; d. it should define the 'get-record and 'get-salary methods and put them in the dispatch table

;;; 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond
      [(equal? op 'real-part) (* r (cos a))]
      [(equal? op 'imag-part) (* r (sin a))]
      [(equal? op 'magnitude) r]
      [(equal? op 'angle) a]
      [else (error "Unknown op: MAKE-FROM-MAG-ANG" op)]))
  dispatch)

;;; 2.76
;; Because we have to change the top level functions using explicit dispatch it is not good
;; for both cases. As for the data-directed style and message-passing style we don't have to
;; change the top funtion definitions so they are good for the task.

;; exports
(#%provide attach-tag)
(#%provide type-tag)
(#%provide contents)
(#%provide get)
(#%provide put)
