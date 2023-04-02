#lang sicp

; (#%require "../1/1.1.scm")
; (#%require "../1/1.2.scm")
; (#%require "./2.1.scm")
; (#%require "./2.2.scm")

;;;
(define (memq item x)
  (cond
    [(null? item) false]
    [(eq? item (car x)) true]
    [else (memq item (cdr x))]))

;;; 2.54
(define (equal? list1 list2)
  (cond
    [(and (null? list1) (null? list2)) true]
    [(or (null? list1)
         (null? list2)
         (not (symbol? (car list1)))
         (not (symbol? (car list2)))
         (not (eq? (car list1) (car list2))))
     false]
    [else (equal? (cdr list1) (cdr list2))]))

;;; 2.56
;;; ''abracadabra can be rewritten as (quote (quote abracadabra))
;;; which when we apply car returns 'quote


