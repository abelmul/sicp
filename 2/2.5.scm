#lang sicp

(#%require "./2.4.scm")

;;2.77
#|
a. assuming z is a complex number(because magnitude isn't defined for other numbers)
(magnitude z) = (apply-generic 'magnitude z) = ((get 'magnitude (type-tag z)) z) = ((get 'magnitude 'complex) z)

but since there is no magnitude operation registered for complex it fails. Which makes

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

necessary
|#

;; 2.78

(define (attach-tag type-tag contents)
  (if (number? contents) contents (cons type-tag contents)))
(define (type-tag datum)
  (if (number? datum)
      'scheme-number
      (if (pair? datum) (car datum) (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (if (number? datum)
      datum
      (if (pair? datum) (cdr datum) (error "Bad tagged datum: CONTENTS" datum))))

;; 2.79
(put 'equ? '(scheme-number scheme-number) (lambda (x y) (eq? x y)))
(put 'equ?
     '(rational rational)
     (lambda (x y) (and (eq? (numer x) (numer y)) (eq? (denom x) (denom y)))))
(put 'equ?
     '(complex complex)
     (lambda (x y) (and (eq? (magnitude x) (magnitude y)) (eq? (angle x) (angle y)))))

;; 2.80
(define (=zero? number)
  (apply-generic '=zero? number?))
(put '=zero? 'scheme-number (lambda (x) (zero? x)))
(put '=zero? 'rational (lambda (x) (zero? (numer x))))
(put '=zero? 'complex (lambda (x) (and (zero? (magnitude x)) (zero? (angle x)))))

;;; 2.81

#|
a. it will be a recursive function that never stops.
(apply-generic op . args) = (apply-generic op . (list ((get-coercion T T) (car args)) (cdr args))) = (apply-generic op . args)

b. it works correctly but we can check if it is the same type and disable coercison
   not to be infinitly recursive.
|#

;; c

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ([type1 (car type-tags)]
                    [type2 (cadr type-tags)]
                    [a1 (car args)]
                    [a2 (cadr args)])
                (if (not (equal? type1 type2))

                    (let ([t1->t2 (get-coercion type1 type2)]
                          [t2->t1 (get-coercion type2 type1)])
                      (cond
                        [t1->t2 (apply-generic op (t1->t2 a1) a2)]
                        [t2->t1 (apply-generic op a1 (t2->t1 a2))]
                        [else (error "No method for these types" (list op type-tags))]))
                    (error "No method for these types" (list op type-tags)))
                (error "No method for these types" (list op type-tags))))))))

;;; 2.82

(define (apply-generic op . args)
  (define (coerce-all type args)
    (if (null? args)
        nil
        (let ([arg-type (type-tag (car args))]
              [arg (cons args)]
              [next-coercion (recursive-coercion type (cdr args))])
          (cond
            [(equal? #f next-coercion) #f]
            [(equal? arg-type type) (cons arg next-coercion)]
            [else
             (let ([t2->t2 (get-coercion (get-coercion type arg-type))])
               (if (t1->t2) (cons (t1->t2 arg) next-coercion) #f))]))))

  (define (recursive-coercion types args)
    (if (null? types)
        #f
        (let ([coerced (coerce-all (car types) args)])
          (if (equal? #f coerced) (recursive-coercion (cdr types) args) coerced))))

  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          ;; try to coercion recursively
          (let ([coerced-args (recursive-coercion type-tags args)])
            (if coerced-args
                (apply-generic op . coerced-args)
                (error "No method for these types" (list op type-tags))))))))

;;; 2.83

(put 'raise 'integer (lambda (x) (make-rational x 1)))
(put 'raise 'rational (lambda (x) (make-real (/ (numer x) (denom x)))))
(put 'raise 'real (lambda (x) (make-from-real-imag x 0)))

(define (raise x)
  (apply-generic 'raise x))

;;; 2.84
(put 'inherits 'integer (lambda () #f))
(put 'inherits 'rational (lambda () (cons 'integer ((get 'inherits 'integer)))))
(put 'inherits 'real (lambda () (cons 'rational ((get 'inherits 'rational)))))
(put 'inherits 'complex (lambda () (cons 'real ((get 'inherits 'real)))))

(define (inherits type1 type2)
  (define (search needle l)
    (if (and (not (null? l)) (equal? (car l) needle)) #t (search needle (cdr l))))
  (search type2 ((get 'inherits type1))))

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ([type1 (car type-tags)]
                    [type2 (cadr type-tags)]
                    [a1 (car args)]
                    [a2 (cadr args)])
                (cond
                  [(inherits type1 type2) (apply-generic op (cons a1 (raise a2)))]
                  [(inherits type2 type1) (apply-generic op (cons (raise a1) a2))]
                  [else (error "No method for these types" (list op type-tags))])))))))

;;; 2.85

(put 'project 'rational (lambda (x) (round (/ (numer x) (denom x)))))
(put 'project
     'real
     (lambda (x)
       (let ([number (rationalize (inexact->exact 1.2) 1/100)])
         (make-rational (numerator number) (denominator number)))))
(put 'project 'complex (lambda (x) (make-real (imag-part x))))

(define (drop x)
  (let ([project-proc (get 'project (type-tag x))])
    (if project-proc
        (let ([projected (project x)]) (if (equ? (raise projected) projected) (drop projected) x))
        x)))

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ([type1 (car type-tags)]
                    [type2 (cadr type-tags)]
                    [a1 (car args)]
                    [a2 (cadr args)])
                (cond
                  [(inherits type1 type2) (apply-generic op (cons a1 (raise a2)))]
                  [(inherits type2 type1) (apply-generic op (cons (raise a1) a2))]
                  [else (error "No method for these types" (list op type-tags))])))))))

;;; 2.86

;; we have to define  + interms of add
;; other than that we can already use the abstraction
;; untoched by only implementing sin, cos, and atan for the different
;; numers and use that
(put 'cos-n 'integer (lambda (x) (tag (cos x))))
(put 'cos-n 'rational (lambda (x) (tag (cos (/ (numer x) (denom x))) )))
(put 'cos-n 'real (lambda (x) (tag (cos x) )))

(put 'sin-n 'integer (lambda (x) (tag (sin x) )))
(put 'sin-n 'rational (lambda (x) (tag (sin (/ (numer x) (denom x))) )))
(put 'sin-n 'real (lambda (x) (tag (sin x) )))

(put 'atan-n 'integer (lambda (x) (tag (atan x) )))
(put 'atan-n 'rational (lambda (x) (tag (atan (/ (numer x) (denom x))) )))
(put 'atan-n 'real (lambda (x) (tag (atan x) )))

(define (cos-n x) (apply-generic 'cos-n x))
(define (sin-n x) (apply-generic 'cos-n x))
(define (atan-n x) (apply-generic 'cos-n x))

#|
now we just have to define the different complex implementations
using add and one of this functions
for eg. the imag-part for the polar package becomes
(define (imag-part z) (mul (magnitude z) (sin-n (angle z))))
|#
