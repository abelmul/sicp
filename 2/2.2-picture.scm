#lang sicp

(#%require sicp-pict)

(define (flipped-pairs painter)
  (let ([painter2 (beside painter (flip-vert painter))]) (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (right-split painter (- n 1))]) (beside painter (below smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ([up (up-split painter (- n 1))] [right (right-split painter (- n 1))])
        (let ([top-left (beside up up)]
              [bottom-right (below right right)]
              [corner (corner-split painter (- n 1))])
          (beside (below painter top-left) (below bottom-right corner))))))
(define (square-limit painter n)
  (let ([quarter (corner-split painter n)])
    (let ([half (beside (flip-horiz quarter) quarter)]) (below (flip-vert half) half))))

;;; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (up-split painter (- n 1))]) (below painter (beside smaller smaller)))))

;;; 2.45

(define (split proc1 proc2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ([smaller ((split proc1 proc2) painter (- n 1))])
          (proc1 painter (proc2 smaller smaller))))))

;;; 2.46

(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))

(define (add-vect v1 v2)
  (cons (cons (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2)))))
(define (sub-vect v1 v2)
  (cons (cons (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2)))))
(define (scale-vect v s)
  (cons (* (xcor-vect v) s) (* (ycor-vect v) s)))

;;; 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame-cons frame)
  (origin-frame frame))
(define (edge1-frame-cons frame)
  (edge1-frame frame))
(define (edge2-frame-cons frame)
  (cddr frame))

