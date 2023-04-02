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

;;;
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                        (scale-vect (ycor-vect v) (edge2-frame frame))))))

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

;;;

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
                (draw-line ((frame-coord-map frame) (start-segment segment))
                           ((frame-coord-map frame) (end-segment segment))))
              segment-list)))

;;; 2.48

(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-segment segment)
  (cons (cons (make-vect 0 0) (start-segment segment)) (cons (make-vect 0 0) (end-segment segment))))

;;; 2.49

(define (outline-segments)
  (list (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
        (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))
        (make-segment (make-vect 1.0 1.0) (make-vect 1.0 0.0))
        (make-segment (make-vect 1.0 0.0) (make-vect 0.0 0.0))))
(define (x-segments)
  (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
        (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0))))
(define (diamond-segments)
  (list (make-segment (make-vect 0.5 0.0) (make-vect 0.0 0.5))
        (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
        (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))
        (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))))

(define (outline-painter)
  (segments->painter outline-segments))
(define (x-painter)
  (segments->painter x-segments))
(define (diamond-painter)
  (segments->painter diamond-segments))

;;;

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ([m (frame-coord-map frame)])
      (let ([new-origin (m origin)])
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))

;;; 2.50

(define (rotate90 painter)
  (transform-painter painter (make-vect 0.0 1.0) (make-vect 0.0 0.0) (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter (make-vect 1.0 1.0) (make-vect 0.0 1.0) (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter (make-vect 1.0 0.0) (make-vect 1.0 1.0) (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter painter (make-vect 1.0 0.0) (make-vect 0.0 0.0) (make-vect 1.0 1.0)))

;;; 2.51

(define (below painter1 painter2)
  (let ([split-point [make-vect 0.0 0.5]])
    (let ([paint-bottom
           [transform-painter painter1 (make-vect 0.0 0.0) split-point (make-vect 1.0 0.0)]]
          [paint-top (transform-painter painter2 split-point (make-vect 0.0 1.0) (make-vect 1 0.5))])
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below-with-beside painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
