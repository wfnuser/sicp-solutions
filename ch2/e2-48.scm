(define (make-vector x y)
    (cons x y)
)

(define (xcor-vect v)
    (car v)
)

(define (ycor-vect v)
    (cdr v)
)

(define (add-vect v1 v2)
    (make-vector (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2)))
)

(define (sub-vect v1 v2)
    (make-vector (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2)))
)

(define (scale-vect s v)
    (make-vector (* (xcor-vect v) s) (* (ycor-vect v) s))
)

(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define (origin-frame f)
    (car f)
)
(define (edge1-frame f)
    (cadr f)
)
(define (edge2-frame f)
    (caddr f)
)

(let ((origin (make-vector 1 2)) (edge1 (make-vector 3 4)) (edge2 (make-vector 4 5)))
    (let ((frame (make-frame origin edge1 edge2)))
        (newline)
        (display (origin-frame frame))
        (newline)
        (display (edge1-frame frame))
        (newline)
        (display (edge2-frame frame))
        (newline)
    )
)

(define (make-segment start end)
    (list start end))

(define (start-segment s)
    (car s))

(define (end-segment s)
    (cadr s))

(define (segments->painter segment-list) 
    (lambda (frame)
        (for-each
            (lambda (segment)
                (draw-line
                    ((frame-coord-map frame)
                    (start-segment segment))
                    ((frame-coord-map frame)
                    (end-segment segment))))
            segment-list)))

