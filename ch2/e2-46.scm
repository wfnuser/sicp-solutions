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


(let ((v1 (make-vector 1 2)) (v2 (make-vector 3 4)))
    (newline)
    (display (add-vect v1 v2))
    (display (sub-vect v1 v2))
    (display (scale-vect 3 v2))
)
