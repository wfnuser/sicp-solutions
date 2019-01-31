(define (make-interval a b) (cons a b))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
        (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
        (max p1 p2 p3 p4))))

(define (div-interval x y) (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y)))))

(define (upper-bound z)
    (max (car z)
        (cdr z)))

(define (lower-bound z)
    (min (car z)
        (cdr z)))



(define (sub-interval x y)
    (add-interval x
        (make-interval (- (lower-bound y))
            (- (upper-bound y)))))