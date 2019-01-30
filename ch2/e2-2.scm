(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

(define (make-point x y)
    (cons x y))

; ; s and e should be point
(define (make-segment s e)
    (cons s e))

(define (start-segment seg)
    (car seg))

(define (end-segment seg)
    (cdr seg))

(define (average a b)
    (/ (+ a b) 2))

(define (average-x p-a p-b)
    (average (x-point p-a) (x-point p-b))
)

(define (average-y p-a p-b)
    (average (y-point p-a) (y-point p-b))
)

(define (midpoint-segment seg)
    (let ((s (start-segment seg))
            (e (end-segment seg)))
                (make-point (average-x s e) (average-y s e))))
    
(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

(let* ((p-s (make-point 4 18))
        (p-e (make-point 5 19))
        (seg (make-segment p-s p-e))
        (mid (midpoint-segment seg)))
                (print-point mid))