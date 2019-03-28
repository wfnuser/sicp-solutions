(define (make-accumulator sum)
    (lambda (step) (begin (set! sum (+ sum step)) sum))
)

(define A (make-accumulator 5))

(A 10)
(A 10)