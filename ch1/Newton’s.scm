(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
        (else else-clause))
)

(define (sqrt num)
    
    (define (good-enough? guess num)
        (< (abs (- (square guess) num)) 0.001)
    )

    (define (average x y) 
        (/ (+ x y) 2)
    )

    (define (improve guess num)
        (average guess (/ num guess))
    )

    (define (sqrt-iter guess num)
        (if (good-enough? guess num)
            guess
            (sqrt-iter (improve guess num) num)
        )
    )

    (sqrt-iter 1.0 num)
)

(sqrt 4)