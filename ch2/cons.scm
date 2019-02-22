(define (cons a b)
    (lambda (type)
        (if (= 0 type)
            a
            b
        )
    )
)

(define (car pair)
    (pair 0)
)

(define (cdr pair)
    (pair 1)
)

(let ((pair (cons 1 2)))
    (display (car pair))
    (display (cdr pair))
)