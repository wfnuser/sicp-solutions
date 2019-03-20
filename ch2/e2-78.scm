(define (attach-tag type-tag contents)
    (if (number? contents)
        contents
        (cons type-tag contents)))

(define (type-tag  x)
    (if (pair? x)
        (car x)
        'scheme-number
    )
)

(define (contents  x)
    (if (pair? x)
        (car x)
        x
    )
)