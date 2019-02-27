(define (equal? a b)
    (if (pair? a)
        (if (pair? b)
            (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
            #f
        )
        (eq? a b)
    )
)

(equal? '(1 2 3) '(1 2 3))