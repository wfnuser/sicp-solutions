(define (contain-cycle pairs)
    (define (inner x memo-list)
        (if (memq x memo-list)
            #t          
            (if (pair? x)
                (or (inner (car x) (cons x memo-list)) (inner (cdr x) (cons x memo-list)))
                #f
            )
        )
    )
    (inner pairs '())
)

(contain-cycle (list 1 2 3))

(define cycle (cons 1 (cons 2 (cons 3 '()))))
(set-cdr! (last-pair cycle) cycle)
(contain-cycle cycle)