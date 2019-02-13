(define x (list (list 1 2) (list 3 4 (list 5 6) )))

(define (fringe tree)
    (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))
    )
)

(fringe (list x x))