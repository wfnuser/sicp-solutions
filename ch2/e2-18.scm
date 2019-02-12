(define (reverse items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things) (cons (car things) answer))
        )
    )
    (iter items '())
)

(define odds (list 1 3 5 7))
(reverse odds)
