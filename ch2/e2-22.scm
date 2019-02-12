(define (square-list-fake items)
    (define (iter things answer) 
        (if (null? things)
            answer
            (iter (cdr things)
                (cons answer (square (car things))))))
    (iter items '()))

(define (square-list items)
    (define (iter things answer) 
        (if (null? things)
            (reverse answer)
            (iter (cdr things)
                (cons (square (car things)) answer))))
    (iter items '()))

(define (reverse items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things) (cons (car things) answer))
        )
    )
    (iter items '())
)

(square-list (list 1 2 3 4))