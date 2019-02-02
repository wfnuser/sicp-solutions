(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))
    )
)

(define (append items1 items2)
    (if (null? items1)
        items2
        (cons (car items1) (append (cdr items1) items2))
    )
)

(define (last-pair items)
    (if (= 1 (length items))
        (car items)
        (last-pair (cdr items))
    )
)

(define (fake-reverse items)
    (if (= 1 (length items))
        (car items)
        (cons (fake-reverse (cdr items)) (car items))
    )
)

(define (reverse items)
    (if (= 2 (length items))
        (cons (cdr items) (car items))
        (append (reverse (cdr items)) (car items))
    )
)

(define odds (list 1 3 5 7))
(reverse odds)
