(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))
    )
)

(define (last-pair items)
    (if (= 1 (length items))
        (car items)
        (last-pair (cdr items))
    )
)

(define odds (list 1 3 5 7 9 10))
(last-pair odds)