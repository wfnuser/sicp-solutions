
(define (enumerate-interval start end)
    (if (<= start end)
        (cons end (enumerate-interval start (- end 1)))
        '()
    )
)

(define (accumulate op initial seq)
    (if (null? seq)
        initial
        (op (car seq) (accumulate op initial (cdr seq)))
    )
)

(define (flat-map proc seq)
    (accumulate append '() (map proc seq))
)

(define (min a b)
    (if (< a b)
        a
        b
    )
)

(define (all-triples n)
    (flat-map (lambda (i)
        (if (<= i 2)
            '()
            (flat-map (lambda (j)
                (if (<= j 1)
                    '()
                    (map (lambda (k) (list k j i)) (enumerate-interval 1 (min (- j 1) (- (- n j) i) )))
                )
            )  (enumerate-interval 1 (min (- i 1) (- n i))))
        )
    ) (enumerate-interval 1 n))
)

(all-triples 10)