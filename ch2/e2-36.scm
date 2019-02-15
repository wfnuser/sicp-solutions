(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (
                map (lambda (origin-seq) 
                    (car origin-seq)
                ) seqs
            ))
            (accumulate-n op init (
                map (lambda (origin-seq) 
                    (cdr origin-seq)
                ) seqs
            )))))


(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(accumulate-n + 0 s)