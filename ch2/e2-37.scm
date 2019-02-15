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


(define v (list 1 2 3))
(define w (list 4 5 6))
(define m (list (list 1 2 3) (list 4 5 6)))
(define n (list (list 1 2) (list 4 5) (list 3 6)))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (each-line-of-matrix) 
            (dot-product each-line-of-matrix v)
        ) m)
)

(define (matrix-*-matrix m n)
    (map (lambda (each-line-of-matrix) 
        (matrix-*-vector (transform n) each-line-of-matrix)
    ) m)
)

(define (transform m)
    (accumulate-n cons '() m)
)

(dot-product v w)
(matrix-*-vector m v)
(transform m)
(matrix-*-matrix m n)