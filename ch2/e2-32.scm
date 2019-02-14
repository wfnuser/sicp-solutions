(define (subsets s)
    (if (null? s)
        (list '())
        (let ((rest (subsets (cdr s))))
            (append rest (map (
                lambda (rest)
                (cons (car s) rest)
            ) rest)))))


(subsets (list 1 2 3))