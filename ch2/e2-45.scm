(define up-split (split below beside))

(define (split big-operate small-operate)
    (lambda (painter n) 
        (if (= n 0)
            painter
            (let ((smaller ((split big-combiner small-combiner) painter (- n 1))))
                (big-operate painter (small-operate smaller smaller))
            )
        )
    )
)