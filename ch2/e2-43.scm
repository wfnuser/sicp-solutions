(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1))))
            (below painter (beside up up))
        )
    )
)