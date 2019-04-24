(define (or-gate w1 w2 output)
    (define (or-action-procedure)
        (let ((new-value (logical-or (get-signal w1) (get-signal w2))))
            (after-delay
                or-gate-delay
                (lambda () (set-signal! output new-value))
            )
        )
    )
    (add-action! w1 or-action-procedure)
    (add-action! w2 or-action-procedure)
)