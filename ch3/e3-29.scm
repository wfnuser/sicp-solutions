(define (or-gate w1 w2 output)
    (let ((invert-1 (make-wire)) (invert-2 (make-wire) (and-inverts (make-wire))))
        (inverter w1 invert-1)
        (inverter w2 invert-2)
        (and-get invert-1 invert-2 and-inverts)
        (inverter and-inverts output)
        'ok
    )
)