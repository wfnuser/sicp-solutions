(define (an-integer-between start end)
    (require (< start end))
    (amb start (an-integer-between (+ start 1) end))
)