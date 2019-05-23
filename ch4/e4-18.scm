(define (solve f y0 dt)
    (let ((y '*unassigned*)
        (dy '*unassigned*))
        (let ((a (integral (delay dy) y0 dt))
            (b (stream-map f y)))
            (set! y a)
            (set! dy b))
    y))

; when we evaluate (stream-map f y)
; y is still unassigned, so it won't work