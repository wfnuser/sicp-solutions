(define (run-forever) (run-forever))
(define (try p)
    (if (halts? p p)
        (run-forever)
        'halted))

; try try
; => if (halts? try try)
; if (halts? try try) returns #t
; try try will go in run-forever, which is in a paradox
; otherwise, try try should not halt, but (halts? try try) returns #f