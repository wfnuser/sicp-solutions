; (define (factorial n)
;     (if (= n 1)
;         1
;         (* n (factorial (- n 1)))
;     )
; )

; (define (factorial n)
;     (define (iter-fact product counter max-count)
;         (if (> counter max-count)
;             product
;             (iter-fact (* counter product) (+ counter 1) max-count)
;         )
;     )

;     (iter-fact 1 1 5)
; )

(define (factorial n)
    (define (iter-fact product counter)
        (if (<= counter 0)
            product
            (iter-fact (* counter product) (- counter 1))
        )
    )
    (iter-fact 1 5)
)

(factorial 5)