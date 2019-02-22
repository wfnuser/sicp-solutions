; (define (fib n)
;     (cond ((= 0 n) 0)
;         ((= 1 n) 1)
;         (else (+ (fib (- n 1)) (fib (- n 2))))    
;     )
; )

(define (fib n)
    (define (iter-fib a b count)
        (if (= count 0)
            b
            (iter-fib (+ a b) a (- count 1)))
    )

    (iter-fib 1 0 5)
)