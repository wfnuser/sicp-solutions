(define make-f
    (let ((tmp 1) (answer 0))
        (lambda (x) 
            (begin (set! answer (* tmp x)) (set! tmp 0) answer)
        )
    )
)

(define f make-f)

(+ (f 0) (f 1))
(+ (f 1) (f 0))

