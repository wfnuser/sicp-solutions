(define random-init 0) 
(define (rand-update x) (+ x 1))
(define rand
    (let ((x random-init))
        (define (dispatch message)
            (cond ((eq? message 'generate) (begin (set! x (rand-update x)) x))
                ((eq? message 'reset) (lambda (num) (set! x num))))
        )
        dispatch
    )
)

(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 0)
(rand 'generate)
