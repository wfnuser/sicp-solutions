(define (make-queue)
    (let ((front-ptr '()) (rear-ptr '()))
        (define (insert-queue item)
            (let ((new-item (cons item '())))
                (if (null? front-ptr)
                    (begin
                        (display item)
                        (set! front-ptr new-item)
                        (set! rear-ptr new-item)
                    )
                    (begin
                        (display new-item)
                        (set-cdr! rear-ptr new-item)
                        (set! rear-ptr new-item)
                    )
                )
            )
            front-ptr
        )
        (define (dispatch m)
            (cond ((eq? m 'delete)
                    (cond ((null? front-ptr) (error "DELETE! called with an empty queue"))
                        (else (set! front-ptr (cdr front-ptr))))
                    front-ptr
                )
                ((eq? m 'insert)
                    insert-queue
                )
            )
        )
        dispatch
    )
)

(define q (make-queue))
((q 'insert) 'a)
((q 'insert) 'b)
(q 'delete)
