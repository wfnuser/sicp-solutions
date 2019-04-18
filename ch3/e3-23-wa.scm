(define (make-deque)
    (cons '() '())
)
(define (front-ptr que)
    (car que)
)
(define (rear-ptr que)
    (cdr que)
)
(define (set-front-ptr! queue item)
    (set-car! queue item))
(define (set-rear-ptr! queue item)
    (set-cdr! queue item))
(define (empty-que? que)
    (null? (front-ptr que))
)

(define (make-deque-node item prev next)
    (cons (cons prev next) item)
)

(define (insert-front queue item)
    (if (empty-que? queue)
        (let ((node (make-deque-node item '() '())))
            (set-front-ptr! queue node)
            (set-rear-ptr! queue node)
            front-ptr queue
        )
        (let ((node (make-deque-node item '() '())))
            (set-cdr! (car node) (front-ptr queue))
            (set-car! (car (front-ptr queue)) node)
            (set-front-ptr! queue node)
        )
    )
)

(define (print-que q)
    (define (iter res q)
        (if (null? q)
            res
            (iter (cons (cdr q) res) (cadr q))
        )
    )
    (iter '() (front-ptr q))
)

(define q1 (make-deque))
(print-que q1)

(insert-front q1 'a)
(insert-front q1 'b)
(print-que q1)
