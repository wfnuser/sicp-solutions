
(define (enumerate-interval start end)
    (if (<= start end)
        (cons start (enumerate-interval (+ start 1) end))
        '()
    )
)

(define (accumulate op initial seq)
    (if (null? seq)
        initial
        (op (car seq) (accumulate op initial (cdr seq)))
    )
)

(define (flat-map proc seq)
    (accumulate append '() (map proc seq))
)

(define (queens board-size) 
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter (lambda (positions) (safe? k positions))
                    (flat-map 
                        (lambda (rest-of-queens) 
                            (
                                ; (display rest-of-queens)
                                map (lambda (new-row)
                                        (adjoin-position new-row k rest-of-queens)
                                    )
                                (enumerate-interval 1 board-size)
                            )
                        )
                        (queen-cols (- k 1))
                    )
            )
        )
    )
  (queen-cols board-size)
)

(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))

(define (safe? k positions)
    (check (car positions) (cdr positions) 1)
)
(define (check new-row positions i)
    (if (null? positions)
        #t
        (let ((position (car positions)))
            (if (or (= position new-row)
                    (= (+ position i) new-row)
                    (= (- position i) new-row)
                )
                #f
                (check new-row (cdr positions) (+ i 1)))
        )
    )
)


(queens 6)
(safe? 2 (list 1 2 3))