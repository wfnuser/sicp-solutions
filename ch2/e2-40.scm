
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


(define (unique-pairs n)
    (flat-map (lambda (i) 
       (map (lambda (j) (list j i))  (enumerate-interval 1 i))
    ) (enumerate-interval 1 n))
)

(define (prime? num)
    (define (is-prime? num d)
        (if (<= d 1)
            #t
            (if (= (modulo num d) 0)
                #f
                (is-prime? num (- d 1))
            )
        )
    )
    (is-prime? num (- num 1))
)

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (filter prime-sum? (unique-pairs n))
)

(prime-sum-pairs 10)
