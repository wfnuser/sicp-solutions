(define (cube a)
    (* a a a)
)

(define (sum-cubes a b)
    (if (> a b)
        0
        (+ (cube a) (sum-cubes (+ a 1) b))
    )
)

(define (pi-sum a b)
    (if (> a b) 
        0
        (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))    
    )
)

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))
    )
)

(define (inc n) (+ n 1))

(sum-cubes 1 5)
(pi-sum 1 50)