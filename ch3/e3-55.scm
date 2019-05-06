(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers
    (cons-stream 1 (add-streams ones integers)))


(define (partial-sums s) 
   (add-streams s (cons-stream 0 (partial-sums s)))) 
