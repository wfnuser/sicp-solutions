(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers
    (cons-stream 1 (add-streams ones integers)))

(define s 
    (cons-stream 1 
            (mul-streams s (stream-cdr integers))
    ))
