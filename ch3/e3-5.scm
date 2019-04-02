(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))))

(define (in-circle)
    (let ((x (random-in-range 0.0 1.0)) (y (random-in-range 0.0 1.0)))
        (if (< (+ (* x x) (* y y)) 1)
        #t
        #f)
    )

)

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0) (/ trials-passed trials))
            ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
            (else (iter (- trials-remaining 1) trials-passed))    
        )
    )
    (iter trials 0)
)

(define (get-pi trials)
    (* 4.0 (monte-carlo trials in-circle))
)

(get-pi 100000)
