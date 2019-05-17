(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
            ((and? exp) (eval-and exp env))
            (else
                (error "Unknown expression type: EVAL" exp))))

(define (eval-and exps env)
    (cond ((null? exps) #t)
        ((last-exp? (first-exp exps)) (eval (first-exp exps)))
        ((true? (eval (first-exp exps) env)) (eval-and (rest-exp exps) env))
        (else #f)
    )
)