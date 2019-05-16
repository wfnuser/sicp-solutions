(define (eval exp env)
    (if (self-evaluating? exp) 
        exp
        (let ((eval-proc (get 'eval (expression-type exp))))
            (eval-proc (expression-content exp) env)
        )
    )
)

(define (intall-variable)
    (put 'eval 'variable ((lambda (exp env) lookup-variable-value exp env)))
    (put 'eval 'quoted ((lambda (exp env) (text-of-quotation exp))))
    ;...
)

        
; a) it will consider define as a procedure and try to call it by send x & 3 as parameters
; b) 
; ((procedure-call? exp)                              
;             (apply (eval (operator exp) env)                
;                    (list-of-values (operands exp) env)))    