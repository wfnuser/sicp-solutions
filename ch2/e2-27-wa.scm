(define x (list (list 1 2) (list 3 4)))

(define (fake-deep-reverse tree)
    (cond ((null? tree) '())
        ((not (pair? tree)) tree)
        (else (cons (deep-reverse (cdr tree)) (deep-reverse (car tree)) ))
    )
)

; https://sicp.readthedocs.io/en/latest/chp2/27.html
; actully it's a wrong answer too.
(define (deep-reverse tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) tree)
          (else (reverse (list (deep-reverse (car tree)) (deep-reverse (cadr tree)))))))

(deep-reverse x)
(deep-reverse (list 1 2 3 4))
