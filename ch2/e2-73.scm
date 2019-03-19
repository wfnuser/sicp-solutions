; a. Because if the exp is a number or  variable we can't get operator
; b.

(define (deriv exp var) 
    (cond ((number? exp) 0)
        ((variable? exp) 
            (if (same-variable? exp var) 1 0)) 
        (else ((get 'deriv (operator exp))
            (operands exp) var))))


(define (operator exp) (car exp)) 
(define (operands exp) (cdr exp))


(define (install-sum-package)
    (define (addend s) (car s))
    (define (augend s) (cadr s))

    (define (make-sum x y)
        (cond ((=number? x 0) y)
            ((=number? y 0) x)
            ((and (number? x) (number? y))
                (+ x y))
            (else (attach-tag '+ x y))
        )

    (define (deriv-sum exp var) (
        make-sum (deriv (addend x) var) (deriv (augend x) var)
    ))

    (put 'addend '+ addend)
    (put 'augend '+ augend)
    (put 'make-sum '+ make-sum)
    (put 'deriv '+ deriv-sum)
)

(define (make-sum x y)
    ((get 'make-sum '+) x y))

(define (addend sum)
    ((get 'addend '+) (contents sum)))

(define (augend sum)
    ((get 'augend '+) (contents sum)))


(install-sum-package)

; c.
(define (install-exponentiation-package)

    ;;; internal procedures
    (define (base exp)
        (car exp))

    (define (exponent exp)
        (cadr exp))

    (define (make-exponentiation base n)
        (cond ((= n 0)
                0)
              ((= n 1)
                base)
              (else
                (attach-tag '** base n))))

    ;;; interface to the rest of the system
    (put 'base '** base)
    (put 'exponent '** exponent)
    (put 'make-exponentiation '** make-exponentiation)

    (put 'deriv '**
         (lambda (exp var)
            (let ((n (exponent exp))
                  (u (base exp)))
                (make-product
                    n
                    (make-product
                        (make-exponentiation
                            u
                            (- n 1))
                        (deriv u var))))))

'done)

(define (make-exponentiation base n)
    ((get 'make-exponentiation '**) base n))

(define (base exp)
    ((get 'base '**) (contents exp)))

(define (exponent exp)
    ((get 'exponent '**) (contents exp)))