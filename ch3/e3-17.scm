(define (count-pairs x)
    (length (inner x '())))

(define (inner x memo-list)
    (if (and (pair? x)
             (false? (memq x memo-list)))
        (inner (car x)
               (inner (cdr x)
                      (cons x memo-list)))
        memo-list))

(count-pairs (cons (cons 1 2) (cons 3 4)))