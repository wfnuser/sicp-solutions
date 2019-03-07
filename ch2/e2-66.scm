(define (entry binary-tree)
    (car binary-tree)
)

(define (left binary-tree)
    (cadr binary-tree)
)

(define (right binary-tree)
    (caddr binary-tree)
)

(define (lookup given-key binary-tree)
    (cond ((null? binary-tree) #f)
        ((equal? given-key (entry binary-tree)) #t)
        ((> given-key (entry binary-tree)) (lookup given-key (right binary-tree)))
        ((< given-key (entry binary-tree)) (lookup given-key (left binary-tree)))
    )
)

