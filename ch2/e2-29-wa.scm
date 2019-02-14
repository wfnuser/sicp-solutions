(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define (left-branch mobile)
    (car mobile)
)

(define (right-branch mobile)
    (cadr mobile)
)

(define (branch-length branch)
    (car branch)
)

(define (branch-structure branch)
    (cadr branch)
)

(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile))
        (branch-weight (right-branch mobile)))
)

(define (branch-weight branch)
    (if (branch-is-mobile? branch)
        (total-weight (branch-structure branch))
        (branch-structure branch)
    )
)

(define (branch-is-mobile? branch)
    (pair? (branch-structure branch))
)

(define mobile (make-mobile (make-branch 10 20) (make-branch 10 25)))

(total-weight mobile)

(define (if-balanced? tree)
    (let ((left (left-branch tree)) (right (right-branch tree)))
        (if  (not ( = (branch-weight left) (branch-weight right))
            (#f)
            (and (if-balanced? (branch-structure left)) (if-balanced? (branch-structure right)))
        )
    )
)

(if-balanced? mobile)