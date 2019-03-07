(define (intersection-tree tree another)
    (list->tree
        (intersection-set (tree->list-2 tree)
                          (tree->list-2 another))))

(define (union-tree tree another)
        (list->tree
            (union-set (tree->list-2 tree)
                            (tree->list-2 another))))