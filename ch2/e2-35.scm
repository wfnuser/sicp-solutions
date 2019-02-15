(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

; 没懂。。。 稍微有点感觉
; 但 sub-tree 难道不是一直和tree一样吗
(define (count-leaves tree)
    (accumulate +
                0   
                (map (lambda (sub-tree)
                    (newline)
                    (display sub-tree)
                    (if (pair? sub-tree)           ; 如果这个节点有分支
                        (count-leaves sub-tree)    ; 那么这个节点调用 count-leaves 的结果就是这个节点的树叶数量
                        1))                        ; 遇上一个叶子节点就返回 1
                    tree)))

(define test-tree (list 1 2 (list 3 4 (list 6 7)) 5))


; 我自己的解法，一开始没写完整 后来补全了如下
(define (enumurate-tree tree)
    (cond ((null? tree) '())
        ( (not (pair? tree)) (list tree))
        (else (append (enumurate-tree (car tree)) (enumurate-tree (cdr tree))))
    )
)

(define (my-count-leaves tree)
    (accumulate + 0 (map (lambda(x) 1) (enumurate-tree tree)))
)

(my-count-leaves test-tree)
