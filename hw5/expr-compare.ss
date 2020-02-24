#lang racket
(define (expr-compare x y)
  (cond
    [(and (list? x) (list? y)) ;if both are lists
     (cond
       [
        (equal? x y) ;ne they're equal output either oen
        x
       ]
       [
        (= (length x) (length y))
        '(write this)
       ]
       [else
        (list 'if '% x y)
       ]
     )
    ]
    [else
     '(compare expressions TODO)
    ]
   )
)