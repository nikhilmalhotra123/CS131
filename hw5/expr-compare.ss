#lang racket
(provide (all-defined-out))

(define (lambda? x)
  (member x '(lambda λ)))

(define (lambda-expr? x)
  (and (list? x) (= (length x) 3) (lambda? (car x)))
)

(define (if? x)
  (equal? x 'if)
)

(define (if-expr? x)
  (and (list? x) (= (length x) 4) (if? (car x)))
)

(define (lambda-choose x y)
  (if (equal? x y) x 'λ)
)

(define (value-bind x y)
  (if (equal? x y) x (string->symbol (string-append (string-append (symbol->string x) "!") (symbol->string y))))
)

(define (get-bindings bindings1 bindings2 x y)
  (cond
    [(equal? (length x) 0)
     (list bindings1 bindings2)
    ]
    [else
     (get-bindings (hash-set bindings1 (car x) (value-bind (car x) (car y)))
                   (hash-set bindings2 (car y) (value-bind (car x) (car y)))
                   (cdr x) (cdr y))
    ]
  )
)

(define (apply-bindings binding body)
  (cond
    [(lambda-expr? body)
      (map (lambda (b) (apply-bindings (car (get-bindings binding binding (cadr body) (cadr body))) b)) body) 
    ]
    [(list? body)
     (map (lambda (b) (apply-bindings binding b)) body)
    ]
    [(hash-has-key? binding body)
     (hash-ref binding body)
    ]
    [else body]
  )
)

(define (lambda-compare x y)
  (cond
        [(not (equal? (length (cadr x)) (length (cadr y)))) ; if args aren't same length
            (list 'if '% x y)
        ]
        [else
         (letrec ((bindings (get-bindings (hash) (hash) (cadr x) (cadr y))) (x-bind (car bindings)) (y-bind (cadr bindings)))
           (cons (lambda-choose (car x) (car y)) (cons (apply-bindings x-bind (cadr x)) (list (expr-compare (apply-bindings x-bind (caddr x)) (apply-bindings y-bind (caddr y))))))
         )
        ]
  )
)

(define (expr-compare x y)
  (cond
    [(equal? x y) ; if both are equal, return one of them
     x
    ]
    [(and (boolean? x) (boolean? y)) ;both diffent bools, if they were same, case above would've executed
     (if x '% '(not %))
    ]
    
    ;The following have the same output but are split for readability
    [(or (not (list? x)) (not (list? y))) ;if both aren't lists
     (list 'if '% x y)
    ]
    [(or (not (equal? (length x) (length y))) (= (length x) 0) (= (length y) 0)) ;if they have different lengths
     (list 'if '% x y)
    ]
    [(or (equal? (car x) 'quote) (equal? (car y) 'quote)) ; if at least one is quoted
     (list 'if '% x y)
    ]
    [(and (or (if-expr? x) (if-expr? y)) (not (equal? (car x) (car y))))
     (list 'if '% x y)
    ]
    [(and (lambda-expr? x) (lambda-expr? y))
     (lambda-compare x y)
    ]
    [(or (lambda-expr? x) (lambda-expr? y))
     (list 'if '% x y)
    ]
    [else
     (let ((head (expr-compare (car x) (car y))))
       (cons head (expr-compare (cdr x) (cdr y))))
    ]
   )
)

(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

(define test-expr-x `(cons (if 0 'lambda 'if) (cons 'hello ((lambda (a c) (- (+ a 1) ((lambda (g) g) 6))) 2 7))))
(define test-expr-y `(cons (if 2 'p 'q) (cons 11 ((λ (b c) (/ (+ b 2) ((lambda (f) f) 4))) 3 5))))
