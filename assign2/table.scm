;(define (assoc x y)
;    (cond
;        ((null? y) #f)
;        ((equal? x (caar y)) (car y))
;        (else (assoc x (cdr y)))
;        )
;    )

(define table nil)

(define (clearTable) (set! table nil))

(define (putTable tag types function)
    (set! table (cons (list (list tag types) function) table))
    )

(define (getTable tag types )
    (define result (assoc (list tag types) table))
    (if (eq? result #f) nil (cadr result))
    )

(define (removeTable tag)
    (define (iter prev items)
        (inspect items)
        (cond
            ((null? items) #f)
            ((equal? tag (car (caar items)))
                (if (null? prev)
                    (set! table (cdr items))
                    (set-cdr! prev (cdr items))
                    )
                #t
                )
            (else
                (iter items (cdr items))
                )
            )
        )
    (iter nil table)
    )


;(putTable 'x '(y) 'z)
;(putTable '(a 1) 'b 3)
;(putTable 3 '(z z) 4)
;
;(inspect (getTable 'x '(y)))
;(inspect (getTable '(a 1) 'b))
;(inspect (getTable 3 '(z z)))
;(inspect (getTable 4 4))
;
;(inspect table)
;(println "removing 3...")
;(removeTable 3)
;(println "removing (a 1)...")
;(removeTable '(a 1))
;(inspect table)

