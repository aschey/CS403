(define (author)
  (println "AUTHOR: Austin Schey aeschey@crimson.ua.edu")
  )


(define (nonlocals func)
  (define (flatten l)
    (cond
      ((null? l) l)
      ((pair? (car l)) (append (flatten (car l)) (flatten (cdr l))))
      ((atom? (car l)) (append (list (car l)) (flatten (cdr l))))
      )
    )

  (define params (get 'parameters func))
  (define code (flatten (get 'code func)))
  (inspect code)
  (define (variable? atom)
    (and (not (string? atom)) (not (number? atom)))
    )


  (define (addVar varsToReturn otherVars var)
    (if (or (member? var varsToReturn) (member? var otherVars))
      varsToReturn        
      (cons var varsToReturn)
      )
    )

  (define (iter remaining nonLocalVars localVars)
    (inspect localVars)
    (cond
      ((null? remaining) nonLocalVars)
      ((eq? (car remaining) 'define)
       ; add "define" and the value it defines
       (iter (cddr remaining) (addVar nonLocalVars localVars (car remaining)) (addVar localVars nonLocalVars (cadr remaining)))
       )
      ((eq? (car remaining) 'quote)
       ; skip the quoted value
       (iter (cddr remaining) (addVar nonLocalVars localVars (car remaining)) localVars)
       )
      (else
        (if (variable? (car remaining))
          (iter (cdr remaining) (addVar nonLocalVars localVars (car remaining)) localVars)
          (iter (cdr remaining) nonLocalVars localVars)
          )
        )
      )
    )

  (iter code (list) params)
  )

(define (run1)
  (define (test x a b)
    (define c 2)
    (let
      ((d 2)
       (e 3)
       )
      )
    (cond
      ((not (null? b))
       (inspect a)
       (inspect x)
       'la
       (+ a 1.2)
       (+ c 2)
       )
      )
    )
  (inspect (nonlocals test))
  )

;(run1)

(define (run2)
  (define (fib n)
    (cond
      ((< n 2) n)
      (else (+ (fib (- n 1)) (fib (- n 2))))
      )
    )
