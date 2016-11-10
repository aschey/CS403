(define (adder a1 a2 sum)
  (define obj this)
  (define (newInput)
    (cond
      ((and (a1 'set?) (a2 'set?))
       ((sum 'set!)
        (+ (a1 'get) (a2 'get))
        obj
        )
       )
