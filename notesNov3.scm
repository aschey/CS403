(define (scons # x $y)
  (cons x
        (cons # $y)
        )
  )
; # + $y = thunk

(define (scar c) (car c))

(define (scdr c) (eval (cddr c) (cadr c)))

(define ones (scons 1 (lambda () ones)))

(inspect (scar (scdr ones)))

(define (ints-from n)
  (scons n (ints-from (+ n 1))))

(inspect (scar (scdr (ints-from 1))))

(define naturals (ints-from 1))

(define fibs
  (scons 0
         (scons 1
                (sadd 
                  fibs
                  (scdr fibs)
                  )
                )
         )
  )

(define facts
  (scons 1
         (smul
           (ints-from 2)
           facts
           )
         )
  )

(define (shuffle s t)
  (scons
    (scar s)
    (scons
      (scar t)
      (shuffle (scdr s) (scdr t)))
    )
  )

(define (better-shuffle s t)
  (scons
         (scar s)
         (better-shuffle t (scdr s))
         )
  )
