(define (svdisplay s n)
  (cond
    ((= n 1) (print (scar s)))
    ((> n 2) (print (scar s) " ") (svdisplay s n) (println " ...]"))
    (else nil)
    )
  )
(define (sdisplay s n) (print "[") (svdisplay s n) (println " ...]"))
(define (sop op s t) (scons (op (scar s) (scar t)) (sop op (scdr s) (scdr t))))
(define (smap f s) (scons (f (scar s)) (smap f (scdr s))))

(define (delay # $x)
  (cons $x #)
  )
(define (force x)
  (eval (car x) (cdr x))
  )


(define (integral s x dx)
  (define int
    (scons
      x
      (sop +
           (smap (lambda (a) (* a dx)) (force s))
           int)))
    int
    )

(define y (integral (delay y) 1 .001))
(sdisplay y 1)
(sref y 1000)

; problem 5: barrier object has a count, when thread hits it, increment count. limit set with set method. threads loop as long as count doesn't hit limit. first time thread seen, increment count. else, just loop. when third thread hits, go to next phase. use gettid to store list of threads seen so far. have to protect instance variables of barrier object using semaphore. can't have two threads updating same instance variable.

; problem 2
(replace '+ + f)
(define (replace sym val)
  ((eq? (car code) sim)
   (set-car! code val)
