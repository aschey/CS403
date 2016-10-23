(define (author)
  (println "AUTHOR: Austin Schey aeschey@crimson.ua.edu")
  )

; Problem 1
(define (iterate # $var vals $)
  (define f (append (list 'lambda (list $var)) $))
  (define func (eval f #))
  (define (iterList items)
    (cond 
      ((not (null? items))
       (func (car items))
       (iterList (cdr items))
       )
      )
    )
  (iterList vals)
  )

(define (run1)
  (iterate i (list 1 2 3) 
           (inspect i) 
           (inspect (* i i)) 
           (inspect (* i i i )))
)
;(run1)

; End problem 1

; Problem 2

(define (peval f @)
  (define (callFunc remaining params)
    (define func (cons 'f (merge remaining params (list))))
    (eval func this)
    )
  (define (merge remaining params store)
    (cond
      ((= (length params) 0) store)
      (else
        (let ((curParam (car params))
              (nextParams (cdr params)))
              (if (eq? curParam 'MISSING) 
                (merge (cdr remaining) nextParams (append store (list (car remaining))))
                (merge remaining nextParams (append store (list curParam)))
                )
              )
        )
      )
    )
  (define params @)
  (lambda (@) (callFunc @ params))
)

(define (run2)
  (define (f a b c) (inspect a) (inspect b) (inspect c))
  (define . 'MISSING)
  (inspect ((peval f 1 . 2) 1 2))
  ;(inspect (peval f 1 2 .))
  )
;(run2)

; End problem 2

; Problem 3

(define (Stack) (_Stack (list) 0))

(define (_Stack store size) this)

(define (storeEmpty stack)
  (= (length (stack 'store)) 0)
  )

(define (push stack val)
  (_Stack (cons val (stack 'store)) (+ (stack 'size) 1))
  )

(define (pop stack)
  (if (storeEmpty stack)
    nil
   (_Stack (cdr (stack 'store)) (- (stack 'size) 1))
   )
  )

(define (speek stack)
  (if (storeEmpty stack)
    nil
    (car (stack 'store))
    )
  )

(define (ssize stack)
  (stack 'size)
  )

(define (Queue) (_Queue (Stack) (Stack)))
  
(define (_Queue inbox outbox) this)

(define (outboxEmpty queue)
  (= (ssize (queue 'outbox)) 0)
  )

(define (inboxEmpty queue)
  (= (ssize (queue 'inbox)) 0)
  )

(define (enqueue queue val)
  (_Queue (push (queue 'inbox) val) (queue 'outbox))
  )

(define (makeOutbox in out)
  (cond 
    ((> (ssize in) 0)
     (define nextVal (speek in))
     (makeOutbox (pop in) (push out nextVal))
     )
    (else
      (_Queue in out)
      )
    )
  )

(define (dequeue queue)
  (cond 
    ((outboxEmpty queue)
     (if (inboxEmpty queue)
       nil
       (dequeue (makeOutbox (queue 'inbox) (queue 'outbox)))
       )
     )
    (else (_Queue (queue 'inbox) (pop (queue 'outbox))))
    )
  )

(define (qpeek queue)
  (cond
    ((outboxEmpty queue) 
     (if (inboxEmpty queue)
       nil
       (qpeek (makeOutbox (queue 'inbox) (queue 'outbox)))
       )
     )
    (else (speek (queue 'outbox)))
    )
  )

(define (qsize queue)
  (+ (ssize (queue 'inbox)) (ssize (queue 'outbox)))
  )

(define (run3)
  (define s (Stack))
  (inspect (ssize s))
  (define s2 (push s 3))
  (inspect (speek s2))
  (inspect (ssize s2))
  (define s3 (push s2 4))
  (inspect (speek s3))
  (inspect (ssize s3))
  (define s4 (pop s3))
  (inspect (speek s4))
  (inspect (ssize s4))
  (define s5 (pop s4))
  (inspect (speek s5))
  (inspect (ssize s5))
  )

(define (run3test)
  (define q (Queue))
  (inspect (qsize q))
  (define q2 (enqueue q 1))
  (inspect (qpeek q2))
  (inspect (qsize q2))
  (define q3 (enqueue q2 2))
  (inspect (qsize q3))
  (define q4 (dequeue q3))
  (inspect (qpeek q4))
  (inspect (qsize q4))
  (define q5 (enqueue q4 3))
  (inspect (qsize q5))
  (define q6 (dequeue q4))
  (inspect (qpeek q5))
 ) 

;(run3)
;(run3test)

; End problem 3

; Problem 4

(define (no-locals code)
  (define def (car code))
  (define signature (cadr code))

  (define (construct sig body params)
    (cons (list 'lambda sig body) params)
    )
  (define (any? search ref)
    (cond
      ((null? search) #f)
      ((member? (car search) ref) #t)
      (else (any? (cdr search) ref))
      )
    )

  (define (no-locals-iter remaining sig body params)
    (cond
      ((null? remaining)
       (construct sig body params)
       )
       (else
         (define current (car remaining))
         (cond
           ((or (atom? current) (not (eq? (car current) 'define)))
              (no-locals-iter (cdr remaining) sig current params)
            )
           (else
            (define varName (cadr current))
            (define defBody (caddr current))
            (if (any? sig defBody)
              (construct sig (no-locals-iter (cdr remaining) (list varName) body (list defBody)) params) 
              (no-locals-iter (cdr remaining) (cons varName sig) body (cons defBody params))
              )
            )
           )
         )
       )
    )
   (define funcBody (no-locals-iter (cddr code) (list) (list) (list)))
   (list def signature funcBody)
  )

(define (run4)
  ;(inspect (no-locals (quote (define (nsq a) (define x (+ 1 1)) (define y (+ x x)) (+ x y)))))
  ;(inspect (no-locals (quote (define (nsq a) (define x (+ 1 1)) (+ x x)))))
  (define (nsq a) (define i (+ 1 1)) (define x (+ a 1)) (define y (+ x x)) (define j (+ 2 2)) (* y y x i j))
  (inspect (nsq 5))
  (inspect ((eval (no-locals (quote (define (nsq a) (define i (+ 1 1)) (define x (+ a 1)) (define y (+ x x)) (define j (+ 2 2)) (* y y x i j)))) this) 5))
  (inspect (no-locals '(define (f) (define x 3) (+ x 1))))
  )

;(run4)

; End problem 4

; Problem 5

(define (pred churchNum)

  (define (makePair first)
    (lambda (second) 
      (lambda (firstOrSecond) 
        (if (eq? firstOrSecond #t)
          first
          second
          )
        )
      )
    )

  (define (getFirst pair)
    (pair #t)
    )

  (define (getSecond pair)
    (pair #f)
    )

  (lambda (next) 
    (lambda (first)
      (define predFirst ((makePair #t) first))
      (define predNext 
        (lambda (pair) 
          (if (getFirst pair)
            ((makePair #f) first)
            ((makePair #f) (next (getSecond pair)))
            )
          )
        )
      (define finalPair ((churchNum predNext) predFirst))
      ;(inspect finalPair)
      (getSecond finalPair)
      )
    )
  )

(define (run5)
  (define (testFunc x) (+ x 1))
  (define zero (lambda (f) (lambda (x) x)))
  (define one (lambda (f) (lambda (x) (f x))))
  (define two (lambda (f) (lambda (x) (f (f x)))))
  (define (predTest num val)
    (((pred num) testFunc) val)
    )
  
  (inspect (predTest two 2))
  )
;(run5)
; End problem 5

; Problem 6
(define (treeNode val left right)
  (list val left right)
  )

(define (accumulate op base l)
  (cond
    ((null? l) base)
    (else (op (car l) (accumulate op base (cdr l))))
    )
  )

(define (treeflatten tree)

  (define (addDepth tree depth)
    (if (nil? tree)
      nil
      (list depth (car tree))
      )
    )

  (define (leaf? node)
    (and (nil? (cadr node)) (nil? (caddr node)))
    )

  (define (recBranch func current depth storeOp)
    (rec (func current) (+ depth 1) storeOp)
    )

  (define (rec current depth store)
    (cond
      ((nil? current) store)
      ((leaf? current)
       (append 
         (recBranch cadr current depth (cons (addDepth current depth) store))
         (recBranch caddr current depth nil))
       )
      (else
        (append
          (recBranch cadr current depth store)
          (recBranch caddr current depth store))
        )
      )
    )

  (rec tree 0 ())
  )

(define (treedepth tree)
  (define depths (map (lambda (node) (car node)) (treeflatten tree)))
  (define total (accumulate + 0.0 depths))
  (/ total (length depths))
  )



(define (run6)
  
  (inspect (treedepth (treeNode 1 (treeNode 6 nil nil) (treeNode 2 (treeNode 5 nil nil) (treeNode 3 nil (treeNode 4 nil nil))))))
  (inspect (treedepth (treeNode 1 (treeNode 2 nil nil) (treeNode 3 (treeNode 4 nil nil) nil))))
  )
;(run6)

; End problem 6

; Problem 7

(define (filter pred l)
  (cond
    ((nil? l) l)
    ((eq? (pred (car l)) #t) (cons (car l) (filter pred (cdr l))))
    (else (filter pred (cdr l)))
    )
  )

(define (enumerateInterval low high)
  (if (= low high)
    nil
    (cons low (enumerateInterval (+ low 1) high))
    )
  )

(define (flatmap f l)
  (accumulate append () (map f l))
  )

(define (adjoinPosition newCol k restOfQueens)
  (cons (cons (- k 1) newCol) restOfQueens)
  )

(define (safe? k positions)
  (define (difference func point1 point2)
    (abs (- (func point1) (func point2)))
    )

  (define (diagonal? point1 point2)
    (= (difference car point1 point2) (difference cdr point1 point2))
    )

  (define (canAttack? point1 point2)
    (cond
      ((eq? (cdr point1) (cdr point2)) #t)
      ((diagonal? point1 point2) #t)
      (else #f)
      )
    )

  (define newQueen (car positions))
  (define rest (cdr positions))

  (define (iter otherQueens)
    (cond
      ((nil? otherQueens) #t)
      ((canAttack? newQueen (car otherQueens)) #f)
      (else (iter (cdr otherQueens)))
      )
    )
  (iter rest)
  )

(define (queens boardSize)
  (define (queenRows k)
    (if (= k 0)
      (list nil)
     (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (restOfQueens)
            (map (lambda (newCol)
                   (adjoinPosition newCol k restOfQueens))
                 (enumerateInterval 0 boardSize)))
          (queenRows (- k 1))
          )
        )
      )
   )
  (define result (queenRows boardSize))
  (if (eq? result nil)
    (list result)
    result
    )
  )

(define (run7)
  (inspect (queens 6))
  ;(inspect (safe? 8 (list (cons 1 4) (cons 2 3) (cons 5 2) (cons 1 1) (cons 6 0))))
  ;(inspect (safe? 8 '((1 . 4) (2 . 3) (5 . 2) (1 . 1) (6 . 0))))
  )
;(run7)

; End problem 7

; Problem 8

(define (cxr ops)
  (define (addOp op rest)
    (cons op (list rest))
    )

  (define body 
    (accumulate 
      (lambda (op rest)
        (cond
          ((equal? op "a") (addOp 'car rest))
          ((equal? op "d") (addOp 'cdr rest))
          )
        )
      'l
      (string ops)
      )
    )
    (eval (list 'lambda '(l) body) this)
  )

(define (run8)
  (inspect ((cxr 'add) (list 1 2 3 4 5)))
  )

;(run8)

; Problem 9

(include "table.scm")

(define old+ +)
(define old- -)
(define old* *)
(define old/ /)

(define (getOld op)
  (cond
    ((eq? op '+) old+)
    ((eq? op '-) old-)
    ((eq? op '*) old*)
    ((eq? op '/) old/)
    )
  )

(define (apply-generic op a b)
  (define (defined? func)
    (not (eq? func nil))
    )

  (define (coerce var)
    (cond
      ((eq? (type var) 'INTEGER) (string var))
      ((eq? (type var) 'STRING) (integer var))
      (else var)
      )
    )


  (define (getFunc x y)
    (define func (getTable op (list (type x) (type y))))
    (if (defined? func)
      (lambda () (func x y))
      nil
      )
    )

  (define func (getFunc a b))
  (cond 
    ((defined? func) (func))
    (else
      (define coercedFunc (getFunc a (coerce b)))
      (if (defined? coercedFunc)
        (coercedFunc)
        ((getTable op (list 'INTEGER 'INTEGER)) a b)
        )
      )
    )
  )

(define (addStrings a b)
  (string+ a b)
  )

(define (addStringInteger a b)
  (string+ a (string b))
  )

(define (subtractStringInteger a b)
  (define (iter current remaining)
    (if (= remaining 0)
      current
      (iter (cdr current) (- remaining 1))
      )
    )
  (iter a b)
  )

(define (mulStringInteger a b)
  (define (iter result remaining)
    (if (= remaining 0)
      result
      (iter (string+ a result) (- remaining 1))
      )
    )
  (iter "" b)
  )

(define (install-generic)
  (clearTable)
  (set! + (lambda (a b) (apply-generic '+ a b)))
  (set! - (lambda (a b) (apply-generic '- a b)))
  (set! * (lambda (a b) (apply-generic '* a b)))
  (set! / (lambda (a b) (apply-generic '/ a b)))
  (putTable '+ '(INTEGER INTEGER) old+)
  (putTable '- '(INTEGER INTEGER) old-)
  (putTable '* '(INTEGER INTEGER) old*)
  (putTable '/ '(INTEGER INTEGER) old/)
  (putTable '+ '(STRING STRING) addStrings)
  (putTable '- '(STRING INTEGER) subtractStringInteger)
  (putTable '* '(STRING INTEGER) mulStringInteger)
  'generic-system-installed
  )

(define (uninstall-generic)
  (set! + old+)
  (set! - old-)
  (set! * old*)
  (set! / old/)
  )

(define (run9)
  (install-generic)
  (inspect (+ 1 (list 1 2)))
  (inspect (+ "1" "1"))
  (inspect (+ 123 "4"))
  (inspect (- 123 "4"))
  (inspect (- "abc" 2))
  (inspect (* "abc" 3))
  (inspect (* 3 "33"))
  (inspect (/ 8 "2"))
  (inspect (/ "8" 2))
  (uninstall-generic)
  ;(inspect (/ "8" 2))
  )

(run9)

; End problem 9

; Problem 10 functions

(define (install-coercion)
  (clearTable)
  (putTable 'coerce '(REAL) real)
  (putTable 'coerce '(STRING) string)
  (putTable 'coerce '(INTEGER) integer)
  'coercion-installed
  )

(define (coerce val newType)
  ((getTable 'coerce (list newType)) val)
  )

(define (run10)
  (install-coercion)
  (inspect (coerce 2 'REAL))
  (inspect (coerce 2 'STRING))
  (inspect (coerce 2.000 'INTEGER))
  (inspect (coerce 2.000 'STRING))
  (inspect (coerce "2" 'INTEGER))
  (inspect (coerce "2" 'REAL))
  ; Need clarification
  (inspect (coerce (quote (1 (2 . 2) ((3 4) "5"))) 'STRING))
  )
;(run10)

; End Problem 10

(println  "assignment 2 loaded!")
