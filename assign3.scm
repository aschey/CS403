(define (author)
  (println "AUTHOR: Austin Schey aeschey@crimson.ua.edu")
  )


(define (nonlocals func)
  (define params (get 'parameters func))
  (define code (get 'code func))
  (define (variable? atom)
    (and (not (string? atom)) (not (number? atom)))
    )


  (define (addVar var varsToReturn otherVars)
    (cond 
      ((or (member? var varsToReturn) (member? var otherVars))
       varsToReturn
       )
      (else
       (cons var varsToReturn)
       )
      )
    )

  (define (iter remaining nonLocalVars localVars)
    (cond
      ((null? remaining) 
       nonLocalVars
       )
      (else
        (cond
          ((pair? remaining)
           (define current (car remaining))
           (cond
             ((and (pair? current) (eq? (car current) 'define))
              (cond 
                ((pair? (cadr current))
                 (define params (cdr (cadr current)))
                 (define code (cddr current))
                 (iter (cdr remaining) (iter code nonLocalVars params) localVars)
                 )
                (else
                  (iter (cdr remaining) nonLocalVars (addVar (cadr current) localVars nonLocalVars))
                  )
                )
              )
             ((eq? current 'quote)
              (iter (cddr remaining) (addVar current nonLocalVars localVars) localVars)
              )
             (else
               (iter (cdr remaining) (iter current nonLocalVars localVars) localVars)
               )
             )
           )
          (else
            (cond
              ((variable? remaining)
               (addVar remaining nonLocalVars localVars)
               )
              (else
                nonLocalVars
                )
              )
            )
          )
        )
      )
    )
          
  (iter code (list) params)
  )

(define (run1)
  (define (test x a b)
    (define c 2)
    (define (f g)
      g
      x
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
  (define (test2)
    #t
    )
  (inspect (nonlocals test))
  )

;(run1)

(define (replace func sym newSym)
  (define code (get 'code func))
  (define (iter remaining code)
    (cond
      ((builtin? remaining)
       remaining
       )
      ((null? remaining)
       code
       )
      ((pair? remaining)
       (iter (cdr remaining) (append code (list (iter (car remaining) nil))))
       )
      ((eq? remaining sym)
        newSym
        )
      (else
        remaining
        )
      )
    )
  (define newCode (iter code (list)))
  (set 'code newCode func)
  func
  )


(define (run2)
  (define (fib n)
    (cond
      ((< n 2) n)
      (else (+ (fib (- n 1)) (fib (- n 2))))
      )
    )
  ;(inspect (fib 26))
  (define repFib (replace fib '- -))
  (define dRepFib (replace repFib '+ +))
  ;(inspect (dRepFib 26))
  )
;(run2)

(define (avl)
  (define (avlNode val)
    (define left nil)
    (define right nil)
    (define height 0)
    (define parent nil)
    this
    )

  (define (queue)
    (define store (list))
    (define (enqueue val)
      (set! store (append store (list val)))
      )
    (define (dequeue)
      (define val (car store))
      (set! store (cdr store))
      val
      )
    (define (size)
      (length store)
      )
    this
    )

  (define root nil)
  (define treeSize 0)

  (define (insert x)
    (define node (avlNode x))
    (cond
      ((eq? root nil)
       (set! root node)
       )
      (else
        (_insert root node)
        (setBalance node)
        (insertionFixup node)
        )
      )
    (set! treeSize (+ treeSize 1))
    )

  (define (size)
    treeSize
    )

  (define (printTree)
    (_printTree root)
    )

  (define (_insert curRoot node)
    (cond
      ((< (node 'val) (curRoot 'val))
       (cond
         ((eq? (curRoot 'left) nil)
          (setLeftChild curRoot node)
          )
         (else
           (_insert (curRoot 'left) node)
           )
         )
       )
      (else
       (cond
         ((eq? (curRoot 'right) nil)
          (setRightChild curRoot node)
          )
         (else
          (_insert (curRoot 'right) node)
          )
         )
       )
      )
    )

  (define (prune node)
    (define parent (node 'parent))

    (cond
      ((eq? parent nil)
       (set! root nil)
       )
      ((isLeftChild node)
       (setLeftChild parent nil)
       )
      (else
        (setRightChild parent nil)
        )
      )
    )

  (define (_printTree curRoot)
    (cond
      ((not (eq? (curRoot nil)))
       (_printTree((curRoot 'left)))
       (println (curRoot 'val))
       (_printTree((curRoot 'right)))
       )
      )
    )

  (define (insertionFixup node)
    (define break #f)
    (while (and (not (eq? node root)) (not break))
           (define parent (node 'parent))
           (define parentFav (favorite parent))
           (define sib (sibling node))
           (cond
             ((and (not (eq? sib nil)) (eq? parentFav sib))
              (setBalance parent)
              (set! break #t)
              )
             ((isBalanced parent)
              (setBalance parent)
              (set! node parent)
              )
             (else
               (define fav (favorite node))
               (cond
                 ((not (eq? fav nil))
                  (cond
                    ((and (not (isLinear node fav)) (not (isLinear parent node)))
                     (rotate node fav)
                     (rotate parent fav)
                     (setBalance node)
                     (setBalance parent)
                     (setBalance fav)
                     )
                    )
                  )
                 (else
                   (rotate parent node)
                   (setBalance parent)
                   (setBalance node)
                   )
                 )
               )
             )
           )
    )

  (define (find val)
    (_find val root)
    )

  (define (_find val curRoot)
    (cond
      ((eq? curRoot nil)
       #f
       )
      ((< val (curRoot 'val))
       (_find val (curRoot 'left))
       )
      ((> val (curRoot 'val))
       (_find val (curRoot 'right))
       )
      (else
        #t
        )
      )
    )

  (define (setBalance node)
    (define leftHeight 0)
    (define rightHeight 0)
    (if (not (eq? (node 'left) nil))
      (set! leftHeight ((node 'left) 'height))
      )
    (if (not (eq? (node 'right) nil))
      (set! rightHeight ((node 'right) 'height))
      )
    (define nodeHeight (node 'height))
    (set 'height (+ (maxHeight leftHeight rightHeight) 1) node)
    )

  (define (rotate parent child)
    (define grandparent (parent 'parent))

    (cond
      ((isLeftChild child)
       (setLeftChild parent (child 'right))
       (setRightChild child parent)
       )
      (else
        (setRightChild parent (child 'left))
        (setLeftChild child parent)
        )
      )

    (cond
      ((eq? grandparent nil)
       (set! root child)
       )
      ((eq? (grandparent 'left) parent)
       (setLeftChild grandparent child)
       )
      (else
        (setRightChild grandparent child)
        )
      )
    )

  (define (maxHeight a b)
    (if (> a b)
      a
      b
      )
    )

  (define (sibling node)
    (if (eq? node ((node 'parent) 'left))
      ((node 'parent) 'right)
      ((node 'parent) 'left)
      )
    )

  (define (favorite parent)
    (define balFact (balanceFactor parent))
    (cond
     ((= balFact 1)
      (parent 'left)
      )
     ((= balFact -1)
      (parent 'right)
      )
     (else
       nil
       )
     )
    )

  (define (balanceFactor node)
    (define leftHeight 0)
    (define rightHeight 0)
    (cond 
      ((not (eq? (node 'right) nil))
       (set! rightHeight ((node 'right) 'height))
       )
      )
    (cond
      ((not (eq? (node 'left) nil))
       (set! leftHeight ((node 'left) 'height))
       )
      )
    (- rightHeight leftHeight)
    )

  (define (isBalanced node)
    (define balFact (balanceFactor node))
    (and (< balFact 2) (> balFact -2))
    )

  (define (isLeftChild node)
    (eq? node ((node 'parent) 'left))
    )

  (define (isRightChild node)
    (eq? node ((node 'parent) 'right))
    )

  (define (setLeftChild parent child)
    (set 'left child parent)
    (cond
      ((not (eq? child nil))
       (set 'parent parent child)
       )
      )

    ;(if (not (eq? child nil))
    ;  (set 'parent parent child)
    ;  )
    )

  (define (setRightChild parent child)
    (set 'right child parent)
    (cond
      ((not (eq? child nil))
       (set 'parent parent child)
       )
      )
    ;(if (not (eq? child nil))
    ;  (set 'parent parent child)
    ;  )
    )

  (define (isLinear parent child)
    (if (isLeftChild parent)
      (isLeftChild child)
      (isRightChild child)
      )
    )

  (define (isLeaf node)
    (and (null? (node 'left)) (null? (node 'right)))
    )

  (define (statistics)
    (define q (queue))
    ((q 'enqueue) root)
    (while (> ((q 'size)) 0)
           (define current ((q 'dequeue)))
           (print (current 'val))
           (print ":")
           (print (* -1 (balanceFactor current)))
           (print " ")
           (cond 
             ((not (eq? (current 'left) nil))
              ((q 'enqueue) (current 'left))
              )
             )
           (cond
             ((not (eq? (current 'right) nil))
              ((q 'enqueue) (current 'right))
              )
             )
           )
    (println)
    )

  this
  )

(define (run3)
  (define t (avl))
  ((t 'insert) 3) 
  ((t 'insert) 4)
  ((t 'insert) 5)
  ((t 'insert) 1)
  ((t 'insert) 0)
  ((t 'statistics))
  (inspect ((t 'find) 7))
  ;(inspect ((t 'size)))
  )
;(run3)
(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) #t)
        (else (memq item (cdr x)))))
(define (percentError a b)
  (abs (/ (- b a) a))
  )

(define (make-connector)
  (define percentErrorMargin 0.00001)
  (let ((value #f) (informant #f) (constraints (list)))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((> (percentError value newval) percentErrorMargin)
             (print "Contradiction")
             (println (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (cond
        ((eq? retractor informant)
        (begin (set! informant #f)
               (for-each-except retractor
                                inform-about-no-value
                                constraints)))
        (else
        'ignored)))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints
          (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if (not (eq? informant #f)) #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (print "Unknown operation -- CONNECTOR"
                           (println request)))))
    me))
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))(set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (print "Unknown request -- MULTIPLIER")
            (println request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)
(define (constant value connector)
  (define (me request)
    (print "Unknown request -- CONSTANT")
    (println request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (squarer a b) 
   (define (process-new-value) 
     (if (has-value? b) 
         (if (< (get-value b) 0) 
             (error "square less than 0 -- SQUARER" (get-value b)) 
             (set-value! a 
                         (sqrt (get-value b)) 
                         me)) 
         (if (has-value? a) 
             (set-value! b 
                         (square (get-value a)) 
                         me)))) 
   (define (process-forget-value) 
     (forget-value! a me) 
     (forget-value! b me)) 
   (define (me request) 
     (cond ((eq? request 'I-have-a-value) 
            (process-new-value)) 
           ((eq? request 'I-lost-my-value) 
            (process-forget-value)) 
           (else 
            (error "Unknown request -- SQUARER" request)))) 
   (connect a me) 
   (connect b me) 
   me) 
        

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (gravity f m1 m2 r)
  (define G 0.00667300)
  (let ((w (make-connector))
        (x (make-connector))
        (y (make-connector))
        (z (make-connector)))
        
    (constant G w)
    (squarer r z)
    (multiplier m1 m2 x)
    (multiplier w x y)
    (multiplier f z y)
    (multiplier r r z)
    'ok))

(define (run4)
  (define f (make-connector))
  (define m1 (make-connector))
  (define m2 (make-connector))
  (define r (make-connector))
  (gravity f m1 m2 r)
  (set-value! m1 5 this)
  (set-value! m2 10 this)
  ;(set-value! r 2 this)
  ;(forget-value! r this)
  (set-value! f 1 this)
  (inspect (get-value r))
  )
;(run4)

(define (barrier)
  (define numThreads 0)
  (define threads (list))
  (define (set newNumThreads)
    (set! numThreads newNumThreads)
    )

  (define (install)
    (tjoin (thread 
      (begin
        (while #t
               (println (gettid))
               (define tid (gettid))
               (cond 
                 ((not (member? tid))
                  (cons tid threads)
                  (println tid)
                  )
                 )
           )
        )
      ))
    )

  (define (remove)
    )

  this
  )

(define (print1)
  (while #t
         (print "1")
         (sleep 1)
         (print "1")
         )
  )

(define (run5)
  (define b (barrier))
  ;((b 'set 1))
  ((b 'install))
  (define t1 (thread (begin (lock) (println "Hello World 1") (unlock))))
  (define t2 (thread (begin (lock) (println "Hello World 2") (unlock))))
  (define t3 (thread (begin (lock) (println "Hello World 3") (unlock))))
  (tjoin t1)
  (tjoin t2)
  (tjoin t3)
  )
;(run5)

(define (stream-display s n)
  (define (display-line x)
    (newline)
    (display x)
    )
  (define (stream-for-each proc s n)
    (if (= n 1)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s) (- n 1))
        )
      )
    )
  (define (svdisplay s n)
    (cond
      ((= n 1)
       (print (stream-car s))
       )
      ((> n 1)
       (print (stream-car s) ",") 
       (svdisplay (stream-cdr s) (- n 1))
       )
      (else
        nil
        )
      )
    )

  (print "[")
  ;(stream-for-each display s n)
  (svdisplay s n)
  (println "...]")
  )

(define (sop op s t)
  (cons-stream (op (stream-car s) (stream-car t)) (sop op (stream-cdr s) (stream-cdr t)))
  )

(define (smap f s)
  (cons-stream (f (stream-car s)) (smap f (stream-cdr s)))
  )


(define (big-gulp)
  (define (sop op s t)
    (cons-stream (op (stream-car s) (stream-car t)) (sop op (stream-cdr s) (stream-cdr t)))
    )
  (define downCount 1)
  (define maxDownCount 1)

  (define upCount 0)
  (define maxUpCount 1)

  (define (nextDownCount)
    (define curDownCount downCount)
    (cond
      ((= downCount 0)
       (set! maxDownCount (+ maxDownCount 1))
       (set! downCount maxDownCount)
       )
      (else
        (set! downCount (- downCount 1))
        )
      )
    curDownCount
    )

  (define (nextUpCount)
    (define curUpCount upCount)
    (cond
      ((= upCount maxUpCount)
       (set! maxUpCount (+ maxUpCount 1))
       (set! upCount 0)
       )
      (else
        (set! upCount (+ upCount 1))
        )
      )
    curUpCount
    )

  (define (downCounter) (cons-stream (nextDownCount) (downCounter)))
  (define (upCounter) (cons-stream (nextUpCount) (upCounter)))
  (define bg (sop (lambda (x y) (* (^ 7 x) (^ 11 y))) (downCounter) (upCounter)))
  bg
  )

(define (run6)
  ;(inspect (stream-car (stream-cdr bgs)))
  ;(stream-display big-gulp 4)
  (define bg (big-gulp))
  (stream-display bg 4)
  (stream-display bg 4)
  )

;(run6)

(define (delay # $x)
  (cons $x #)
  )

(define (force x)
  (eval (car x) (cdr x))
  )

(define (signal f x dx)
  (cons-stream (f x) (signal f (+ x dx) dx))
  )

(define (scale-stream s factor)
  (smap (lambda (x) (* x factor)) s)
  )

(define (stream-cadr s)
  (stream-car (stream-cdr s))
  )

;(define (sSkip s n)ddd
;  (define (iter cur newS)
;    (cond
;      ((= cur n)
;       (cons-stream (stream-car s) 
;  (cons-stream (stream-car s) 

(define (sBinaryMap f s)
  (cons-stream (f (stream-car s) (stream-cadr s)) (sBinaryMap f (stream-cdr s)))
  )
(define (sAccumulate s prevVal)
  (define nextVal (+ (stream-car s) prevVal))
  (cons-stream nextVal (sAccumulate (stream-cdr s) nextVal))
  )
(define (min a b)
  (if (< a b)
    a
    b
    )
  )
(define (max a b)
  (if (> a b)
    a
    b
    )
  )
(define (trapezoid a b base)
  (define maxVal (max a b))
  (define minVal (min a b))
  (define tHeight (- maxVal minVal))
  (define result (+ (* .5 base tHeight) (* base minVal)))
  result
  )

(define (inverseTrapezoid a b base)
  (define maxVal (max a b))
  (define minVal (min a b))
  (+ (* 2 (* base (- maxVal minVal))) minVal)
  )

(define (integral s dx)
  (sAccumulate 
    (cons-stream 0 (sBinaryMap (lambda (a b) (trapezoid a b dx)) s)) 0)
  )

(define (differential start s dx)
  (define (iter prevResult s)
    (define result (inverseTrapezoid prevResult (- (stream-cadr s) (stream-car s)) dx))
    (cons-stream result (iter result (stream-cdr s)))
    )

  (cons-stream start (iter start s))
  )

(define (run7)
  (define (f x) (- (+ (^ x 2) (* 3 x)) 4))
  (define dx 1)
  (define printNum 10)
  (define poly (signal f 0 dx))
  (stream-display poly printNum)
  (define intPoly (integral poly dx))
  (stream-display intPoly printNum)
  (define divIntPoly (differential (stream-car poly) intPoly dx))
  (stream-display divIntPoly printNum)
  (define difference (sop - divIntPoly poly))
  (stream-display difference printNum)
  )
;(run7)


