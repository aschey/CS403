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

(run1)

(define (run2)
  (define (fib n)
    (cond
      ((< n 2) n)
      (else (+ (fib (- n 1)) (fib (- n 2))))
      )
    )
  )

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
