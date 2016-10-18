(define empty-board nil)

(define (filter pred l)
  (cond 
    ((null? l) l)
    ((eq? (pred (car l)) #t) (cons (car l) (filter pred (cdr l))))
    (else (filter pred (cdr l)))
    )
  )
  
(define (accumulate op base l)
  (cond 
    ((null? l) base)
    (else (op (car l) (accumulate op base (cdr l))))
    )
  )

(define (enumerate-interval low high)
  (if (= low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))
    )
  )

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))


(define (flatmap f l)
  (accumulate append '() (map f l))
  )

(define (adjoin-position row col positions)
   (append positions (list (make-position row col))))
   
(define (make-position row col)
   (cons row col))

(define (position-row position)
   (car position))

(define (position-col position)
   (cdr position))
   
(define (safe? col positions)
   (let ((kth-queen (list-ref positions (- col 1)))
         (other-queens (filter (lambda (q)
                                 (not (= col (position-col q))))
                               positions)))
   (define (attacks? q1 q2)
     (or (= (position-row q1) (position-row q2))
         (= (abs (- (position-row q1) (position-row q2)))
            (abs (- (position-col q1) (position-col q2))))))

   (define (iter q board)
     (or (null? board)
         (and (not (attacks? q (car board)))
              (iter q (cdr board)))))
   (iter kth-queen other-queens)))

(define (queens board-size)
   (define (queen-cols k) 
     (if (= k 0)
         (list empty-board)
         ;(filter
          ;(lambda (positions) (safe? k positions))
          (flatmap
           (lambda (rest-of-queens)
             (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                  (enumerate-interval 1 board-size)))
           (queen-cols (- k 1)))))
   ;)
   (queen-cols board-size))

(inspect (queens 4))
   
