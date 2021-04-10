#lang racket
(define (create-heap v H1 H2)
  (list v H1 H2))
(define (make-tree value left right)
  (list value left right))
(define (h-min H) (car H))
(define (value tree) (car tree))
(define (left H) (cadr H))
(define (right H) (caddr H))

;1
(define (tree-equal? t1 t2)
  (cond ((and (null? t1) (null? t2)) #t)
        ((or (null? t1) (null? t2)) #f)
        ((equal? (value t1)
                 (value t2)) (and (tree-equal? (left t1) (left t2))
                                  (tree-equal? (right t1) (right t2))))
        (else #f)))

;2
(define (tree-map t f)
  (cond ((null? t) (list))
        (else (make-tree (f (value t))
                         (tree-map (left t) f)
                         (tree-map (right t) f)))))

;3
(define (insert x T)
  (cond ((null? T) (make-tree x '() '()))
        ((eq? x (value T)) T)
        ((< x (value T)) (make-tree (value T)
                                    (insert x (left T)) (right T)))
        (else (make-tree (value T)
                         (left T) (insert x (right T))))))
  
(define (sort-extract T)
  (if (null? T)
      '()
      (append (sort-extract (left T))
              (list (value T))
              (sort-extract (right T)))))
  
(define (insert-list L T)
  (if (null? L) T
      (insert-list (cdr L) 
                   (insert (car L) T))))
(define (tree-sort l)
  (sort-extract (insert-list l '())))


;4
(define (delete-value v T)
  (define (leaf? T) (and (null? (left T))
                         (null? (right T))))
  (define (tm T) (if (null? (right T))
                     (value T)
                     (max (value T)
                          (tm (right T)))))
  (cond ((null? T)  '())
        ((< v (value T)) (make-tree (value T)
                                    (delete-value v (left T)) (right T)))
        ((> v (value T)) (make-tree (value T) (left T)
                                    (delete-value v (right T))))
        ((null? (left T)) (right T))
        (else (make-tree (tm (left T))
                         (delete-value (tm
                                        (left T)) (left T)) (right T)))))

;5
(define (heap-insert f x H)
  (cond ((null? H) (create-heap x '() '()))
        ((f x (h-min H))
         (create-heap x
                    (right H)
                    (heap-insert f (h-min H) (left H))))
        (else
         (create-heap (h-min H)
                    (right H)
                    (heap-insert f x (left H))))))

(define (heap-insert-list f elements H)
  (if (null? elements)
      H
      (heap-insert-list f (cdr elements)
                        (heap-insert f (car elements) H))))

(define (combine-heaps H1 H2)
  (cond ((null? H1) H2)
        ((null? H2) H1)
        ((< (h-min H1) (h-min H2))
         (create-heap (h-min H1)
                      H2
                      (combine-heaps (left H1) (right H1))))
        (else
         (create-heap (h-min H2)
                      H1
                      (combine-heaps (left H2) (right H2))))))

(define (remove-minimum H)
  (combine-heaps (left H) (right H)))

(define (heap-extract-sorted H)
  (if (null? H)
      (list)
      (cons (h-min H) (heap-extract-sorted (remove-minimum H)))))

(define (hsort elements)
  (heap-extract-sorted (heap-insert-list < elements '())))

;6a
;check if heap is empty and return heap pair
;let sizeL be the size of the left side of the tree
;let sizeR be the size of the right side of the tree

;check if size of car of the heap-pair is larger than the size of cdr of the heap pair by more than 2
;;if so, let n be the subtraction between sizeL from sizeR and divide by two, use floor to get an accurate integer
;;insert the minimum values n times to the other heap
;;remove the minimum values n times from the heap

;also check if size of cdr of the heap-pair is larger than the size of car of the heap-pair by more than 2
;;if so, let n be the subtraction between sizeL and divide by two, use floor to get an accurate integer
;;insert the minimum values n times to the other heap
;;remove the minimum values n times from the heap
(define (tree-size T)
  (if (null? T)
      0
      (+ (tree-size (left T)) 1 (tree-size (right T)))))

(define (equalize-heaps heap-pair)
  (let* ((sizeR (tree-size (cdr heap-pair)))
         (sizeL (tree-size (car heap-pair)))
         (diff (abs (- sizeR sizeL)))) ;get the difference between the two sizes
    ;(display (cons sizeR sizeL))
    ;(display heap-pair)
    (cond ((> 2 diff) heap-pair) ;check if size difference is less than 2
          ((> sizeR sizeL) ;if right side is larger than left side
           (equalize-heaps
            (cons (insert (h-min (cdr heap-pair)) (car heap-pair))
                  (remove-minimum (cdr heap-pair)))))
          (else
           (equalize-heaps
            (cons (insert (h-min (car heap-pair)) (cdr heap-pair))
                  (remove-minimum (car heap-pair))))))))

(define (add-number x heap-pair)
  (let* ((sizeR (tree-size (cdr heap-pair)))
         (sizeL (tree-size (car heap-pair))))
    (cond ((> sizeR sizeL) (insert x (car heap-pair)))
          (else (insert x (cdr heap-pair))))))


(add-number 144 '((1 2 () ()) 1 7 () ()))
;(equalize-heaps (cons '(0 (3 (8 () ()) (6 () (19 () ())))(1 (14 () (17 () ())) (2 () (5 () ())))) '(9 (42 () (64 () ())) (15 (99 () ()) (89 () ())))))