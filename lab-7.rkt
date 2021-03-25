
(define (make-tree value left right)
  (list value left right))
(define (value T) (car T))
(define (right T) (caddr T))
(define (left T) (cadr T))

(define (tree-size T)
  (if (null? T)
      0
      (+ (tree-size (left T)) 1 (tree-size (right T)))))

(define (tree-depth T)
  (if (or (null? T) (and (null? (left T)) (null? (right T)))) 0
      (let ((d-l (tree-depth (left T)))
            (d-r (tree-depth (right T))))
        (if (> d-l  d-r)
            (+ 1 d-l)
            (+ 1 d-r)))))

(define (count-pred P tree)
  (cond ((null? tree) 0)
        ((P (value tree)) (+ 1 (count-pred P (left tree)) (count-pred P (right tree))))
        (else (+ (count-pred P (left tree)) (count-pred P (right tree))))))

(define (count-one-child  tree)
  (let (( left-child (left  tree))
        (right-child (right  tree )))
    (cond ((and (null? left-child)
                (null? right-child )) 0)
          ((and (not (null? left-child ))
                (not (null? right-child )))
           (+ (count-one-child  left-child)
              (count-one-child  right-child )))
          ((null? left-child)
           (+ 1 (count-one-child  right-child )))
          (else (+ 1 (count-one-child  left-child ))))))

(define (invert-bst T)
  (if (null? T)
      (list)
      (make-tree (value T) (invert-bst (right T))
                 (invert-bst (left T)))))