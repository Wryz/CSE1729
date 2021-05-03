#lang racket

[4 points] In order to determine if an integer is narcissistic, we must know the number of digits in the
integer. Define a tail recursive SCHEME function, named (num-digits n), which accepts an integer, n,
as an argument and computes the number of digits in the integer. Note: you did this in Lab 2. However,
this version must be tail recursive.

(define (num-digits n)
  (define (num-digits-aux n acc)
    (if (< n 10)
      (+ acc 1)
      (num-digits-aux (floor (/ n 10)) (+ acc 1))))
  (num-digits-aux n 0))


) [6 points] Define a tail recursive SCHEME function, named (narcissistic? n), which evaluates to
#t if n is a narcissistic number, and #f otherwise. That is, your function should return #t when the
sum of all the digits, raised to the power equal to the number of digits in n, is equal to n. Be sure your
solution is tail recursive.

(define (narcissistic? n)
  (let ((p (num-digits n)))
  (define (narc-aux n acc)
    (if (< n 10)
        (+ (expt n p) acc)
        (narc-aux (floor (/ n 10)) (+ acc (expt (modulo n 10) p)))))
  (= n (narc-aux n 0))))


3-WAY QUICKSORT
The Quicksort sorting algorithm discussed in lecture divided an unsorted input list into two smaller lists, one
of values smaller than a given pivot value and the other of values greater than or equal to the pivot value.
These two lists are recursively sorted using Quicksort. Then, a list of sorted values can be built from the sorted
list of values less than the pivot, the pivot value, and the sorted list of values greater than or equal to the pivot.
For lists which contain many duplicates, we may improve on the standard Quicksort algorithm by partitioning
the list into three smaller lists, one list of values smaller than the pivot, one list of values equal to the pivot,
and one list of values greater than the pivot. The first and last lists can then be recursively sorted using our
modified Quicksort. Every element of the middle list is the same value. So, the middle list is sorted. Once
they are all sorted, these three lists can be joined to form one full sorted list.
(a) [5 points] Define a SCHEME function, named (3-way-partition p lst smaller equal greater),
which partitions the list, lst, into a list of elements less than the pivot value, p, named smaller, a list
of elements equal to the pivot named equal, and a list of elements greater than the pivot, greater.

(define (3-way-partition p lst smaller equal greater)
  (cond ((null? lst) (list smaller equal greater))
        ((< (car lst) p) (3-way-partition p
                                          (cdr lst)
                                          (cons (car lst) smaller)
                                          equal
                                          greater))
        ((= (car lst) p) (3-way-partition p
                                          (cdr lst)
                                          smaller
                                          (cons (car lst) equal)
                                          greater))
        (else (3-way-partition p
                               (cdr lst)
                               smaller
                               equal
                               (cons (car lst) greater)))))


[5 points] Define a SCHEME function, named (3-way-quicksort lst), which sorts the input list,
lst, by partitioning the list into three lists using 3-way-partition. Recursively sorting the unsorted
partitions and joining the components into one sorted list.

(define (3-way-quicksort lst)
  (cond ((null? lst) lst)
        ((null? (cdr lst)) lst)
        (else (let* ((parts (3-way-partition (car lst)
                                             (cdr lst)
                                             (list)
                                             (list)
                                             (list)))
                    (first (3-way-quicksort (car parts)))
                    (last (3-way-quicksort (caddr parts))))
                (append first (cons (car lst) (cadr parts)) last)))))


[10 points] BINARY SEARCH TREES
Define a SCHEME function, named (bst-range T min max), which, when given a Binary Search Tree
(BST), T, evaluates to a BST consisting of all the elements of T which are ≤ max and ≥ min. Your function
must traverse the BST removing any subtrees containing values outside the desired range. To do this, you
must rely on the BST property.

(define (make-tree v left-tree right-tree)
   (list v left-tree right-tree))

(define (value T) (car T))
(define (left T)  (cadr T))
(define (right T) (caddr T))

(define (bst-range T min max)
  (cond ((null? T) T)
        ((< (value T) min) (bst-range (right T) min max))
        ((> (value T) max) (bst-range (left T) min max))
        (else (make-tree (value T)
                         (bst-range (left T) min max)
                         (bst-range (right T) min max)))))


[10 points] HEAPS
The heap insert function discussed in lecture used an alternating subtree heuristic, in which subsequent inserts
into the heap were alternated into the left and right subtrees, in an attempt to create a balanced tree. This
approach works well if few removals from the heap are processed. Alternatively, we may check the size of
each of the left and right subtrees and insert new values into the smaller of the two. In this problem, you
will write a SCHEME function which inserts new values into a min heap data structure using the size of the
subtrees to determine which subtree should receive the new value.
You may start with the function that returns the number of nodes in a binary tree.
(define (tree-size T)
(if (null? T) 0
(+ 1 (tree-size (left T)) (tree-size (right T)))))
Define a SCHEME function, named (heap-insert x H), which inserts value x into heap H and always inserts
new values into the smaller subtree in the heap. Ties may be broken arbitrarily.


(define (create-heap v H1 H2)
  (list v H1 H2))

(define (h-min H) (car H))

(define (tree-size T)
  (if (null? T) 0
      (+ 1 (tree-size (left T)) (tree-size (right T)))))

(define (heap-insert x H)
  (if (null? H)
      (create-heap x (list) (list))
      (let ((root-value (min x (h-min H)))
            (child-value (max x (h-min H))))
        (if (< (tree-size (left H)) (tree-size (right H)))
            (create-heap root-value (heap-insert child-value (left H)) (right H))
            (create-heap root-value (left H) (heap-insert child-value (right H)))))))
