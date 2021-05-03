#lang racket
;1. MapReduce is a programming model for parallel processing of large data sets. The model is composed of a
;Map procedure and a Reduce procedure and was heavily inspired by the map and reduce (you know reduce by
;its other name, fold) operations in functional programming. MapReduce was originally a term for the Google
;proprietary implementation, but has come to refer to any implementation of the method. Unfortunately, other
;models have become more popular for parallel processing of large data sets in recent years.

;(a) [4 points] Define a SCHEME function, named (map-reduce-l map reduce initial lst), which
;performs fold left on the values obtained by applying the function map to each element of the list lst
;using the function reduce to combine these results. Your function should only access the elements of
;the list once and, therefore, should perform the map and reduce operations within the same function
(define (map-reduce-l map reduce initial lst)
  (if (null? lst)
      initial
      (map-reduce-l map reduce (reduce initial (map (car lst))) (cdr lst))))

;(b) [4 points] Define a SCHEME function, named (map-reduce-r map reduce initial lst), which
;performs fold right on the values obtained by applying map to each element of the list lst using the
;function reduce to combine these results. Your function should only access each element of the list once
;and, therefore, should perform the map and reduce operations within the same function.
(define (map-reduce-r map reduce initial lst)
  (if (null? lst)
      initial
      (reduce (map (car lst)) (map-reduce-r map reduce initial (cdr lst)))))


STRAND SORT
Strand sort is a sorting algorithm which is similar in some ways to Merge Sort. Strand sort accepts a list of
elements in any order, let’s call it unsorted, and produces a sorted list, we’ll call sorted, by the following
procedure:
1. Let unsorted be the input list and sorted be the output list.
2. Create an empty templist and move first item of unsorted to it.
3. Traverse remaining items of unsorted. For every item x, check if x is greater than last inserted item to
templist.
(a) If yes, remove x from unsorted and add at the end of templist.
(b) If not, ignore x (Keep it in unsorted).
4. Merge templist into sorted, the output list.
5. Repeat for remaining items in unsorted and current items in sorted.
(a) [5 points] Define a SCHEME function, named (sublist last input ignored sorted), where
last is the last number added to the sorted list, input is a list of elements that have not been processed,
ignored is a list containing elements which were skipped, and sorted is a list of elements which were
removed from input and placed at the end of sorted to maintain a sorted order. sublist makes one
pass through the input list. Every time it encounters a value in input which is larger than the last item
added to sorted, it adds that element to the end of sorted and sets the last argument to this new
element added to sorted. For example, given
15, (7 8 42 10 9 99 1), '(), and '(15) as arguments
sublist would skip over 7 and 8, placing them at the end of the ignored list. Upon reaching 42,
sublist must move 42 to the sorted list (i.e. the templist) and 42 becomes the new last. So, sublist
is called again with
42, (10 9 99 1), '(7 8), and '(15 42) as arguments
Again, it skips over 10 and 9, adding them to the ignored list. Upon reaching 99 it adds 99 to the
sorted list and 99 becomes the new last and sublist is called again with
99, (1), '(7 8 10 9), and '(15 42 99) as arguments. Since, the remaining numbers in the list are
less than 99, this pass produces the ignored list (7 8 10 9 1) and the sorted sublist (15 42 99) in a
SCHEME pair. The sorted list can be merged into the solution computed previously. Repeated applications
of sublist on the ignored list will remove more elements from the input list and produce sorted sublists
which can be merged into the solution until all elements have been merged into the solution.
To merge the sorted templist into the output list, use this SCHEME function, named (merge l1 l2)
which accepts two sorted lists as arguments and evaluates to a sorted list containing all elements of l1
and l2. You may recognize this function from Mergesort.

(define (merge l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
        (else (cons (car l2) (merge l1 (cdr l2))))))
 
[2 points] Are either of these functions tail recursive? If so, which function(s) is(are) tail recursive and
how can you tell?
(define (sublist last input ignored sorted)
  (cond ((null? input) (cons ignored sorted))
        ((> (car input) last)
         (sublist (car input) (cdr input) ignored (append sorted (list (car input)))))
        (else (sublist last (cdr input) (append ignored (list (car input))) sorted))))


[5 points] Define a SCHEME function, named (strand-aux unsorted sorted) which accepts two
lists, a sorted list denoted by sorted and a list of remaining elements to be sorted named unsorted.
strand-aux should use sublist to obtain a sorted sublist, merge the sorted sublist into the sorted
list and recursively apply strand-aux to the ignored list obtained from sublist until all elements are
present in the sorted list and in the correct order.


(define (strand-aux unsorted sorted)
  (cond ((null? unsorted) sorted)
        (else (let* ((list-pair (sublist (car unsorted) (cdr unsorted) (list) (list (car unsorted))))
                     (partial (cdr list-pair))
                     (rest (car list-pair)))
                (strand-aux rest (merge partial sorted))))))

(define (strand-sort lst)
  (strand-aux lst (list)))


BINARY SEARCH TREES
This question is concerned with the ceiling of a value in a binary search tree (BST). The ceiling of a value,
v, in a binary search tree is defined as the smallest value in the binary search tree which is greater than v.
For this question, you may presume there exists at least one value in the BST which is greater than v. Note,
however, the value v may not be stored in the BST.

(define (make-tree value left right)
  (list value left right))
(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))


(a) [1 point] Define a SCHEME function, named (bst-min T), which, given a BST T, returns the smallest
value in T.

(define (bst-min T)
  (if (null? (left T))
      (value T)
      (bst-min (left T))))


[1 point] Define a SCHEME function, named (bst-max T), which, given a BST T, returns the largest
value in T.

(define (bst-max T)
  (if (null? (right T))
      (value T)
      (bst-max (right T))))


[8 points] Define a SCHEME function, named (bst-ceil v T), which, given a value v and a BST T,
returns the smallest value in T which is greater than v. Again, you may presume there is at least one
value in T which is greater than v.
Hint: If v does not appear in the BST, this can be detected if either (value T) < v < (bst-min (right T))
or (bst-max (left T)) < v < (value T).

(define (bst-ceil v T)
  ;;This version returns -1 if there is no element smaller than v
  ;;Prelim 2 question allows one to presume there is at least on element less than v
  (cond ((null? T) -1)
        ((= v (value T)) (if (null? (right T)) -1 (bst-min (right T))))
        ((and (not (null? (right T)))
              (< (value T) v)
              (> (bst-min (right T)) v)) (bst-min (right T)))
        ((and (not (null? (left T)))
              (> (value T) v)
              (<= (bst-max (left T)) v)) (value T))
        ((< v (value T)) (if (null? (left T)) (value T) (bst-ceil v (left T))))
        (else (bst-ceil v (right T)))))


[10 points] HEAPS
The heap data structure, as described in lecture, accommodates duplicate values without modification. In
your coursework, you considered the problem of obtaining a sorted list by using the heap data structure. For
instance, if we insert the following list of values into a heap,
'(5 4 3 5 6 7 23 5 1 2 5 7 34 5 2)
we can extract a sorted list by using the extract-sorted you wrote in Problem Set 8.
'(1 2 2 3 4 5 5 5 5 5 6 7 7 23 34)
However, if the data structure contains many duplicates, this may be difficult to read. It is also the case
that another representation may provide a more compact way to store this sorted sequence of values. One
representation is the run length encoding we used to represent the ways to make change in Problem Set 6.
By using a run length encoding, the sequences of repeated values are collapsed into a pair where the first
component of the pair indicates the number of occurrences of a particular value and the second component
indicates the value that is repeated in the sequence. For example, if we were able to extract the values in the
list
'(5 4 3 5 6 7 23 5 1 2 5 7 34 5 2)
in sorted order, expressed as a run length encoding. This would produce the following list of pairs
'((1 . 1) (2 . 2) (1 . 3) (1 . 4) (5 . 5) (1 . 6) (2 . 7) (1 . 23) (34 . 1))
Define a SCHEME function, named extract-sorted-rle, which extracts the elements stored in a heap, in
sorted order, and represented as a run length encoding. Your function should build just one list in run length
encoding form. For full credit, your function must not simply rely on the existing extract-sorted function
and convert the sorted list into a run length encoding.

(define (create-heap v H1 H2)
  (list v H1 H2))

(define (h-min H) (car H))

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

(define (extract-sorted-rle H)
  (define (extract-aux H v num)
    (if (null? H)
        (list (cons num v))
        (if (= v (h-min H))
            (extract-aux (remove-minimum H) v (+ num 1))
            (cons (cons num v)
                  (extract-aux (remove-minimum H) (h-min H) 1)))))
  (extract-aux (remove-minimum H) (h-min H) 1))
