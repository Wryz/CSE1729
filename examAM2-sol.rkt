#lang racket
;1. MapReduce is a programming model for parallel processing of large data sets. The model is composed of a
;Map procedure and a Reduce procedure and was heavily inspired by the map and reduce (you know reduce by
;its other name, fold) operations in functional programming. MapReduce was originally a term for the Google
;proprietary implementation, but has come to refer to any implementation of the method. Unfortunately, other
;models have become more popular for parallel processing of large data sets in recent years.
;
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

(define (sublist last input ignored sorted)
  (cond ((null? input) (cons ignored sorted))
        ((> (car input) last)
         (sublist (car input) (cdr input) ignored (append sorted (list (car input)))))
        (else (sublist last (cdr input) (append ignored (list (car input))) sorted))))

(define (merge l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
        (else (cons (car l2) (merge l1 (cdr l2))))))

(define (strand-aux unsorted sorted)
  (cond ((null? unsorted) sorted)
        (else (let* ((list-pair (sublist (car unsorted) (cdr unsorted) (list) (list (car unsorted))))
                     (partial (cdr list-pair))
                     (rest (car list-pair)))
                (strand-aux rest (merge partial sorted))))))

(define (strand-sort lst)
  (strand-aux lst (list)))

(define (make-tree value left right)
  (list value left right))
(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))


(define (bst-min T)
  (if (null? (left T))
      (value T)
      (bst-min (left T))))

(define (bst-max T)
  (if (null? (right T))
      (value T)
      (bst-max (right T))))

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