#lang racket
(define (num-digits n)
  (define (num-digits-aux n acc)
    (if (< n 10)
      (+ acc 1)
      (num-digits-aux (floor (/ n 10)) (+ acc 1))))
  (num-digits-aux n 0))

(define (narcissistic? n)
  (let ((p (num-digits n)))
  (define (narc-aux n acc)
    (if (< n 10)
        (+ (expt n p) acc)
        (narc-aux (floor (/ n 10)) (+ acc (expt (modulo n 10) p)))))
  (= n (narc-aux n 0))))

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
