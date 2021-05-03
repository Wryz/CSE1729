#lang racket

;HEAPS

(define (create-heap v H1 H2)
  (list v H1 H2))
(define (h-min H) (car H))
(define (left H) (cadr H))
(define (right H) (caddr H))

(define (insert x H)
  (if (null? H)
      (create-heap x '() '())
      (let ((child-value (max x (h-min H)))
            (root-value (min x (h-min H))))
        (create-heap root-value (right H)
                     (insert child-value (left H))))))

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

(define (heap-insert-list f elements H)
  (if (null? elements)
      H
      (heap-insert-list f (cdr elements)
                        (heap-insert f (car elements) H))))

(define (remove-minimum H)
  (combine-heaps (left H) (right H)))

(define (heap-extract-sorted H)
  (if (null? H)
      (list)
      (cons (h-min H) (heap-extract-sorted (remove-minimum H)))))

(define (hsort elements)
  (heap-extract-sorted (heap-insert-list < elements '())))