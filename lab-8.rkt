#lang racket
(define (create-heap v H1 H2)
  (list v H1 H2))
(define (heap-root H) (car H))
(define (h-min H) (car H))
(define (left H) (cadr H))
(define (right H) (caddr H))

(define (heap-insert f x H)
  (cond ((null? H) (create-heap x '() '()))
        ((f x (heap-root H))
         (create-heap x (right H)
                      (heap-insert f (heap-root H) (left H))))
        (else (create-heap (heap-root H) (right H)
                           (heap-insert f x (left H))))))
                           
(define (combine f H1 H2)
  (cond ((null? H1) H2)
        ((null? H2) H1)
        ((f (h-min H1) (h-min H2))
         (create-heap (h-min H1)
                      H2
                      (combine f (left H1) (right H1))))
        (else
         (create-heap (h-min H2)
                      H1
                      (combine f (left H2) (right H2))))))

(define (empty? H)
  (if (null? H) #t #f))

(define (heap-remove f H)
  (combine f (left H) (right H)))