#lang racket

;SORTING ALGORITHMS AND METHODS


(define (remove v elements)
  (if (null? elements)
      elements
      (if (equal? v (car elements))
          (cdr elements)
          (cons (car elements)
                (remove v (cdr elements))))))

(define (reverse-and-append r-items rest)
  (if (null? r-items)
      rest
      (reverse-and-append (cdr r-items)
                          (cons (car r-items) rest))))

(define (smallest l)
  (define (smaller a b) (if (< a b) a b))
  (if (null? (cdr l))
      (car l)
      (smaller (car l) (smallest (cdr l)))))

(define (selSort l)
  (if (null? l)
      '()
      (let* ((first (smallest l))
             (rest (remove first l)))
        (cons first (selSort rest)))))

(define (make-pair a b) (cons a b))
(define (first p) (car p))
(define (second p) (cdr p))

(define (extractSmallest l)
  (if (null? (cdr l))
      (make-pair (car l) '())
      (let ((p (extractSmallest (cdr l))))
        (if (< (car l)  (first p))
            (make-pair (car l) (cons (first p) (second p)))
            (make-pair (first p) (cons (car l) (second p)))))))

(define (selSort2 l)
  (if (null? l)
      '()
      (let ((p (extractSmallest l)))
        (cons (first p) (selSort (second p))))))

(define (alt-extract elements)
  (define (extract-acc smallest dirty clean)
    (cond ((null? dirty) (make-pair smallest clean))
          ((< smallest (car dirty))
           (extract-acc smallest (cdr dirty) (cons (car dirty) clean)))
          (else (extract-acc (car dirty)
                             (car dirty)
                             (cons smallest clean)))))
  (extract-acc (car elements) (cdr elements) '()))

(define (partition l pivot left right)
  (cond ((null? l) (make-pair left right))
        ((< (car l) pivot)
         (partition (cdr l) pivot
                    (cons (car l) left)
                    right))
        (else
         (partition (cdr l) pivot
                    left
                    (cons (car l) right)))))

(define (qSort l)
  (if (null? l)
      l
      (let* ((pivot (car l))
             (parts (partition (cdr l) pivot '() '()))
             (left (qSort (first parts)))
             (right (qSort (second parts))))
        (append left (cons pivot right)))))