
;;1
(define (merge list1 list2)
  (if (null? list1) list2
      (if (null? list2) list1
          (if (< (car list1) (car list2))
              (cons (car list1) (merge (cdr list1) list2))
              (cons (car list2) (merge (cdr list2) list1))))))

;;2
(define (mergesort lst)
  (define (odd lst)
    (if (null? lst) '()
        (if (null? (cdr lst)) (list (car lst))
            (cons (car lst) (odd (cddr lst))))))
  (define (even lst)
    (if (null? lst) '()
        (if (null? (cdr lst)) '()
            (cons (cadr lst) (even (cddr lst))))))
  (define (split lst)
    (cons (odd lst) (cons (even lst) `())))
  (if (null? lst) lst
      (if (null? (cdr lst)) lst
          (merge(mergesort (car (split lst)))
                (mergesort (cadr (split lst)))))))

;;3
(define (ins x l)
  (cond ((null? l) (cons x l))
        ((<= x (car l)) (cons x l)) 
        (else (cons (car l) (ins x (cdr l))))))

;;4
(define (insSort l)
    (cond ((null? l) l)
          (else (ins (car l) (insSort (cdr l))))))

;;5a
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))
;;5b
(define (fold-left op initial sequence)
  (if (null? sequence)
      initial
      (fold-left op (op initial (car sequence))
                 (cdr sequence))))

;;5c
(define (my-map p sequence)
  (fold-right (lambda (x y) (cons (p x) y)) '() sequence))

;;5d
(define (my-append seq1 seq2)
  (fold-right cons seq2 seq1))

;;5e
(define (my-length sequence)
  (fold-right (lambda (x y) (+ 1 y)) 0 sequence))
;;5f
(define (reverse-r sequence)
  (fold-right (lambda (x y) (my-append y (list x))) '() sequence))

;;5g
(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;;5h
(define (horner-eval x coefficient-list)
  (fold-right (lambda (z y) (+ z (* y x))) 0 coefficient-list))

;;6ai
(define (count-digits n acc)
  (if (< n 10)
    (+ acc 1)
    (count-digits (/ n 10) (+ acc 1))))

(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k)
    (and (> k 1)
         (or (divisor? k) (divisors-upto  (- k 1)))))
  (not (divisors-upto  (- n 1))))

(define (left-truncatable-prime? p)
  (if (= p 1) #f
      (if (prime? p)
          (cond ((< p 10) #t)
            (else (left-truncatable-prime? (modulo p (expt 10 (- (count-digits p 0) 1))))))
          #f)))
;;6aii
(define (find sequence test n)
  (define (find-aux x found)
    (let ((fx (sequence x)))
      (if (test fx)
          (if (= (+ found 1) n)
              fx
              (find-aux (+ x 1) (+ found 1)))
          (find-aux (+ x 1) found))))
  (find-aux 1 0))

(define (nth-left-trunc-prime n)
  (find (lambda (x) x) left-truncatable-prime? n))
;;6bi
(define (right-truncatable-prime? p)
  (if (= p 1) #f
      (if (prime? p)
          (cond ((< p 10) #t)
            (else (right-truncatable-prime? (floor (/ p (expt 10 (- (count-digits p 0) 1)))))))
          #f)))
;;6bii
(define (nth-right-trunc-prime n)
  (find (lambda (x) x) right-truncatable-prime? n))

;;6ci
(define (two-sided-prime? p)
  (and (right-truncatable-prime? p) (left-truncatable-prime? p)))

;;6cii
(define (nth-two-sided-prime n)
  (find (lambda (x) x) two-sided-prime? n))