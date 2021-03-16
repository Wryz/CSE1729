;;1a
(define (list-at l i)
  (if (equal? i 0)
      (car l)
      (list-at (cdr l) (- i 1))))

(define (selSort l)
  (define (smallest l)
    (define (smaller a b) (if (< a b) a b))
    (if (null? (cdr l))
        (car l)
        (smaller (car l) (smallest (cdr l)))))
  (define (remove v l)
    (if (null? l)
        l
        (if (equal? v (car l))
            (cdr l)
            (cons (car l) (remove v (cdr l))))))
  (if (null? l)
      '()
      (let* ((first (smallest l))
             (rest (remove first l)))
        (cons first (selSort rest)))))

;;1b
(define (list-median l)
  (define (insertionsort s)
    (define (insert a s)
      (cond ((null? s) (list a))
            ((> (car s) a) (cons a s))
            (else (cons (car s) ( insert a (cdr s))))))
    (define (inserthelp in out)(if ( null? in)
                                    out
                                    (inserthelp (cdr in) (insert( car in )out ))))
    (inserthelp s (list)))
  (let (( sorted (insertionsort l)))
    (if (= (modulo (length l) 2) 0)
        (* (+ (list-at sorted (floor (/ (length l) 2))) (list-at sorted (- (floor (/ (length l) 2)) 1))) (/ 1 2))
        (list-at sorted (floor (/ (length l) 2))))))

(define (insert v l)
  (cond ((null? l) (list v))
        ((< v (car l)) (cons v l))
        (else (cons (car l) (insert v (cdr l))))))

(define (insertion-sort l)
  (define (insertion-help x d)
    (if (null? x)
        d
        (insertion-help (cdr x) (insert (car x) d))))
  (insertion-help l (list)))

;;2a
(define (explode x)
  (if (< x 10)
      (list x)
      (append (explode (floor (/ x 10)))
              (list (- x (* 10 (floor (/ x 10))))))))

;;2b
(define (implode l)
  (define (add-digits l place)
    (if (null? l)
        0
        (+ (* (car l) (expt 10 place))
           (add-digits (cdr l) (+ place 1)))))
  (add-digits (reverse l) 0))

;;2c
(define (sum-list list)
  (if (null? list)
      0
      (+ (car list)
         (sum-list (cdr list)))))

(define (has-property x)
  (let* ((int (explode x))
         (sum-digits (sum-list int))
         (sum (explode sum-digits))
         (sum-rev (implode (reverse sum))))
    (= (* sum-digits sum-rev) x)))

;;2d
(define (find sequence test n)
  (define (find-aux x found)
    (let ((fx (sequence x)))
      (if (test fx)
          (if (= (+ found 1) n)
              fx
              (find-aux (+ x 1) (+ found 1)))
          (find-aux (+ x 1) found))))
  (find-aux 1 0))

(define (fujiwara n)
  (find (lambda (x) x) has-property n))
