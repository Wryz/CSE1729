#lang racket
;;1
(define (zip lsta lstb)
  (if (or (null? lsta)
          (null? lstb))
      '()
      (cons (cons (car lsta)
                  (car lstb))
            (zip (cdr lsta)
                 (cdr lstb)))))
;;2
(define (unzip plist)
   (if (null? plist)
        '(())
        (let* ((rest (unzip (cdr plist)))
                 (first (car rest))
                 (second (cdr rest)))
           (cons (cons (caar plist) first)
                  (cons (cdar plist) second)))))
;;3
(define ( change n denominations)
   ( cond ((< n 0) 0)
          ((= n 0) 1)
          ((null? denominations) 0)
           (else (+ (change n (cdr denominations))
                    (change (- n (car denominations))
                           denominations)))))
;;4
(define (helper n cur)
  (cond ((null? cur) '())
        (else (cons (append (car cur) (list n))
                    (helper n (cdr cur))))))
(define (make-change n den)
  (cond ((< n 0) '())
        ((= n 0) '(()))
        ((null? den) '())
        (else
         (append (make-change n (cdr den))
                 (helper (car den)
                         (make-change (- n (car den)) den))))))
;;5
(define (rle coins)
  (define (rlehelper coins prev count)
    (cond ((null? coins) (list (cons count prev)))
          ((= (car coins) prev) (rlehelper (cdr coins) prev (+ count 1)))
          (else (cons (cons count prev) (rlehelper (cdr coins) (car coins) 1)))))
  (rlehelper coins (car coins) 0))
;6
(define (rle-all xcoins)
  (define (rle-all-helper xcoins)
    (if (null? xcoins)
        '()
        (cons (rle (car xcoins)) (rle-all-helper (cdr xcoins)))))
  (rle-all-helper xcoins))
;;7a
(define (list-sum  elements)
  (if (null? elements)
      0
      (+ (car  elements)
         (list-sum (cdr  elements )))))
;;7b
(define (average X)
  (/ (list-sum X)
     (length X)))
;;7c
(define (var-map X)
  (define (square x) (* x x))
  (let ((mean (average X)))
    (map (lambda (x) (square  (- x mean ))) X)))
;;7d
(define (stdev X)
  (sqrt (average (var-map X))))
;;7e
(define (map2 f X Y)
  (if (null? X)
      '()
      (cons (f (car X) (car Y))
            (map2 f (cdr X) (cdr Y)))))
;;7f
(define (covar-elements X Y)
  (let (( meanX (average X))
        (meanY (average Y)))
    (map2 (lambda (x y)
            (* (- x meanX)
              (- y meanY )))
          X Y)))
;;7g
(define (pearson X Y)
  (/ (average (covar-elements X Y))
     (* (stdev X) (stdev Y))))
;;8a
(define (best-fit  pX pY)
  (let* ((a (* (pearson  pX pY) (stdev pY) (/ 1 (stdev pX))))
         (b (- (average  pY) (* a (average  pX )))))
    (cons a b)))
;;8b
(define (best-fit-fn pX pY)
  (let* ((a (* (pearson  pX pY) (stdev pY) (/ 1 (stdev pX))))
         (b (- (average  pY) (* a (average  pX )))))
    (lambda (x) (+ (* a x) b))))
(define (fitline X Y)
  (best-fit-fn X Y))