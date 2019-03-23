#lang racket
(define storage (make-hash))
;function for data setup when data list in the form of (NAME 1 2 3 4 ..), returning the hashtable
(define (setdata1 htable lst ptr)
  (define (setdata1-help htable lst ptr)
    (cond
      [(empty? lst) htable]
      [true (hash-set! htable ptr (car lst))
            (setdata1-help htable (cdr lst) (add1 ptr))]))
  (begin
    (hash-set! htable (car lst) ptr)
    (hash-set! htable ptr (cadr lst))
    (setdata1-help htable (cddr lst) (+ 2 ptr))))

;function for data setup when data list in the form of '(NAME (occurences value)), returning the hashtable.
(define (setdata2 htable lst ptr)
  (define (setdata2-help htable lst ptr)
    (cond
      [(zero? (car lst)) htable]
      [true (hash-set! htable ptr (cadr lst))
            (setdata2-help htable (list (sub1 (car lst)) (cadr lst)) (add1 ptr))]))
  (begin
    (hash-set! htable (car lst) ptr)
    (hash-set! htable ptr (second(cadr lst)))
    (setdata2-help htable (list(sub1(car (second lst)))(cadr (second lst))) (+ 2 ptr))))

(define (addinstruction htable lst ptr)
  (cond
    [(empty? lst) htable]
    [(equal? (car (car lst)) 'const) (hash-set! htable (cadr (car lst)) (caddr (car lst)))
                                     (addinstruction htable (cdr lst) (add1 ptr))]
    [(equal? (car (car lst)) 'label) (hash-set! htable (cadr (car lst)) ptr)
                                     (addinstruction htable (cdr lst) ptr)]
    [(equal? (car (car lst)) 'data) (if (list? (caddr (car lst)))
                                        (addinstruction (setdata2 htable (cdr(car lst)) ptr)
                                                        (cdr lst)
                                                        (+ 1 ptr (car (caddr(car lst)))))
                                        (addinstruction (setdata1 htable (cdr (car lst)) ptr)
                                                        (cdr lst)
                                                        (+ ptr (sub1 (length (car lst))))))]
    [(equal? (car (car lst)) 'halt) (hash-set! htable 0 0)
                                    (addinstruction htable (cdr lst) (add1 ptr))]))

(define a '((label LABELONE) (label  LABELTWO) (label LABELTHREE)(const A B) (const B 3) (const C 5)
                             (data fish grouper bass tarpon tuna) (data people (5 names))))
(define b '((data people (5 names))))
(addinstruction storage a 0)