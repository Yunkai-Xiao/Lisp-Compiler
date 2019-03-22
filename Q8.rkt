#lang racket
(define psymboltable
  (make-hash))
(define (first-phase inst row)
   
  (cond
    [(empty? inst) empty]
    [(symbol=? (first (first inst)) 'label)
     (hash-set! psymboltable (second (first inst)) row)
     (first-phase (rest inst) (add1 row))]
    [(symbol=? (first (first inst)) 'const)
     (hash-set! psymboltable (second (first inst)) (third (first inst)))
     (first-phase (rest inst) (add1 row))]
    [(symbol=? (first (first inst)) 'data)
     (hash-set! psymboltable (second (first inst)) row)
     (cond
       [(list? (third (first inst)))
        (set-data-abbr row (first (third (first inst)))
                       (second (third (first inst))))]
       [else     
        (set-data row (rest (rest (first inst))))])
     (first-phase (rest inst) (add1 row))]))

(define (set-data row datalst)
  (cond
    [(empty? datalst) empty]
    [else
     (hash-set! psymboltable row (first datalst))
     (set-data (add1 row) (rest datalst))]))

(define (set-data-abbr row times data)
  (cond
    [(zero? times) empty]
    [else
     (hash-set! psymboltable row data)
     (set-data-abbr (add1 row) (sub1 times) data)]))
;; Tests for first phase here
(first-phase '((label NAME)
               (const NAME1 13)
               (data X Y)
               (data Y (10 1))) 0)

psymboltable