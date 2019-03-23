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
    (setdata1-help htable (cddr lst) (+ 1 ptr))))

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
    (setdata2-help htable (list (sub1 (car (second lst)))(cadr (second lst))) (+ 1 ptr))))

(define (addinstruction htable lst ptr)
  (cond
    [(empty? lst) htable]
    [(equal? (car (car lst)) 'const) (if (hash-has-key? htable (second (car lst)))
                                         (error "duplicate definition.")
                                         (begin
                                           (hash-set! htable (cadr (car lst)) (caddr (car lst)))
                                           (addinstruction htable (cdr lst) ptr)))]
    [(equal? (car (car lst)) 'label) (if (hash-has-key? htable (second (car lst)))
                                         (error "duplicate definition.")
                                         (begin
                                           (hash-set! htable (cadr (car lst)) ptr)
                                           (addinstruction htable (cdr lst) ptr)))]
    [(equal? (car (car lst)) 'data) (if (hash-has-key? htable (second (car lst)))
                                         (error "duplicate definition.")
                                         (if (list? (caddr (car lst)))
                                        (addinstruction (setdata2 htable (cdr(car lst)) ptr)
                                                        (cdr lst)
                                                        (+ ptr (car (caddr(car lst)))))
                                        (addinstruction (setdata1 htable (cdr (car lst)) ptr)
                                                        (cdr lst)
                                                        (+ ptr (- (length (car lst)) 2)))))]
    [(equal? (car (car lst)) 'halt) (hash-set! htable 0 0)
                                    (addinstruction htable (cdr lst) (add1 ptr))]))

;returns a pair of (num (list of vars)) with all vars equal to the value of num, if circular then report error.
(define (checkerror htable key)
  (define (checkerror-h htable key myset)
    (cond
      [(symbol? (hash-ref htable key [(error "undefined usage")])) (if (set-member? myset (hash-ref htable key))
                                           (error "circular data definition.")
                                           (checkerror-h htable (hash-ref htable key) (set-add myset key)))]
      [(number? (hash-ref htable key)) (list (hash-ref htable key) (set->list myset))]
      ))
  (if (symbol? (hash-ref htable key))
      (checkerror-h htable key (set))
      empty))

(define (replace-val htable mylst) ;mylst in the form of (list num (list of vars))
  (define (replace-help htable num varlst)
    (cond
      [(empty? varlst) htable]
      [true (hash-set! htable (car varlst) num)
       (replace-help htable num (cdr varlst))]))
  (cond  
    [(empty? mylst) htable]
    [true (replace-help htable (car mylst) (second mylst))]))

(define (check-replace htable)
  (define table-lst (hash->list htable))
  (define (cr-help htable hlst)
    (cond
      [(empty? hlst) htable]
      [true (cr-help (replace-val htable (checkerror htable (car (car hlst))))
                     (cdr hlst))]))
  (cr-help htable table-lst))

(define a '((label LABELONE) (label  LABELTWO) (label LABELTHREE)(const A B) (const B 3) (const C 5)
                             (data fish grouper bass tarpon tuna) (data people (5 names))))
(define b '((data people (5 names))))
(define c '((const A B)
            (const B C)
            ))
(define d '((const A B) (const A C)(const B C) (const E F)(const C 2)(const F D)))

(define htable (addinstruction storage c 0))

