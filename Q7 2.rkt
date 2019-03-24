#lang racket
(define datatable (make-hash))
(define consttable (make-hash))
(define labeltable (make-hash))
(define totaltable (make-hash))
(define test '((label X)
               
               (data B 5 6 7)
               (const A 4)
               (data D (7 A))
               (const Y Z)
               (const Z 4)))
(define ptr 0)
(define (first-phase prog)
  (cond
    [(empty? prog) empty]
    [else
     
     (define curinst (first prog))
     
     (cond
       [(symbol=? (first curinst) 'label)
        (if (hash-has-key? totaltable (second curinst)) (error "duplicate")
            (begin (hash-set! labeltable (second curinst) ptr)
                   (hash-set! totaltable (second curinst) ptr)
                   (first-phase (rest prog))))]
       [(symbol=? (first curinst) 'const)
        (if (hash-has-key? totaltable (second curinst)) (error "duplicate")
            (begin
              (hash-set! consttable (second curinst) (third curinst))
              (hash-set! totaltable (second curinst) (third curinst))
              (first-phase (rest prog))))]
       [(symbol=? (first curinst) 'data)
        (if (hash-has-key? totaltable (second curinst)) (error "duplicate")
            (begin
              (hash-set! datatable (second curinst) ptr)
              (hash-set! totaltable (second curinst) ptr)
              (if (list? (third curinst))
                  (data-add-abbr (first (third curinst)) (second (third curinst)))
                  (data-add (rest (rest curinst))))
              (first-phase (rest prog))))]
       [else
        (set! ptr (+ 1 ptr))
        (first-phase (rest prog))])]))
(define (data-add datas)
  (cond
    [(empty? datas) void]
    [else
     (hash-set! datatable ptr (first datas))
     (hash-set! totaltable ptr (first datas))
     (set! ptr (+ 1 ptr))
     (data-add (rest datas))]))
(define (data-add-abbr times data)
  (cond
    [(zero? times) void]
    [else
     (hash-set! datatable ptr data)
     (hash-set! totaltable ptr data)
     (set! ptr (+ 1 ptr))
     (data-add-abbr (sub1 times) data)]))
        



;; Tests for first phase here
(define (first-phase-test prog)
  (first-phase prog)
  (display "LabelTable: ")
  (display labeltable)
  (newline)
  (display "consttable: ")
  (display consttable)
  (newline)
  (display "datatable: ")
  (display datatable)
  (newline)
  (display "totaltable: ")
  (display totaltable)
  (newline))
  
;(first-phase-test test-prog)

;; ====================== Second Phase ========================
;; in second phase, we are going to resovle any psymbols
(define (resolve-const)
  (resolve-const-h (hash->list consttable)))

(define (resolve-const-h constlst)
  (cond
    [(empty? constlst) void]
    [(symbol? (cdr (first constlst)))
     (set-sym-const (cdr (first constlst)) (set (car (first constlst))))
     (resolve-const-h (rest constlst))]
    [else
     (resolve-const-h (rest constlst))]))
(define (set-sym-const sym set)
  (define key (hash-ref totaltable sym (λ() (error "undefined"))))
  (cond
    [(set-member? set key) (error "circular")]
    [(symbol? key)
     (set-sym-const key (set-add set key))]
    [else
     (set-all (set->list set) key)]))


(define (set-all setlst key)
  (cond
    [(empty? setlst)
     empty]
    [(hash-has-key? consttable (first setlst))
     (hash-set! consttable (first setlst) key)
     (hash-set! totaltable (first setlst) key)
     (set-all (rest setlst) key)]
    [else
     (set-all (rest setlst) key)]))

(define (resolve-data)
  (resolve-data-h (hash->list datatable)))
(define (resolve-data-h datalst)
  (cond
    [(empty? datalst) empty]
    [else
     (define datpair (first datalst))
     (cond
       [(symbol? (cdr datpair))
        (hash-set! datatable (car datpair) (hash-ref totaltable (cdr datpair)
                                                     (λ() (error "undefined"))))
        (hash-set! totaltable (car datpair) (hash-ref totaltable (cdr datpair)
                                                      (λ() (error "undefined"))))
        (resolve-data-h (rest datalst))]
       [else
        (resolve-data-h (rest datalst))])]))
(define (second-phase-test)
  (resolve-const)
  (resolve-data)
  (display "LabelTable: ")
  (display labeltable)
  (newline)
  (display "consttable: ")
  (display consttable)
  (newline)
  (display "datatable: ")
  (display datatable)
  (newline)
  (display "totaltable: ")
  (display totaltable)
  (newline))

(define (second-phase)
  (resolve-const)
  (resolve-data))


(define (data? key)
  (hash-has-key? datatable key))
(define (label? key)
  (hash-has-key? labeltable key))
(define (const? key)
  (hash-has-key? consttable key))

(define (primp-assemble aprimplst)
  (first-phase aprimplst)
  (second-phase)
  (define htable totaltable)
  (define (checkref key) ;for basic operators (add, sub, mul, div, mod) only
    (cond
      [(number? key) key]
      [(symbol? key)
       (cond
         [(data? key) (list (hash-ref htable key (λ() (error "undefined"))))]
         [else (hash-ref htable key (λ() (error "undefined")))])]
      [(string? key) key]
      [(list? key)
       (list (first key)
             (checkref (hash-ref htable (second key) (λ()(error "undefined")))))]))
  (define (checklist-help key pair htable)
    (define startkey (hash-ref htable key (λ() (error "undefined"))))
    (define (cl-h key pair htable acc)
      (cond
        [(zero? (first pair)) acc]
        [true (cl-h (add1 startkey) (list (sub1(first pair))(second pair)) htable (cons (second pair) acc))]))
    (cl-h startkey pair htable empty)
    )
  (define (dolist key len); easy data type
    (define startkey (hash-ref htable key (λ() (error "undefined"))))
    (define (dl-help key len acc)
      (cond
        [(zero? len) acc]
        [true (dl-help (add1 key) (sub1 len) (cons (hash-ref htable key (λ() (error "undefined")))acc))]))
    (dl-help startkey len empty))
  (define (pa-h lst acc)
    (if (empty? lst) (reverse acc)
        (cond
          [true (match (car lst)
                  [`(add ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                                (pa-h (cdr lst) (cons (list 'add (checkref dest) (checkref opd1)(checkref opd2)) acc)))]
                  [`(sub ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                                (pa-h (cdr lst) (cons (list 'sub (checkref dest) (checkref opd1)(checkref opd2)) acc)))]
                  [`(mul ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                                (pa-h (cdr lst) (cons (list 'mul (checkref dest) (checkref opd1)(checkref opd2)) acc)))]
                  [`(div ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                                (pa-h (cdr lst) (cons (list 'div (checkref dest)(checkref opd1)(checkref opd2)) acc)))]
                  [`(mod ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                                (pa-h (cdr lst) (cons (list 'mod (checkref dest)(checkref opd1)(checkref opd2)) acc)))]
                  [`(gt ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                               (pa-h (cdr lst) (cons (list 'gt (checkref dest)(checkref opd1)(checkref opd2)) acc)))]
                  [`(ge ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                               (pa-h (cdr lst) (cons (list 'ge (checkref dest)(checkref opd1)(checkref opd2)) acc)))]
                  [`(lt ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                               (pa-h (cdr lst) (cons (list 'lt (checkref dest)(checkref opd1)(checkref opd2)) acc)))]
                  [`(le ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                               (pa-h (cdr lst) (cons (list 'le (checkref dest)(checkref opd1)(checkref opd2)) acc)))]
                  [`(equal ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                                  (pa-h (cdr lst) (cons (list 'equal (checkref dest)(checkref opd1)(checkref opd2)) acc)))]
                  [`(not-equal ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                                      (pa-h (cdr lst) (cons (list 'not-equal (checkref dest)(checkref opd1)(checkref opd2)) acc)))]
                  [`(land ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                                 (pa-h (cdr lst) (cons (list 'land (checkref dest)(checkref opd1)(checkref opd2)) acc)))]
                  [`(lor ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                                (pa-h (cdr lst) (cons (list 'lor (checkref dest)(checkref opd1)(checkref opd2)) acc)))]
                  [`(lnot ,dest ,opd1 ,opd2) (if (or (label? dest) (const? dest)) (error "incorrect")
                                                 (pa-h (cdr lst) (cons (list 'lnot (checkref dest)(checkref opd1)(checkref opd2)) acc)))]
                  [`(jump ,opd) (pa-h (cdr lst) (cons (list 'jump (checkref opd)) acc))]
                  [`(branch ,opd1 ,opd2) (pa-h (cdr lst) (cons (list 'branch (checkref opd1) (checkref opd2)) acc))]
                  [`(move ,dest ,opd) (if (or (label? dest) (const? dest)) (error "incorrect")
                                          (pa-h (cdr lst) (cons (list 'move (checkref dest)(checkref opd)) acc)))]
                  [`(print-val ,opd) (pa-h (cdr lst) (cons (list 'print-val (checkref opd)) acc))]
                  [`(print-string ,str) (pa-h (cdr lst) (cons (car lst) acc))]
                  [`(halt) (pa-h (cdr lst)(cons 0 acc))]
                  [`(lit ,psym) (pa-h (cdr lst)(cons psym acc))]
                  [`(const ,psym ,opd) (pa-h (cdr lst) acc)]
                  [`(label ,psym) (pa-h (cdr lst) acc)]
                  
                  [`(data, psym, (list a b)) 
                                        (pa-h (cdr lst)(append (checklist-help psym (list a (checkref b)) htable) acc))]
                  [`(data,psym, vals ...) 
                                         (pa-h (cdr lst) (append (dolist psym (length vals)) acc))]
                    
                  )] 
          )))
  (pa-h aprimplst empty))
(define test2
  '((label LOOP-TOP)
    (gt (0 A) A B)
    (const A B)
    (const B C)
    (print-val C)
    (const C D)
    (const D 3)
    (data X 5)
    (data Y 6)
    (data Z (6 10))
    (add Y 6 Y)
    (land Z A B)
    (lor Z A B)
    (branch D X)
    (jump M)
    (const M 5)
    ))

(primp-assemble test2)