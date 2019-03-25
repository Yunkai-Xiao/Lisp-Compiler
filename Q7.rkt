;Collaboration with Yunkai Xiao (20776848), CS146 Winter 2019.
;date: March 25th, 2019
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
(define (first-phase-r prog)
  (set!  datatable (make-hash))
  (set!  consttable (make-hash))
  (set!  labeltable (make-hash))
  (set!  totaltable (make-hash))
  (set! ptr 0)
  (first-phase prog))
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

(define (third-phase aprimplst acc)
  (define (solveimm imm)
    (cond
      [(not(symbol? imm)) imm]
      [(label? imm) (error "incorrect")]
      [(const? imm) (hash-ref totaltable imm (λ ()(error "undefined")))]
      [(data? imm) (hash-ref totaltable imm (λ ()(error "undefined")))]))
  (define (solveind ind)
    (cond
      [(not (symbol? ind)) ind]
      [(label? ind) (error "incorrect")]
      [(const? ind) (error "incorrect")]
      [(data? ind) (list(hash-ref totaltable ind (λ ()(error "undefined"))))]))
  (define (solvepsym psym)
    (if (symbol? psym) (hash-ref totaltable psym (λ ()(error "undefined"))) psym))
  (define (solvedest dest)
     (cond
       [(list? dest) (list (solveimm (first dest)) (solveind (second dest)))]
       [(symbol? dest) (solveind dest)]
       [true dest]))
  (define (solveopd opd)
  (cond
    [(symbol? opd)
     (cond
       [(data? opd) (list (hash-ref totaltable opd (λ() (error "undefined"))))]
       [(label? opd) (error "incorrect")]
       [(const? opd) (hash-ref totaltable opd (λ() (error "undefined")))])]
    [(list? opd) (list (solveimm (first opd)) (solveind (second opd)))]
    [true opd]))
  (define (solvebj opd)
  (cond
    [(symbol? opd)
     (cond
       [(data? opd) (list (hash-ref totaltable opd (λ() (error "undefined"))))]
       [(label? opd) (hash-ref totaltable opd (λ() (error "undefined")))]
       [(const? opd) (list (hash-ref totaltable opd (λ() (error "undefined"))))])]
    [(list? opd) (list (solveimm (first opd)) (solveind (second opd)))]
    [true opd]))
  (define (solvedata1 occur val acc)
    (if (zero? occur) acc (solvedata1 (sub1 occur) val (cons val acc))))
  (define (solvedata2 vallst acc)
    (if (empty? vallst) acc (solvedata2 (cdr vallst) (cons (solvepsym (first vallst)) acc))))
  (if (empty? aprimplst) (reverse acc)
      (match (car aprimplst)
        [`(add ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'add (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(sub ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'sub (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(mul ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'mul (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(div ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'div (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(mod ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'mod (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(gt ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'gt (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(ge ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'ge (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(lt ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'lt (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(le ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'le (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(equal ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'equal (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(not-equal ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'not-equal (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(land ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'land (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(lor ,dest ,opd1 ,opd2)(third-phase (cdr aprimplst) (cons (list 'lor (solvedest dest) (solveopd opd1) (solveopd opd2)) acc))]
        [`(lnot ,dest ,opd1)(third-phase (cdr aprimplst) (cons (list 'lnot (solvedest dest) (solveopd opd1)) acc))]
        [`(jump ,opd) (third-phase (cdr aprimplst) (cons (list 'jump (solvebj opd)) acc))]
        [`(branch ,opd1 ,opd2) (third-phase (cdr aprimplst) (cons (list 'branch (solveopd opd1) (solvebj opd2)) acc))]
        [`(move ,dest ,opd) (third-phase (cdr aprimplst) (cons (list 'move (solvedest dest) (solveopd opd)) acc))]
        [`(print-val ,opd) (third-phase (cdr aprimplst) (cons (list 'print-val (solveopd opd)) acc))]
        [`(print-string, string) (third-phase (cdr aprimplst) (cons (list 'print-string string) acc))]
        [`(halt) (third-phase (cdr aprimplst) (cons 0 acc))]
        [`(lit ,psym) (third-phase (cdr aprimplst) (cons (solvepsym psym) acc))]
        [`(const ,a ,b) (third-phase (cdr aprimplst) acc)]
        [`(label ,a) (third-phase (cdr aprimplst) acc)]
        [`(data ,name ,(list a b)) (third-phase (cdr aprimplst) (solvedata1 a (solvepsym b) acc))]
        [`(data ,name ,val ...) (third-phase (cdr aprimplst)(solvedata2 val acc))]))
  )

(define (primp-assemble aprimplst)
  (first-phase aprimplst)
  (second-phase)
  (third-phase aprimplst empty))


