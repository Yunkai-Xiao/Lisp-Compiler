#lang racket
(define datatable (make-hash))
(define consttable (make-hash))
(define labeltable (make-hash))
(define totaltable (make-hash))

(define ptr 0)
(define (first-phase prog)
  (cond
    [(empty? prog) empty]
    [else
     
     (define curinst (first prog))
     
     (cond
       [(not (list? curinst))
        (set! ptr (+ 1 ptr))
        (first-phase (rest prog))]
       [(equal? (first curinst) 'label)
        (if (hash-has-key? totaltable (second curinst)) (error "duplicate")
            (begin (hash-set! labeltable (second curinst) ptr)
                   (hash-set! totaltable (second curinst) ptr)
                   (first-phase (rest prog))))]
       [(equal? (first curinst) 'const)
        (if (hash-has-key? totaltable (second curinst)) (error "duplicate")
            (begin
              (hash-set! consttable (second curinst) (third curinst))
              (hash-set! totaltable (second curinst) (third curinst))
              (first-phase (rest prog))))]
       [(equal? (first curinst) 'data)
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

(define (resolve-imm imm)
  (cond
    [(not (symbol? imm)) imm]
    [(label? imm) (error "incorrect")]
    [(data? imm) (hash-ref totaltable imm (λ() (error "undefined")))]
    [(const? imm)(hash-ref totaltable imm (λ() (error "undefined")))]))
(define (resolve-ind ind)
  (cond
    [(not (symbol? ind)) ind]
    [(label? ind) (error "incorrect")]
    [(const? ind) (error "incorrect")]
    [(data? ind) (list (hash-ref totaltable ind (λ() (error "undefined"))))]))

(define (resolve-dest dest)
     (match dest
       [(list a b) (list (resolve-imm a) (resolve-ind b))]
       [a (if (symbol? a) (resolve-ind a) a)]))

(define (resolve-opd opd)
  (cond
    [(symbol? opd)
     (cond
       [(data? opd) (list (hash-ref totaltable opd (λ() (error "undefined"))))]
       [(label? opd) (error "incorrect")]
       [(const? opd) (hash-ref totaltable opd (λ() (error "undefined")))])]
    [else
     (match opd
       [(list a b) (list (resolve-imm a) (resolve-ind b))]
       [a a])]))

(define (resolve-opd-jb opd)
  (cond
    [(symbol? opd)
     (cond
       [(data? opd) (list (hash-ref totaltable opd (λ() (error "undefined"))))]
       [(label? opd) (hash-ref totaltable opd (λ() (error "undefined")))]
       [(const? opd) (list (hash-ref totaltable opd (λ() (error "undefined"))))])]
    [else
     (match opd
       [(list a b) (list (resolve-imm a) (resolve-ind b))]
       [a a])]))
(define (resolve-psym psym)
  (cond
    [(symbol? psym)
     (hash-ref totaltable psym (λ() (error "undefined")))]
    [else
     psym]))
      
    

(define (change-table test-prog)
  (cond
    [(empty? test-prog) empty]
    [else
     (match (first test-prog)
       [`(add,dest,opd1,opd2)
        (cons (list 'add
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(sub,dest,opd1,opd2)
        (cons (list 'sub
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(mul,dest,opd1,opd2)
        (cons (list 'mul
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(div,dest,opd1,opd2)
        (cons (list 'div
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(mod,dest,opd1,opd2)
        (cons (list 'mod
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(gt,dest,opd1,opd2)
        (cons (list 'gt
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(ge,dest,opd1,opd2)
        (cons (list 'ge
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(lt,dest,opd1,opd2)
        (cons (list 'lt
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(le,dest,opd1,opd2)
        (cons (list 'le
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(equal,dest,opd1,opd2)
        (cons (list 'equal
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(not-equal,dest,opd1,opd2)
        (cons (list 'not-equal
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(land,dest,opd1,opd2)
        (cons (list 'land
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(lor,dest,opd1,opd2)
        (cons (list 'lor
                    (resolve-dest dest)
                    (resolve-opd opd1)
                    (resolve-opd opd2))
              (change-table (rest test-prog)))]
       [`(lnot,dest,opd1)
        (cons (list 'lnot
                    (resolve-dest dest)
                    (resolve-opd opd1))
              (change-table (rest test-prog)))]
       [`(jump,opd)
        (cons (list 'jump
                    (resolve-opd-jb opd))
              (change-table (rest test-prog)))]
       [`(branch,opd1,opd2)
        (cons (list 'branch
                    (resolve-opd opd1)
                    (resolve-opd-jb opd2))
              (change-table (rest test-prog)))]
       [`(move,dest,opd)
        (cons (list 'move
                    (resolve-dest dest)
                    (resolve-opd opd))
              (change-table (rest test-prog)))]
       [`(print-val,opd)
        (cons (list 'print-val
                    (resolve-opd opd))
              (change-table (rest test-prog)))]
       [`(print-string,str)
        (cons (first test-prog)
              (change-table (rest test-prog)))]
       [`(halt) (cons 0 (change-table (rest test-prog)))]
       [`(lit,a) (cons (resolve-psym a)
                       (change-table (rest test-prog)))]
       [`(const,psym, a) (change-table (rest test-prog))]
       [`(data,psym,(list a b))
        (append (data-abbr a (resolve-psym b))
                (change-table (rest test-prog)))]
       [`(data,psym,vals ...)
        (append (datas vals)
                (change-table (rest test-prog)))]
       [`(label,psym)
        (change-table (rest test-prog))]
       [x (resolve-psym x)])]))

(define (data-abbr times data)
  (cond
    [(zero? times) empty]
    [else
     (cons data (data-abbr (sub1 times) data))]))
(define (datas vals)
  (cond
    [(empty? vals)
     empty]
    [else
     (cons (resolve-psym (first vals))
           (datas (rest vals)))]))
(define test-prog
  '(
(label LOOP-TOP)        ; loop-top:
(gt TMP1 X 0)           ;  tmp1 <- (x > 0)
(branch TMP1 LOOP-CONT) ;  if tmp1 goto loop-cont
(jump LOOP-DONE)        ;  goto loop-done
(label LOOP-CONT)       ; loop-cont:
(mul Y 2 Y)             ;  y <- 2 * y
(sub X X 1)             ;  x <- x - 1
(print-val Y)           ;  print y
(print-string "\n")     ;  print "\n"
(jump LOOP-TOP)         ;  goto loop-top
(label LOOP-DONE)       ; loop-done:
(halt)                  ;  halt
(data X 10)
(data Y 1)
(data TMP1 0)))
(define (primp-assemble test-prog)
  (first-phase test-prog)
  (second-phase)
  (change-table test-prog))
     
