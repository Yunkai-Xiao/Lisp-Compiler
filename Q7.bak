#lang racket
(define psymboltable
  (make-hash))
(define test-prog
  '((label LOOP-TOP)        ; loop-top:
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
    (data TMP1 0)
    ))
(define a '((const A B)
            (const B C)
            ))
(define c '((const A B) (const A E)(const B C) (const E F)(const C 2)(const F 5)))
(define d '((const A B) (const D E)(const B C) (const E F)(const C 2)(const F D)))
(define pc 0)

(define (first-phase inst)
   
  (cond
    [(empty? inst) empty]
    [(symbol=? (first (first inst)) 'label)
     (if (hash-has-key? psymboltable (second (first inst)))
         (error "duplicate")
         (begin
           (hash-set! psymboltable (second (first inst)) pc)
           (first-phase (rest inst))))]
    [(symbol=? (first (first inst)) 'const)
     (if (hash-has-key? psymboltable (second (first inst)))
         (error "duplicate")
         (begin
           (hash-set! psymboltable (second (first inst)) (third (first inst)))
           (first-phase (rest inst))))]
    [(symbol=? (first (first inst)) 'data)
     (if (hash-has-key? psymboltable (second (first inst)))
         (error "duplicate")
         (begin
           (hash-set! psymboltable (second (first inst)) pc)
           (cond
             [(list? (third (first inst)))
              (set-data-abbr (first (third (first inst)))
                             (second (third (first inst))))]
             [else
              (set-data (rest (rest (first inst))))])
           (first-phase (rest inst))))]
    [else
     (first-phase (rest inst))]))

(define (set-data datalst)
  (cond
    [(empty? datalst) empty]
    [else
     (hash-set! psymboltable pc (first datalst))
     (set! pc (add1 pc))
     (set-data (rest datalst))]))

(define (set-data-abbr times data)
  (cond
    [(zero? times) empty]
    [else
     (hash-set! psymboltable pc data)
     (set! pc (add1 pc))
     (set-data-abbr  (sub1 times) data)]))
;; Tests for first phase here
(first-phase a)
psymboltable
;; ====================== Second Phase ========================
;; in second phase, we are going to resovle any psymbols
(define (second-phase psymbolhashtable)
  (second-phase-h (hash->list psymbolhashtable)))


(define (second-phase-h psymbollst)
  (cond
    [(empty? psymbollst) empty]
    [(and (symbol? (car (first psymbollst)))
          (symbol? (cdr (first psymbollst))))
     (search-sym (cdr (first psymbollst)) (set (car (first psymbollst))))
     (second-phase-h (cdr psymbollst))]
    [(and (number? (car (first psymbollst)))
          (symbol? (cdr (first psymbollst))))
     (search-sym (cdr (first psymbollst)) (set))
     (hash-set! psymboltable (car (first psymbollst))
                (hash-ref psymboltable (cdr (first psymbollst))))
     (second-phase-h (cdr psymbollst))]
    [else
     (second-phase-h (cdr psymbollst))]))

(define (search-sym sym localset)
  (define sym-cont (hash-ref psymboltable sym (λ() (error "undefined"))))
  (cond
    [(set-member? localset sym) (error "circular")]
    [(symbol? sym-cont)
     (search-sym sym-cont (set-add localset sym))]
    [else
     (sets-all-syms (set->list (set-add localset sym)) sym-cont)]))

(define (sets-all-syms setlst data)
  (cond
    [(empty? setlst) empty]
    [else
     (hash-set! psymboltable (first setlst) data)
     (sets-all-syms (rest setlst) data)]))

(second-phase psymboltable)
psymboltable

;; ==================  Final Phase ==========================
(define (find-in-hash data)
  (cond
    [(number? data) data]
    [(list? data) data]
    [else
     (hash-ref psymboltable data)]))
(define (third-phase primp-prog)
  (if (empty? primp-prog) empty
  (match (first primp-prog)
      [`(add,dest,opd1,opd2)
       (cons (list 'add
                   (find-in-hash dest)
                   (find-in-hash opd1)
                   (find-in-hash opd2))
             (third-phase (rest primp-prog)))]
    [`(sub,dest,opd1,opd2)
       (define _dest dest)
       (define _opd1 opd1)
       (define _opd2 opd2)
       (cond
         [(symbol? _dest) (set! _dest (hash-ref psymboltable _dest))]
         [(symbol? _opd1) (set! _opd1 (hash-ref psymboltable _opd1))]
         [(symbol? _opd2) (set! _opd2 (hash-ref psymboltable _opd2))])
       
       (cons (list 'sub _dest _opd1 _opd2)
             (third-phase (rest primp-prog)))]
    
    [x
     x
     (third-phase (rest primp-prog))])))
;;(third-phase test-prog)

     
     
