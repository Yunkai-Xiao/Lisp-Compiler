#lang racket
(define totaltable (make-hash))
(define datatable (make-hash))
(define psymboltable
  (make-hash))
(define test-prog1
  '((label LOOP-TOP)
    (lit 3); loop-top:
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
    (data X (10 Y))
    (data Y 1)
    (data TMP1 0)
    ))
(define test-prog '((const X 1) (data Y (10 X))))
(define a '((const A B)
            (const B C)
            ))
(define c '((const A B) (const D E)(const B C) (const E F)(const C 2)(const F 5)))
(define d '((const A B) (const D E)(const B C) (const E F)(const C 2)(const F D)))
(define pc 0)

(define (first-phase inst)
   
  (cond
    [(empty? inst) empty]
    [(symbol=? (first (first inst)) 'label)
     (if (or (hash-has-key? datatable (second (first inst)))
             (hash-has-key? psymboltable (second (first inst))))
         (error "duplicate")
         (begin
           (hash-set! psymboltable (second (first inst)) pc)
           (hash-set! totaltable (second (first inst)) pc)
           (first-phase (rest inst))))]
    [(symbol=? (first (first inst)) 'const)
     (if (or (hash-has-key? datatable (second (first inst)))
             (hash-has-key? psymboltable (second (first inst))))
         (error "duplicate")
         (begin
           (hash-set! psymboltable (second (first inst)) (third (first inst)))
           (hash-set! totaltable (second (first inst)) (third (first inst)))
           (first-phase (rest inst))))]
    [(symbol=? (first (first inst)) 'data)
     (if (or (hash-has-key? datatable (second (first inst)))
             (hash-has-key? psymboltable (second (first inst))))
         (error "duplicate")
         (begin
           (hash-set! datatable (second (first inst)) pc)
           (hash-set! totaltable (second (first inst)) pc)
           (cond
             [(list? (third (first inst)))
              (set-data-abbr (first (third (first inst)))
                             (second (third (first inst))))]
             [else
              (set-data (rest (rest (first inst))))])
           (first-phase (rest inst))))]
    [else
     (set! pc (+ pc 1))
     (first-phase (rest inst))]))

(define (set-data datalst)
  (cond
    [(empty? datalst) empty]
    [else
     (hash-set! datatable pc (first datalst))
     (hash-set! totaltable pc (first datalst))
     (set! pc (add1 pc))
     (set-data (rest datalst))]))

(define (set-data-abbr times data)
  (cond
    [(zero? times) empty]
    [else
     (hash-set! datatable pc data)
     (hash-set! totaltable pc data)
     (set! pc (add1 pc))
     (set-data-abbr  (sub1 times) data)]))
;; Tests for first phase here
(first-phase test-prog)
;psymboltable
;datatable
;; ====================== Second Phase ========================
;; in second phase, we are going to resovle any psymbols
(define (second-phase psymbolhashtable)
  (void (second-phase-h (hash->list psymbolhashtable) psymbolhashtable)))


(define (second-phase-h psymbollst table)
  
  (cond
    [(empty? psymbollst) empty]
    [(and (symbol? (car (first psymbollst)))
          (symbol? (cdr (first psymbollst))))
     (search-sym (cdr (first psymbollst)) (set (car (first psymbollst))) table)
     (second-phase-h (cdr psymbollst) table)]
    [(and (number? (car (first psymbollst)))
          (symbol? (cdr (first psymbollst))))
     (search-sym (cdr (first psymbollst)) (set) table)
     (hash-set! table (car (first psymbollst))
                (hash-ref table (cdr (first psymbollst))))
     (second-phase-h (cdr psymbollst) table)]
    [else
     (second-phase-h (cdr psymbollst) table)]))

(define (search-sym sym localset table)
  (define sym-cont (hash-ref totaltable sym (λ() (error "undefined"))))
  (display sym-cont)
  (cond
    [(set-member? localset sym) (error "circular")]
    [(symbol? sym-cont)
     (search-sym sym-cont (set-add localset sym))]
    [else
     (sets-all-syms (set->list (set-add localset sym)) sym-cont table)]))

(define (sets-all-syms setlst data table)
  (cond
    [(empty? setlst) empty]
    [else
     (hash-set! table (first setlst) data)
     (sets-all-syms (rest setlst) data table)]))

(second-phase psymboltable)
(second-phase datatable)
psymboltable
datatable
;; ==================  Final Phase ==========================
(define (find-in-hash data)
  (cond
    [(number? data) data]
    [(list? data) data]
    [else
     (hash-ref psymboltable data)]))
(define (third-phase prog lens)
  (cond
    [(empty? prog)
     empty]
    [else
     (define curinst (first prog))
     (cond
       [(or (symbol=? (first curinst) 'label)
            (symbol=? (first curinst) 'const)
            (symbol=? (first curinst) 'data)
            )
        (third-phase (rest prog) lens)]
       [(symbol=? (first curinst) 'lit)
        (cons (second curinst)
        (third-phase (rest prog) (add1 lens)))]
       [(symbol=? (first curinst) 'halt)
        (cons 0
        (third-phase (rest prog) (add1 lens)))]
       [else
        (cons (cons (first curinst)
                    (replace (rest curinst)))
              (third-phase (rest prog) (add1 lens)))]
        )]))

(define (replace inst)
  (cond
    [(empty? inst) empty]
    [(hash-has-key? psymboltable (first inst))
     
     (cons (hash-ref psymboltable (first inst))
           (replace (rest inst)))]
    [(hash-has-key? datatable (first inst))
     (cons (list (hash-ref datatable (first inst)))
           (replace (rest inst)))]
    [else
     (cons (first inst)
           (replace (rest inst)))]))

(define (datas lens)
  (cond
    [(hash-has-key? datatable lens)
     (cons (hash-ref datatable lens)
           (datas (add1 lens)))]
    [else
     empty]))
  
;(third-phase test-prog 0)

(define (primp-assemble prog)
  (begin
    (first-phase prog)
    (second-phase psymboltable)
    (second-phase datatable)
    (third-phase prog 0)))

;;(primp-assemble test-prog)
     
