(load "pattern-matcher.scm")
(load "final.scm")
(print-graph #f) ; display circular structures
(print-gensym #f) ; print gensym as g1234
(case-sensitive #f) ; ditto
(print-brackets #f) ; do not use brackets when pretty-printing

(revert-interaction-semantics) ; allow builtins to be redefined

;;; fix bug in optimizer
(#%$sputprop 'append '*flags* 122)
(#%$sputprop 'append! '*flags* 34)
(#%$sputprop 'list* '*flags* 1250)
(#%$sputprop 'cons* '*flags* 1250)

;;; And just for good luck :-)
(define with (lambda (s f) (apply f s)))

(define *void-object* (void));(display "making the void object\n")) ;'\#<void>)

; make predicate for list of 'pre's 
(define ^list-of
  (lambda (pre)
    (lambda (list)
      (and (list? list)
           (andmap pre list)))))

; check that let vars are of different names
(define distinct-vars?
  (lambda (var-val-pairs)
    (letrec ((check-diff
              (lambda (ls)
                (if (null? ls)
                    #t
                    (let ((first (car ls)) (rest (cdr ls)))
                      (and
                       (andmap (lambda(var) (not (equal? var first))) rest)
                       (check-diff rest)))))))
      (check-diff (map car var-val-pairs)))
    ))

;; const
(define simple-const? 
  (lambda (exp)
    (or 
     (boolean? exp)
     (char? exp)
     (number? exp)
     (string? exp)
     (eq? (void) exp)
     )))

; varibles
(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote 
        unquote-splicing quote set!))

(define var?
  (lambda (exp)
    (and
     (andmap
      (lambda (res) (not (equal? res exp)))
      *reserved-words*)
     (symbol? exp)
     )
    ))

; cond
(define clause?
  (lambda(exp)
    (not (null? (cdr exp)))
    ))

; lambda
(define vars-list?
  (lambda (lst)
    (and 
     (list? lst)
     (andmap var? lst)
     )
    ))

(define opt-vars-list?
  (lambda (lst)
    (and 
     (pair? lst)
     (var? (car lst))
     (or 
      (var? (cdr lst))
      (opt-vars-list? (cdr lst)))
     )))

(define opt-lambda-vars
  (lambda (vars)
    (if (pair? vars)
        (cons (car vars) (opt-lambda-vars (cdr vars)))
        '())))

(define opt-lambda-rest
  (lambda (vars)
    (if (pair? vars)
        (opt-lambda-rest (cdr vars))
        vars)))

; define
(define MIT-style?
  (lambda (args)
    (or
     (vars-list? args) ; simple lambda
     (opt-vars-list? args) ; optional arguments lambda
     (var? args) ; variadic lambda
     )))

;application
(define application?
  (lambda (exp)
    (and
     (list? exp)
     (andmap exp? exp)
     )))

; begin
(define begin-body?
  (lambda (body)
    (and
     (list? body)
     ;(andmap exp? body)
     )))  

(define begin?
  (lambda (e)
    (and
     (list? e)
     (equal? (car e) 'begin)
     (begin-body? (cdr e))
     )))

(define beginify
  (lambda (lst)
    (cond ((null? lst) (parse *void-object*))
          ((null? (cdr lst)) (parse (car lst)))
          (else `(seq ( ,@(map parse lst))))
          )))

; let
(define var-val-pair?
  (lambda (exp)
    (and (pair? exp) (var? (car exp)))))

(define with
  (lambda (s f) 
    (apply f s))) 

(define expand-letrec
  (lambda (letrec-expr)
    (with letrec-expr
          (lambda (ribs exprs)
            (let* ((fs (map car ribs))
                   (lambda-exprs (map cdr ribs))
                   (nu (gensym))
                   (nu+fs `(,nu ,@fs))
                   (body-f `(lambda ,nu+fs ,@exprs))
                   (hofs
                    (map (lambda (lambda-expr) `(lambda ,nu+fs ,@lambda-expr))
                         lambda-exprs)))
              `(Ym ,body-f ,@hofs))))))

; quasiquote
(define expand-qq
  (lambda (e)
    (cond ((unquote? e) (cadr e))
          ((unquote-splicing? e)
           (error 'expand-qq "unquote-splicing here makes no sense!"))
          ((pair? e)
           (let ((a (car e))
                 (b (cdr e)))
             (cond ((unquote-splicing? a) `(append ,(cadr a) ,(expand-qq b)))
                   ((unquote-splicing? b) `(cons ,(expand-qq a) ,(cadr b)))
                   (else `(cons ,(expand-qq a) ,(expand-qq b))))))
          ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
          ((or (null? e) (symbol? e)) `',e)
          (else e))))

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
           (eq? (car e) tag)
           (pair? (cdr e))
           (null? (cddr e))))))

(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

;;;;;;;;;;;
;; parsers
;;;;;;;;;;;

(define exp?
  (lambda (exp)
    (and
     (andmap
      (lambda (res) (not (equal? res exp)))
      *reserved-words*)
     (parse exp))
    ))

;; (quasiquote <sexpr>)
(define <quasiquote-parser>
  (compose-patterns
   (pattern-rule
    `(quasiquote . ,(? 'exprs pair? list?))
    (lambda (exprs)
      (parse (expand-qq exprs))))
   ))

;; sub parser for derived expresions
(define <derived-parser>
  (compose-patterns
   ;(let <var-lst> <exps)
   (pattern-rule
    `(let ,(? 'var-lst (^list-of var-val-pair?) distinct-vars?) ,(? 'exp) . ,(? 'exps list?))
    (lambda (var-lst exp exps)
      (parse `((lambda ,(map car var-lst) ,exp ,@exps)
               ,@(map cadr var-lst)))))
   ;; let*
   (pattern-rule
    `(let* () ,(? 'expr) . ,(? 'exprs list?))
    (lambda (expr exprs)
      (beginify (cons expr exprs))
      ))
   (pattern-rule
    `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
    (lambda (var val rest exprs)
      (parse `(let ((,var ,val))
                (let* ,rest . ,exprs)))))
   ; (letrec <var-lst> <exps)
   (pattern-rule
    `(letrec ,(? 'ribs list?) . ,(? 'exprs pair? list?))
    (lambda (ribs exprs)
      (parse (expand-letrec `(,ribs ,exprs)))))
   
   ; and
   (pattern-rule
    `(and)
    (lambda () (parse #t)))
   (pattern-rule
    `(and ,(? 'last))
    (lambda (last) (parse last)))
   (pattern-rule
    `(and ,(? 'first) . ,(? 'rest pair? list?))
    (lambda (first rest)
      (parse `(if ,first (and ,@rest) #f))
      ))
   
   
   ; cond
   (pattern-rule
    `(cond (else . ,(? 'body list? pair?)))
    (lambda (body)
      (parse `(begin . ,body))
      ))
   (pattern-rule
    `(cond ,(? 'only-clause clause?))
    (lambda (only-clause)
      (parse `(if ,(car only-clause)
                  (begin . ,(cdr only-clause))))
      ))
   (pattern-rule
    `(cond ,(? 'first-clause clause?) . ,(? 'rest list?))
    (lambda (first-clause rest)
      (parse `(if ,(car first-clause)
                  (begin . ,(cdr first-clause))
                  (cond ,@rest)))
      ))
   ))

; general parser
(define parse
  (let ((run
         (compose-patterns
          ; simple constants
          (pattern-rule
           (? 'c simple-const?)
           (lambda (c) `(const ,c)))
          ; (quote <exp>)
          (pattern-rule
           `(quote ,(? 'c))
           (lambda (c) `(const ,c)))
          ; varible name
          (pattern-rule
           (? 'v var?)
           (lambda (v) `(var ,v)))
          
          ; (if <test> <dit>)
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit))
           (lambda (test dit)
             `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
          ; (if <test> <dit> <dif>)
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
           (lambda (test dit dif)
             `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
          
    ;;; assigment 3 part ;;;
          ; (or)
          (pattern-rule
           `(or)
           (lambda () (parse '#f)))
          ; (or <e>)
          (pattern-rule
           `(or ,(? 'exp))
           (lambda (exp) (parse exp)))
          ; (or <e1> ... <en>)
          (pattern-rule
           `(or . ,(? 'exp-lst list? pair?))
           (lambda (exp-lst) 
             `(or ,(map parse exp-lst))))
          
     ;;; assignment 3 part end ;;;
          
          ; (lambda (<var-1> ... <var-n>) <e1> .. <em>) : regular lambda
          (pattern-rule
           `(lambda ,(? 'vars vars-list?) . ,(? 'body pair? list? begin-body?))
           (lambda (vars body)
             `(lambda-simple ,vars ,(parse `(begin ,@body)))))
          ; (lambda (<var-1> ... <var-n> . <rest>) <e1> .. <em>) : lambda with optional arguments
          (pattern-rule
           `(lambda ,(? 'vars opt-vars-list?) . ,(? 'body begin-body?))
           (lambda (vars body)
             `(lambda-opt ,(opt-lambda-vars vars) ,(opt-lambda-rest vars) ,(parse `(begin ,@body)))))
          ; (lambda <args> <e1> .. <em>) : variadic lambda
          (pattern-rule
           `(lambda ,(? 'vars var?) . ,(? 'body begin-body?))
           (lambda (vars body)
             ;`(lambda-opt () ,vars ,(parse `(begin ,@body)))))
             `(lambda-variadic ,vars ,(parse `(begin ,@body)))))
          
          ; (define <var> <exp>) : regular define
          (pattern-rule
           `(define ,(? 'var var?) ,(? 'body))
           (lambda (var body)
             `(define ,(parse var) ,(parse body))))
          ; (define (<var> <arg-1> ... <arg-n>) <e1> ... <em>) : MIT style 
          (pattern-rule
           `(define (,(? 'name var?) . ,(? 'MIT-args MIT-style?)) . ,(? 'body begin-body?))
           (lambda (name MIT-args body)
             `(define ,(parse name) ,(parse `(lambda ,MIT-args ,@body)))))
          
          ; (<var>  <e1> ... <em>) : aplication
          (pattern-rule
           `(,(? 'proc exp?) . ,(? 'args list?))
           (lambda (proc lst)
             `(applic ,(parse proc) ,(map parse lst))))
          
          ;(begin <exp-1> ... <exp-n>) ; sequence
          (pattern-rule
           `(begin . ,(? 'exp list?))
           (lambda (lst)
             (beginify lst)))
          
          <derived-parser>
          <quasiquote-parser>
          ;; add more rules here
          )))
    (lambda (e)
      (run e
           (lambda ()
             (error 'parse
              (format "I can't recognize this: ~s" e))
             ;#f
             )))))


;;; assignment 3 stuff ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ^tagged-by
  (lambda (tag)
    (lambda (lst) (and (pair? lst) (equal? (car lst) tag)))))

(define lambda?
  (lambda (exp)(or (lambda-simple? exp) (lambda-opt? exp) (lambda-variadic? exp))))

(define lambda-simple?    (^tagged-by 'lambda-simple))
(define lambda-opt?       (^tagged-by 'lambda-opt))
(define lambda-variadic?  (^tagged-by 'lambda-variadic))
(define tagged-var?       (^tagged-by 'var))

(define tag-bound-var
  (lambda (var env major)
    (if (null? env) 
        `(fvar ,var)
        (let ((bound (assoc var (car env))))
          (if bound 
              (let ((minor (cdr bound)))
                (if (= major -1) 
                    `(pvar ,var ,minor)
                    `(bvar ,var ,major ,minor)))
              (tag-bound-var var (cdr env) (add1 major))
              )))))

(define ^lambda-env
  (lambda (pe env)
    (letrec ((run (lambda (params minor)
                    (if (null? params) 
                        params
                        (cons (cons (car params) minor) (run (cdr params) (add1 minor)))
                        ))))
      (cond ((lambda-simple? pe) (cons (run (cadr pe) 0) env))
            ((lambda-opt? pe)    (cons (run `(,@(cadr pe) ,(caddr pe)) 0) env))
            ((lambda-variadic? pe)    (cons (run (list (cadr pe)) 0) env))
            (else (error '^lambda-env (format "unknown type of lambda: ~a" pe))))
      )))

(define lambda-body
  (lambda (exp) (if (lambda-opt? exp)
                    (cadddr exp)
                    (caddr exp))))

(define lambda->lex-pe
  (lambda (pe env)
    (let ((parsed-body 
           (inner_pe->lex-pe (lambda-body pe) (^lambda-env pe env))))
      (cond ((lambda-simple? pe)    `(,(car pe) ,(cadr pe) ,parsed-body))
            ((lambda-opt? pe)       `(,(car pe) ,(cadr pe) ,(caddr pe) ,parsed-body))
            ((lambda-variadic? pe) `(,(car pe) ,(cadr pe) ,parsed-body))
            (else (error 'lambda->lex-pe (format "unknown type of lambda: ~a" pe))))
      )))

(define inner_pe->lex-pe
  (lambda (pe env)
    (cond
      ((tagged-var? pe) (tag-bound-var (cadr pe) env -1))
      ((lambda? pe) (lambda->lex-pe pe env))
      ((list? pe) (map (lambda (exp) (inner_pe->lex-pe exp env)) 
                         pe))
      ((pair? pe) `(,(inner_pe->lex-pe (car pe) env) . ,(inner_pe->lex-pe (cdr pe) env)))
      (else pe))
    ))
    

(define pe->lex-pe
  (lambda (pe)
    (inner_pe->lex-pe pe '())))


;;;;;;; Ass3 Q4
(define annotate-tc-logic
  (lambda (pe in-tc?)
    (let ((func (car pe)))
      (cond ((eq? func 'var) pe)
            ((eq? func 'const) pe)
            ((eq? func 'fvar) pe)
            ((eq? func 'pvar) pe)
            ((eq? func 'bvar) pe)
            ((eq? func 'seq) pe)
            ((eq? func 'if3)
             (with pe (lambda(_if3 test dit dif)
                        `(if3 ,(annotate-tc-logic test #f)
                              ,(annotate-tc-logic dit in-tc?)
                              ,(annotate-tc-logic dif in-tc?)))))
            ((eq? func 'or)
             (with pe (lambda(_or exps)
                        (let ((last (last exps)) (all-but-last (all-but-last exps)))
                          `(or ,(append (map (lambda (e) (annotate-tc-logic e #f)) all-but-last)
                                     `(,(annotate-tc-logic last in-tc?))))
                          ))))
            ((eq? func 'define)
             (with pe (lambda(_define var val)
                        `(define ,var ,(annotate-tc-logic val #f)))))
            ((in? func '(lambda-simple lambda-variadic))
             (with pe (lambda (_lambda params body)
                        `(,_lambda ,params ,(annotate-tc-logic body #t)))))
            ((eq? func 'lambda-opt)
             (with pe (lambda (_lambda-opt params opt-params body)
                        `(lambda-opt ,params ,opt-params ,(annotate-tc-logic body #t)))))
            ((eq? func 'applic)
             (with pe (lambda (_applic proc args)
                        (let ((new-proc (annotate-tc-logic proc #f))
                              (new-args (map (lambda (e) (annotate-tc-logic e #f)) args)))
                          `(,(if in-tc? 'tc-applic 'applic) ,new-proc ,new-args)))))
            ))
    ))
             
             
;; ass3 q4 auxileries
(define annotate-tc
  (lambda (pe)
    (annotate-tc-logic pe #f)
    ))

(define last
  (lambda (lst)
    (if (and (list? lst) (pair? lst))
        (car (reverse lst))
        lst)
    ))

(define all-but-last
  (lambda (lst)
    (if (and (list? lst) (pair? lst))
        (reverse (cdr (reverse lst)))
        lst)
    ))

(define annotate-tc-false
  (lambda (pe)
    (annotate-tc-logic pe #f)))

(define in?
  (lambda (e lst)
    (cond ((null? lst) #f)
          ((eq? (car lst) e) #t)
          (else (in? e (cdr lst))))
    ))
