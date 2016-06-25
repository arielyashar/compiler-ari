; list of cisc primitives (kernel procedures)
(define cisc-prim
  ; (proc-name code-lable closure-addr)
  (letrec ((add-addr (lambda (prim-lst addr)
                       (if (null? prim-lst)
                           '()
                           (cons `(,@(car prim-lst) ,addr)
                                 (add-addr (cdr prim-lst) (+ 3 addr)))))))
    (add-addr                     
     `((cons "L_prim_cons")
       (car "L_prim_car") ;TODO - check what hapen whith cadr caddr ...
       (cdr "L_prim_cdr")
       (boolean? "L_pred_bool")
       (symbol? "L_pred_symbol")
       (procedure? "L_pred_procedure")
       (integer? "L_pred_integer")
       (char? "L_pred_char")
       (vector? "L_pred_vector")
       (string? "L_pred_string")
       (pair? "L_pred_pair")
       (void? "L_pred_void")
       (null? "L_pred_nil")
       (apply "L_prim_apply")
       (eq? "L_prim_eq")
       (string->symbol "L_prim_str2sym")
       (list->string "L_prim_lst2str")
       (exit "L_prim_exit")
       (display "L_prim_display")
       (+ "L_prim_plus")
       (- "L_prim_minus")
       (/ "L_prim_div")
       (* "L_prim_mul")
       (< "L_prim_less")
       (> "L_prim_greater")
       (= "L_prim_equal")
       (set-car! "L_prim_set_car")
       (set-cdr! "L_prim_set_cdr")
       (sob-length "L_prim_sob_length")
       (string-ref "L_prim_string_ref")
       (string-set! "L_prim_string_set")
       (vector-ref "L_prim_vector_ref")
       (vector-set! "L_prim_vector_set")
       (symbol->string "L_prim_sym2str")
       (integer->char "L_prim_int2char")
       (char->integer "L_prim_char2int")
       (make-vector "L_prim_make_vector")
       )
     200000)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define extract-fvars
 (lambda(exp)
   (cond ((not (list? exp)) '())
         ((null? exp) '())
         ((eq? 'fvar (car exp)) (cdr exp))
         (else (append (extract-fvars (car exp)) (extract-fvars (cdr exp)))))
   ))

; make the primitive dinamicly 
(define code-gen-lib-funcs-tbl
  (lambda(fvars-tbl)
    (if (null? fvars-tbl) 
        ""
        (let* ((sym (caar fvars-tbl))
               (prim-row (assoc sym cisc-prim))
               (sym-addr (cdar fvars-tbl)))
          (if prim-row   ; check if the symbol is primitive
              (let ((code (cadr prim-row))
                    (closure-addr (caddr prim-row)))
                (string-append
                 (format "\t/* make closure for library func: ~a */" sym) nl
                 (format "\tMOV(IND(~a), IMM(~a));" closure-addr t_closure) nl
                 (format "\tMOV(IND(~a), IMM(9999));\t\t// dummy env" (+ 1 closure-addr)) nl
                 (format "\tMOV(IND(~a), LABEL(~a));" (+ 2 closure-addr) code)  nl
                 (format "\tMOV(IND(~a), IMM(~a));\t\t//add the closure to sym-table" sym-addr closure-addr) nl
                 (code-gen-lib-funcs-tbl (cdr fvars-tbl))
                 ))
              (string-append
               (format "\tMOV(IND(~a), IMM(~a));\t\t// put null in the cell of the symbol ~a" sym-addr 0 sym) nl
               (code-gen-lib-funcs-tbl (cdr fvars-tbl))))
          ))))

; define handle (update the correct cell)
(define code-gen-define
  (lambda (pe depth)
    (with pe
          (lambda (define symbol value)
            (if (equal? (car symbol) 'fvar)
                (let ((addr (get-sym-addr (cadr symbol))))
                  (string-append
                   (format "/*** define code gen of symbol: ~a ***/" symbol) nl
                   (code-gen value depth)
                   "\tMOV(IND(" (number->string addr) ") , R0);\t\t//update the 'bucket' of the symbol: " (format "~a" symbol) nl
                   "\tMOV(R0, " (sob_void) ");\t\t// define return void" nl
                   "/** end of define code gen **/" nl nl 
                      ))
                (error 'code-gen-define (format "~a is not a free var" symbol)))
            ))
    ))   
            
; free var code
(define code-gen-fvar
  (lambda (pe depth)
    (with pe
          (lambda (fvar symbol)
            (let ((addr (get-sym-addr symbol))
                  (Lok  (^label-fvat_ok)))
              (string-append
               (format "/*** code-gen-fvar of symbol: ~a ***/" symbol) nl
               (format "\tMOV(R0, IND(~a));" addr) nl
               "\tCMP(R0, IMM(0));      // check that this varible was defined" nl
               "\tJUMP_NE(" Lok ");" nl
               (code-gen-err-msg (format "Exception: variable ~a is not bound" symbol))
               Lok ":" nl nl ; label
               
               )))
          ))
    )

; find the address of fvar
(define get-sym-addr
  (lambda (symbol)
    (let ((in-tbl (assoc symbol fvars-tbl)))
      (if in-tbl
          (cdr in-tbl)
          (error 'get-sym-addr (format "the symbol: ~a is not in sym-tbl" symbol)))
      )))

;cisc-prim ;;TODO: IF WORKS, DELETE

(define build-fvars-tab
  (lambda (f-list addr)
    (if (null? f-list)
        '()
        (cons (cons (car f-list) addr) (build-fvars-tab (cdr f-list) (add1 addr)))
        )))

; remove duplicate symbols
(define filter-dup-sym
  (lambda (lst)
    (if (null? lst)
        '()
        (let* ((c (car lst))
               (filter-first (remove c lst)))
          (if (not (symbol? c)) ; just for luck
              (error 'filter-dup-sym (format "~a is not a symbol" c))
              (cons c (filter-duplicate-const filter-first)))
          ))
    ))

; this function receive list of expresions and make the sym-tbl (list of (sym . addr) )
; should be caled at the begining
(define make-fvars-tbl
  (lambda (exp-lst addr)
    (let* ((fvars-lst (filter-dup-sym ; remove the duplicate symbols
                     (append (map car cisc-prim) ; add the primitive to
                             (apply append ; make one long list 
                                    (map extract-fvars exp-lst))))))
      (build-fvars-tab fvars-lst addr))
    ))


