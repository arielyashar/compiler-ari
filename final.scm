; (load "compiler.scm") ;; TODO - the compiler.scm loads this file
(load "compiler-tc.scm")
(load "const.scm")
(load "fvars.scm")
(load "sym_tab.scm")

;;; globals ;;;
(define const-tbl 'undefine)
(define fvars-tbl 'undefine)

;;auxileries

(define ^^label
  (lambda (name)
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        (string-append name
                       (number->string n))))))

(define ^label-if3_else (^^label "Lif3_else"))
(define ^label-if3_exit (^^label "Lif3_exit"))
(define ^label-or_exit (^^label "Lor_exit"))
(define ^label-lambda_b0_copy (^^label "Llambda_b0_copy"))
(define ^label-lambda_b0_copy_end (^^label "Llambda_b0_copy_end"))
(define ^label-lambda_env_copy (^^label "Llambda_env_copy"))
(define ^label-lambda_env_copy_end (^^label "Llambda_env_copy_end"))
(define ^label-lambda_body (^^label "Llambda_body"))
(define ^label-lambda_body_cont (^^label "Llambda_body_cont"))
(define ^label-lambda_end (^^label "Llambda_end"))
(define ^label-lambda_opt_list_insertion (^^label "Llambda_opt_list_insertion"))
(define ^label-lambda_opt_list_insertion_end (^^label "Llambda_opt_list_insertion_end"))
(define ^label-lambda_var_list_insertion (^^label "Llambda_var_list_insertion"))
(define ^label-lambda_var_list_insertion_end (^^label "Llambda_var_list_insertion_end"))
(define ^label-applic_end (^^label "Lapplic_end"))
(define ^label-applic_not_proc (^^label "Lapplic_not_proc"))
(define ^label-applic_frame_copy (^^label "Lapplic_tc_frame_copy"))
(define ^label-print_result_end (^^label "Lprint_result_end"))
(define ^label-fvat_ok (^^label "Lfvar_ok"))
(define nl (list->string (list #\newline)))

;;; constants ;;;
(define t_void "T_VOID")	
(define t_nill "T_NIL")	
(define t_bool "T_BOOL")	
(define t_char "T_CHAR")	
(define t_integer "T_INTEGER")
(define t_string "T_STRING")
(define t_symbol "T_SYMBOL")
(define t_pair "T_PAIR")
(define t_vector "T_VECTOR")
(define t_closure "T_CLOSURE")

(define fully-parse
  (lambda (exp)
    (annotate-tc  ; TODO - uncomment after implementing tc-applic
     (pe->lex-pe (parse exp))
    )
    ))

(define prologue
  (lambda ()
    (let ((const_adj (map (lambda(x) (cons (cadr x) (car x))) const-tbl)))
      (string-append
       "#include <stdio.h>" nl
       "#include <stdlib.h>" nl nl
       "/* change to 0 for no debug info to be printed: */" nl
       "#define DO_SHOW 1 //TODO - remove at the and" nl nl
       (format "#define sob_void ~a" (cdr (assoc (void) const_adj))) nl ;;TODO: using (void) is ok?
       (format "#define sob_nil ~a" (cdr (assoc '() const_adj))) nl
       (format "#define sob_true ~a" (cdr (assoc #t const_adj))) nl
       (format "#define sob_false ~a" (cdr (assoc #f const_adj))) nl
       "#include \"cisc.h\"\n\n" nl
       "int main(){" nl nl
       "\tSTART_MACHINE;" nl
       "\tJUMP(CONTINUE);" nl nl
       
       "\t#include \"char.lib\"" nl
       "\t#include \"io.lib\"" nl
       "\t#include \"math.lib\"" nl
       "\t#include \"string.lib\"" nl
       "\t#include \"system.lib\"" nl
       "\t#include \"scheme.lib\"" nl nl
       "CONTINUE:" nl nl
       
       "\t/*** dummy frame initialization ***/" nl
       "\tPUSH(IMM(0));\t\t// push initial num of args to be 0" nl
       "\tPUSH(IMM(0));\t\t// as to represent a dummy env" nl
       "\tPUSH(LABEL(end_program));\t\t// push artificial return address" nl 
       "\tPUSH(FP);\t\t// push old fp" nl nl
       "\tMOV(FP,SP);\t\t// open main fram" nl nl
       "\tADD(IND(0), IMM(300000));\t\t// make place for consts + symbols + primitives" nl nl)
      )))

(define insert-prologue
  (lambda (out-port)
    (display (prologue) out-port)
    ))

(define termination
  (lambda ()
    (string-append
     nl nl
     "\tend_program:" nl
     "\tSTOP_MACHINE;"nl
     "\treturn 0;"nl
     "}"
     )))

(define insert-termination
  (lambda (out-port)
    (display (termination) out-port)
    ))

; generate code to print error message to the screen end exit
(define code-gen-err-msg
  (lambda (string)
    ;; TODO need to implement in cisc
    (string-append
     "\t/*** code-gen-err-msg ***/" nl
     "\tprintf(\"" string "\\n\");" nl
     "\tprintf(\"exiting program \\n\");" nl
     "\tprintf(\" n = %ld\", FPARG(1));"
     "\tHALT;" nl)
    ))
;; end of auxileries



(define code-gen-pvar
  (lambda (pe depth)
    (with pe
          (lambda (pvar name minor)
            (string-append
             "\t/*** code-gen-pvar ***/" nl
             ;"\tMOV(R0,IMM(" (number->string (+ 2 minor))"));" nl
             (format "\tMOV(R0,FPARG(~a));\t\t// R0 gets pvar ~a = FPARG(2+~a)" (+ 2 minor) name minor) nl))
          )))


(define code-gen-bvar
  (lambda (pe depth)
    (with pe
          (lambda (bvar name major minor)
            (string-append
             "\t/*** code-gen-bvar ***/" nl
             "\tMOV(R0,FPARG(0));" nl
             (format "\tMOV(R0,INDD(R0, ~a));"  major) nl
             (format "\tMOV(R0,INDD(R0, ~a));" minor) nl)
            ))
    ))


(define code-gen-or
  (lambda (pe depth)
    (with pe
          (lambda (or exps)
            (let* ((Lor-exit (^label-or_exit))
                   (jump-code-gen (lambda (exp)
                                    (string-append
                                     (code-gen exp depth)
                                     "\tCMP(R0,IMM(" (sob_false) "));" nl
                                     (format "\tJUMP_NE(~a);" Lor-exit) nl))))
              (string-append 
               "\t/*** code-gen-or ***/" nl
               (apply string-append (map jump-code-gen (all-but-last exps)))
               (code-gen (last exps) depth)
               Lor-exit ":" nl)
              ))
          )))


(define code-gen-if3
  (lambda (pe depth)
    (with pe
          (lambda (if3 test do-if-true do-if-false)
            (let ((code-test (code-gen test depth))
                  (code-dit (code-gen do-if-true depth))
                  (code-dif (code-gen do-if-false depth))
                  (label-else (^label-if3_else))
                  (label-exit (^label-if3_exit)))
              (string-append
               "\t/*** code-gen-if3 ***/" nl
               code-test nl ; when run, the result of the test will be in R0
               "\tCMP(R0, IMM(" (sob_false) "));" nl
               "\tJUMP_EQ(" label-else ");" nl
               code-dit nl
               "\tJUMP(" label-exit ");" nl
               label-else ":" nl
               code-dif nl
               label-exit ":" nl)
              ))
          )))


(define code-gen-seq
  (lambda(pe depth)
    (with pe
          (lambda (seq exps)
            (string-append
             "\t/*** code-gen-seq ***/" nl
             (format "\t/* sequence: ~a */\n" pe)
             (apply string-append    
                    (map (lambda (x) (code-gen x depth)) exps))
             (format "\t/* end of sequence: ~a */\n\n" pe)
             )))
    ))


(define code-gen-applic
  (lambda (pe depth)
    (with pe
          (lambda (applic func params)
            (letrec ((Lend (^label-applic_end))
                     (Lnot_proc (^label-applic_not_proc))
                     (params-count (length params))
                     (code-gen-all-params 
                      (lambda (param-lst)
                        (if (null? param-lst)
                            ""
                            (string-append 
                             (code-gen-all-params (cdr param-lst))
                             (code-gen (car param-lst) depth)
                             "\tPUSH(R0);\t\t// push i's parameter for the applic call" nl
                             )
                            ))))
              (string-append
               "\t/*** code-gen-applic ***/" nl
               ;TODO: should be removed? (next line)
               "\tPUSH(IMM(" (sob_nil) "));\t\t// add magic-space for lambda variadic case" nl
               (code-gen-all-params params)
               "\t/* end of parameters preparing for application */"nl
               "\tPUSH(IMM(" (number->string params-count) "));\t\t// push n (number of parameters)" nl nl
               "\t/* gen code for the proc before application */" nl
               (code-gen func depth)
               "\t/* end of proc code, now R0 holds the proc object */" nl
               "\tCMP(INDD(R0, IMM(0)), " t_closure ");\t\t// check if its really a closure" nl
               "\tJUMP_NE(" Lnot_proc ");" nl
               "\tPUSH(INDD(R0, IMM(1)));\t\t// push the closure's env" nl
               "\tMOV(R0, INDD(R0, IMM(2)));" nl
               "\tCALLA(R0);\t\t// call the code of this pcocedure" nl
               
               "\tMOV(R1, STARG(0));\t\t// get the run time parameters count" nl
               "\tADD(R1, IMM(3));\t\t// DROP(3+params) : 3 for env + n + '()" nl
               "\tDROP(R1);" nl
               
               ;"\tDROP(" (number->string (+ 3 params-count)) ");\t\t// DROP(3+params) : 3 for env + n + '()" nl
               ;TODO: change above params-count to STARG(0/1) for tc-applic case (Tomer's notes)
               "\tJUMP(" Lend ");" nl
               Lnot_proc ":" nl
               "\tDROP(" (number->string (+ 2 params-count)) ");\t\t// DROP(2+params) : 2 for n + '()" nl
               (code-gen-err-msg "application expected closure") nl; TODO- need to implement
               Lend ":" nl
               "\t/* end of application */" nl nl
               ))
            ))
    ))

;sub-function of code-gen-lambda
(define code-create-b0
  (lambda (Largs Largs_end)
    (string-append
     "\tMOV(R0, FPARG(1));\t\t//get the argument count " nl ;TODO - check case of n=0 (malloc(0)??)
     "\tINCR(R0);\t\t//increment for magic space" nl
     "\tPUSH(R0);" nl
     "\tCALL(MALLOC);" nl
     "\tDROP(1);" nl
     "\tMOV(R1, R0);\t\t//now R1 holds the new empty array b0" nl
     
     "\tMOV(R2,FPARG(1));\t\t//R2 is the loop counter" nl
     "\tINCR(R2);\t\t//increment for magic-space" nl
     "\tDECR(R2);\t\t//and holds curr arg index (initially last arg)" nl
     "\t" Largs ":" nl
     "\tCMP(R2,IMM(0));" nl
     "\tJUMP_LT(" Largs_end ");" nl
     "\tMOV(R3,R2);" nl
     "\tADD(R3, IMM(2));\t\t//R3 points to curr arg in FPARG terms" nl
     "\tMOV(R3, FPARG(R3));" nl
     "\tMOV(INDD(R1, R2), R3);\t\t// b0[i] = arg[i] (= FPARG(i+2))" nl
     "\tDECR(R2);" nl
     "\tJUMP(" Largs ");" nl
     "\t" Largs_end ":" nl nl)
    ))

;sub-function of code-gen-lambda
(define code-create-new-env
  (lambda (env_depth Lenv Lenv_end)
    (string-append
     "\tPUSH(IMM(" env_depth "));\t\t//use to malloc the next env" nl 
     "\tCALL(MALLOC);" nl
     "\tDROP(1);" nl
     "\tMOV(R4,R0);\t\t//R4 holds new env" nl
     "\tMOV(INDD(R4, 0), R1);\t\t//insert newly created b0 into new env" nl
     
     "\tMOV(R1,FPARG(0));\t\t//R1 holds old env" nl
     "\tMOV(R2,IMM(0));\t\t//R2 will iterate old env" nl
     "\tMOV(R3,IMM(1));\t\t//R3 will iterate new env (starting from 1, for 0 holds b0)" nl
     
     "\t" Lenv ":" nl
     "\tCMP(R3,IMM(" env_depth "));" nl
     "\tJUMP_EQ(" Lenv_end ");" nl
     "\tMOV(INDD(R4,R3),INDD(R1,R2));" nl ;TODO: check if Mem-Mem operation is allowed
     "\tINCR(R2);" nl
     "\tINCR(R3);" nl
     "\tJUMP(" Lenv ");\t\t//R4 holds new fully copied env" nl
     
     "\t" Lenv_end ":" nl)
    ))

;sub-function of code-gen-lambda
(define code-create-closure
  (lambda (Lbody)
    (string-append
     "\tPUSH(LABEL(" Lbody "));\t\t// first argument for creating closure (code pointer)" nl
     "\tPUSH(R4);\t\t// second argument for creating closure (env pointer)" nl
     "\tCALL(MAKE_SOB_CLOSURE);\t\t// make the closure" nl
     "\tDROP(2);" nl)
    ))


(define code-gen-lambda-simple
  (lambda (pe depth)
    (with pe
          (lambda (lambda-simple args seq)
            (let ((Largs (^label-lambda_b0_copy))
                  (Largs_end (^label-lambda_b0_copy_end))
                  (Lenv (^label-lambda_env_copy))
                  (Lenv_end (^label-lambda_env_copy_end))
                  (Lbody (^label-lambda_body))
                  (Lbody_cont (^label-lambda_body_cont))
                  (Lend (^label-lambda_end))
                  (env_depth (number->string (add1 depth)))
                  (args-count (number->string (length args))))
              (string-append
               "\t/*** code-gen-lambda-simple ***/" nl
               
               (code-create-b0 Largs Largs_end ) ;R1 points at b0
               (code-create-new-env env_depth Lenv Lenv_end) ;R4 points at new env 
               (code-create-closure Lbody) ;R0 points at newly created closure
              
               "\tJUMP(" Lend ");" nl nl
               
               ;; make the lambda code
               "\t" Lbody ":\t\t// body code" nl
               "\tPUSH(FP);" nl
               "\tMOV(FP, SP);" nl
               
               ; check n is ok
               "\tMOV(R0,FPARG(1));" nl
               "\tCMP(R0, IMM(" args-count "));\t\t//check n==arg_count" nl
               "\tJUMP_EQ(" Lbody_cont ");" nl
               (code-gen-err-msg 
                (string-append "error: incorrect number of arguments for simple application, expected " 
                               args-count))
               
               Lbody_cont ":" nl
               (code-gen seq (add1 depth))
               
               "\tPOP(FP);" nl
               "\tRETURN;\t\t//that's where code of application returns" nl
               "\t" Lend ":\t\t//finished making the closure" nl
               "\t /* end of lambda */" nl nl
               ))
            ))
    ))

(define code-gen-lambda-opt
  (lambda (pe depth)
    (with pe
          (lambda (lambda-opt args opt seq)
            (let ((Largs (^label-lambda_b0_copy))
                  (Largs_end (^label-lambda_b0_copy_end))
                  (Lenv (^label-lambda_env_copy))
                  (Lenv_end (^label-lambda_env_copy_end))
                  (Lbody (^label-lambda_body))
                  (Lbody_cont (^label-lambda_body_cont))
                  (Lend (^label-lambda_end))
                  (Llist_insertion (^label-lambda_opt_list_insertion))
                  (Llist_insertion_end (^label-lambda_opt_list_insertion_end))
                  (env_depth (number->string (add1 depth)))
                  (args-count (number->string (length args))))
              
              (string-append
               "\t/*** code-gen-lambda-opt ***/" nl
               
               (code-create-b0 Largs Largs_end ) ;R1 points at b0
               (code-create-new-env env_depth Lenv Lenv_end) ;R4 points at new env 
               (code-create-closure Lbody) ;R0 points at newly created closure
              
               "\tJUMP(" Lend ");" nl nl
               
               ;; make the lambda code
               "\t" Lbody ":\t\t// body code" nl
               "\tPUSH(FP);" nl
               "\tMOV(FP, SP);" nl
               
               ; check n is ok
               "\tMOV(R0, FPARG(1));" nl
               "\tCMP(R0, IMM(" args-count "));\t\t//check n==arg_count" nl
               "\tJUMP_GE(" Lbody_cont ");" nl
               (code-gen-err-msg 
                (string-append "error: incorrect number of arguments for opt application, expected " 
                               args-count))
               "\t" Lbody_cont ":" nl
               
               ;insert optional args into list
               "\t/* insert optional args into list */" nl
               "\tMOV(R1,FPARG(1));\t\t//R1 holds real count of pushed args" nl
               "\tMOV(R2,R1);" nl
               "\tINCR(R2);\t\t//R2 holds index for FPARG to point at arg being copied (initially last arg)" nl
               "\tSUB(R1," args-count ");\t\t//R1 holds loop counter" nl
               ; TODO - need to find way to make it more elegant
               "\tMOV(R3, IMM(" (sob_nil) "));\t\t//R3 holds 'List so far'" nl
               "\t" Llist_insertion ":" nl
               "\tCMP(R1,IMM(0));" nl
               "\tJUMP_EQ(" Llist_insertion_end ");" nl
               "\tPUSH(R3);" nl
               "\tPUSH(FPARG(R2));" nl
               "\tCALL(MAKE_SOB_PAIR);" nl
               "\tDROP(IMM(2));" nl
               "\tMOV(R3, R0);\t\t//R3 holds updated 'rest of list'" nl
               "\tDECR(R1);" nl
               "\tDECR(R2);" nl
               "\tJUMP(" Llist_insertion ");\t\t//now R3 holds list of optional args" nl
               "\t" Llist_insertion_end ":" nl
               "\tINCR(R2);\t\t//R2 holds index of first optional arg" nl
               "\tMOV(FPARG(R2),R3);\t\t//FPARG(R2) gets list of optional args" nl
               
               (code-gen seq (add1 depth))
               
               "\tPOP(FP);" nl
               "\tRETURN;\t\t//that's where code of application returns" nl
               "\t" Lend ":\t\t//finished making the closure" nl
               "\t /* end of lambda */" nl nl
               ))
            ))
    ))

(define code-gen-lambda-variadic
  (lambda (pe depth)
    (with pe
          (lambda (lambda-var arg seq)
            (let ((Largs (^label-lambda_b0_copy))
                  (Largs_end (^label-lambda_b0_copy_end))
                  (Lenv (^label-lambda_env_copy))
                  (Lenv_end (^label-lambda_env_copy_end))
                  (Lbody (^label-lambda_body))
                  (Lend (^label-lambda_end))
                  (Llist_insertion (^label-lambda_var_list_insertion))
                  (Llist_insertion_end (^label-lambda_var_list_insertion_end))
                  (env_depth (number->string (add1 depth))))
              
              (string-append
               "\t/*** code-gen-lambda-simple ***/" nl
               
               (code-create-b0 Largs Largs_end ) ;R1 points at b0
               (code-create-new-env env_depth Lenv Lenv_end) ;R4 points at new env 
               (code-create-closure Lbody) ;R0 points at newly created closure
              
               "\tJUMP(" Lend ");" nl nl
               
               ;; make the lambda code
               "\t" Lbody ":\t\t// body code" nl
               "\tPUSH(FP);" nl
               "\tMOV(FP, SP);" nl
               
               ;insert args into list
               "\t/* insert args into list */" nl
               "\tMOV(R1,FPARG(1));\t\t//R1 holds real count of pushed args = loop counter" nl
               "\tMOV(R2,R1);" nl
               "\tINCR(R2);\t\t//R2 holds index for FPARG to point at arg being copied (initially last arg)" nl
               "\tMOV(R3,IMM(" (sob_nil) "));\t\t//R3 holds 'List so far'" nl
               "\t" Llist_insertion ":" nl
               "\tCMP(R1,IMM(0));" nl
               "\tJUMP_EQ(" Llist_insertion_end ");" nl
               "\tPUSH(R3);" nl
               "\tPUSH(FPARG(R2));" nl
               "\tCALL(MAKE_SOB_PAIR);" nl
               "\tDROP(IMM(2));" nl
               "\tMOV(R3, R0);\t\t//R3 holds updated 'List so far'" nl
               "\tDECR(R1);" nl
               "\tDECR(R2);" nl
               "\tJUMP(" Llist_insertion ");\t\t//now R3 holds list of args" nl
               "\t" Llist_insertion_end ":" nl
               "\tINCR(R2);\t\t//R2 holds index of first (and only) arg" nl
               "\tMOV(FPARG(R2),R3);\t\t//FPARG(R2) gets list of optional args" nl
               
               (code-gen seq (add1 depth))
               
               "\tPOP(FP);" nl
               "\tRETURN;\t\t//that's where code of application returns" nl
               "\t" Lend ":\t\t//finished making the closure" nl
               "\t /* end of lambda */" nl nl
               ))
            ))
    ))


(define code-gen
  (lambda (pe depth)
    (with pe
          (lambda (first . rest)
            (cond ((equal? first 'pvar) (code-gen-pvar pe depth)) ; TODO 
                  ((equal? first 'bvar) (code-gen-bvar pe depth)) ; TODO 
                  ((equal? first 'fvar) (code-gen-fvar pe depth)) ; TODO - shuld be replaced by some lockup at the 'define' table
                  ((equal? first 'seq) (code-gen-seq pe depth)) ; TODO: check about optimization
                  ((equal? first 'or) (code-gen-or pe depth))
                  ((equal? first 'lambda-simple) (code-gen-lambda-simple pe depth))
                  ((equal? first 'lambda-opt) (code-gen-lambda-opt pe depth))
                  ((equal? first 'lambda-variadic) (code-gen-lambda-variadic pe depth))
                  ((equal? first 'applic) (code-gen-applic pe depth))
                  ((equal? first 'tc-applic) (code-gen-tc-applic pe depth)) ; TODO - need to implement
                  ((equal? first 'const) (code-gen-const pe depth)) ; TODO - replace by the real const table implementation
                  ((equal? first 'if3) (code-gen-if3 pe depth))
                  ((equal? first 'define) (code-gen-define pe depth))
                  (else (error 'code-gen (format "unknown parsed expr: ~a" pe)))
                  ))
          )))

;; code-print-result
(define code-print-result
  (lambda ()
    (let ((Lend (^label-print_result_end)))
      (string-append
       nl nl
       "\t/****** end of scheme expression - print return value ******/" nl
       "\tMOV(R1, R0);" nl
       "\tPUSH(R0);" nl
       "\tCALL(IS_SOB_VOID);" nl
       "\tDROP(1);" nl ; TODO - can be reused to write
       "\tCMP(R0, IMM(1));\t\t// if result==<void> do not print" nl
       "\tJUMP_EQ(" Lend ");" nl
       "\tPUSH(R1);\t\t// the result" nl
       "\tCALL(WRITE_SOB);" nl
       "\tDROP(1);" nl
       
       "\tPUSH(IMM(10));" nl
       "\tCALL(PUTCHAR);" nl
       "\tDROP(1);" nl
       
       "\t" Lend ":" nl 
       nl nl
       ))
    ))


;; this func reads scheme code from src file and writes compiled code to dst file
(define compile
  (lambda (in out)
    (letrec ((in-port (open-input-file in))
             (lib-in-port (open-input-file "compiler-lib.scm"))
             (out-port (open-output-file out 'replace))
             
             #| TODO: delete
             (commit (lambda()                       
                       (let ((curr-read (read in-port)))
                         (if (not (eof-object? curr-read))
                             (begin (display (code-gen (fully-parse curr-read) 0) out-port)
                                    (display (code-print-result) out-port)
                                    (commit))))))
|#

             ; read all input file and return as list of sexprs 
             (reader (lambda (in-port)
                        (let ((curr-read (read in-port)))
                          (if (eof-object? curr-read)
                              `()
                              `(,@(reader in-port) ,curr-read)))
                        ))
             ; code-gen sexpr
             (commit (lambda (pe)
                       (begin (display (code-gen pe 0) out-port)
                              (display (code-print-result) out-port)))))
      (let* ((pe-lst (map fully-parse (append (reader in-port) (reader lib-in-port)))) ; TODO: do not rely on map evaluation order 
             (local-const-tbl (make-const-tbl pe-lst 100))
             (local-fvars-tbl (make-fvars-tbl pe-lst 100000)))
        
        ; print prologue
        (set! const-tbl local-const-tbl)
        (set! fvars-tbl local-fvars-tbl)
        (insert-prologue out-port)
        (display (code-gen-const-tbl const-tbl) out-port)
        (display (code-gen-lib-funcs-tbl fvars-tbl) out-port)
        (display (code-gen-sym-tbl (make-sym-tbl const-tbl)) out-port)
        ;(commit) ;; TODO: delete
        ; print program code
        (map commit pe-lst)
        
        ;print epilogue
        (insert-termination out-port)
        (close-input-port in-port)
        (close-output-port out-port))
      )))
  
(define compile-scheme-file compile) ;; TODO - choose the right name








