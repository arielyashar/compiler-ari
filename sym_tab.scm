(define make-sym-tbl
  (lambda (const-tbl)
    (if (null? const-tbl)
        '()
        (let ((first (car const-tbl)))
          (if (symbol? (cadr first))
              (cons (cons (cadr first) (car (cdaddr first))) (make-sym-tbl (cdr const-tbl)))
              (make-sym-tbl (cdr const-tbl)))
          ))
    ))
;note: (cadr first) = the symbol itself (sexpr)
;      (car (cdaddr first))) = address of representative string

(define code-gen-sym-tbl
  (lambda(sym-tbl)
    (let* ((inner-code-gen 
            (lambda (sym)
              (string-append
               "\t" (format "/* generating sym ~a */" (car sym)) nl
               "\tPUSH(IMM(2));" nl
               "\tCALL(MALLOC);" nl
               "\tDROP(1);" nl
               "\tMOV(IND(R0)," (number->string (cdr sym)) ");" nl
               "\tMOV(INDD(R0,1),IMM(0));" nl
               "\tMOV(IND(R1),R0);" nl
               "\tMOV(R1,R0);" nl
               "\tINCR(R1);\t\t// R1 again holds addr of list's last 'next'" nl
              ))
            ))
      (string-append
       nl nl
       "\t/*** code-gen-sym-tbl ***/" nl
       "\tMOV(IND(1), IMM(0));\t\t// initialize addr 1 to hold pointer to symbols linked list" nl
       "\tMOV(R1, IMM(1));\t\t// R1 holds address of list's last 'next'" nl
       (apply string-append (map inner-code-gen sym-tbl))
      ))
    ))
        