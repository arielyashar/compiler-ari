

(define code-gen-tc-applic
  (lambda (pe depth)
    (with pe
          (lambda (applic func params)
            (letrec ((Lend (^label-applic_end))
                     (Lnot_proc (^label-applic_not_proc))
                     (Lframe_copy (^label-applic_frame_copy)) ; TODO - new in tc
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
               "\t/*** code-gen-tc-applic ***/" nl
               ;(format "printf(\"start tc-applic for: ~a \\n\");" pe) nl
               "\tPUSH(IMM(" (sob_nil) "));\t\t// add space for lambda variadic case" nl
               ;"printf(\"befor params SP=%ld ending with: %ld \\n\" , SP , STACK(SP - 1));" nl
               (code-gen-all-params params)
               ;"printf(\"after params SP=%ld ending with: %ld,\" , SP , STACK(SP - 1));" nl
               "\t/* end of parameters preparing for application */"nl
               "\tPUSH(IMM(" (number->string params-count) "));\t\t// push n (number of parametters)" nl nl
               ;"printf(\"n=%ld \\n\" , STACK(SP - 1));" nl
               "\t/* gen code for the proc before application */" nl
               ;"printf(\"befor func-gen SP=%ld ending with: %ld\\n\" , SP , STACK(SP - 1));" nl
               (code-gen func depth)
               ;"printf(\"after func-gen SP=%ld ending with: %ld\" , SP , STACK(SP - 1));" nl
               "\t/* end of proc code, now R0 holds the proc object */" nl
               "\tCMP(INDD(R0, IMM(0)), " t_closure ");\t\t// check if its really a closure" nl
               "\tJUMP_NE(" Lnot_proc ");" nl
               "\tPUSH(INDD(R0, IMM(1)));\t\t// push the closure's env" nl
               "\tMOV(R0, INDD(R0, IMM(2)));" nl
               
               ;; tc aditional code:
               "\tPUSH(FPARG(-1));\t\t// push the current frame's return address" nl
               ;TODO "\tMOV(FP, FPARG(-2));\t\t// Restore old FP in preparation of JUMP" nl
               "\t/* loop that overide the current frame */" nl
               "\tMOV(R1, SP);\t\t// end copy 'from' here" nl
               "\tMOV(R2, SP);" nl
               "\tSUB(R2,IMM(" (number->string (+ 4 params-count)) "));\t\t// start copy 'from' here" nl
               
               "\tMOV(R3, FP);\t\t// start override the frame from here" nl
               "\tSUB(R3, IMM(5));" nl
               "\tSUB(R3, FPARG(1));" nl
               "\tMOV(FP, FPARG(-2));\t\t// Restore old FP in preparation of JUMP" nl
               ;"\tprintf(\" fparg(1) = %ld \\n\", FPARG(1));"nl             
               ;"printf(\"befor starting to copy SP=%ld ending with: %ld\\n\" , SP , STACK(SP - 1));" nl
               Lframe_copy ":" nl
               
               ; TODO "\t\tPUSH(STACK(R2));" nl
               "\t\tMOV(STACK(R3), STACK(R2));" nl ; TODO - it is legal opcode?
               "\t\tINCR(R3);" nl
               "\t\tINCR(R2);" nl
               ;"\t}" nl nl
               
               "\tCMP(R1, R2);" nl
               "\tJUMP_NE(" Lframe_copy ");" nl
               
    
               "\tMOV(SP, R3);" nl
               "\t/* end of loop that overrides the current frame */" nl
               ;"printf(\"after copping SP=%ld ending with: %ld\\n\" , SP , STACK(SP - 1));" nl
               
               
               ;TODO - canged to JUMP for tc: "\tCALLA(R0);\t\t// call the code of this pcocedure" nl
               "\tJUMPA(R0);\t\t// call the code of this pcocedure" nl
               
               ;; tc additional code end
               
               ;TODO - from here most of the rest are dead code
               ;TODO - not needed for tc: "\tDROP(" (number->string (+ 3 params-count)) ");\t\t// DROP(3+params) : 3 for env + n + '()" nl
               ;TODO: change above params-count to STARG(0/1) for tc-applic case (Tomer's notes)
               "\tprintf(\"should not reach here\");"
               "\tJUMP(" Lend ");" nl
               Lnot_proc ":" nl
               "\tDROP(" (number->string (+ 2 params-count)) ");\t\t// DROP(2+params) : 2 for n + '()" nl
               (code-gen-err-msg "application expected closure") nl; TODO- need to implement
               Lend ":" nl
               "\t/* end of tc application */" nl nl
               ))
            ))
    ))


