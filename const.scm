 ; utils ;;
(define (sob_void)
  (number->string (find-const-addr (void) const-tbl)))
(define (sob_nil)
  (number->string (find-const-addr '() const-tbl)))
(define (sob_true)
  (number->string (find-const-addr #t const-tbl)))
(define (sob_false)
  (number->string (find-const-addr #f const-tbl)))



;; identifiers ;;
;;;;;;;;;;;;;;;;;

(define void?
  (lambda (exp)
    (equal? (void) exp)
    ))

; hendeling  constants:
;;;;;;;;;;;;;;;;;;;;;;;;

(define find-all-sub-const
  (lambda (exp)
    (cond
      ((or (number? exp) (string? exp) (void? exp) (boolean? exp) (null? exp) (char? exp)) 
       `(,exp))
      ((pair? exp) 
       `(,@(find-all-sub-const (car exp)) ,@(find-all-sub-const (cdr exp)) ,exp))
      ((vector? exp)
       `(,@(apply append (map find-all-sub-const
                              (vector->list exp))) ,exp))
      ((symbol? exp)
       `(,@(find-all-sub-const (symbol->string exp)) ,exp))
      (else
       (error 'find-all-sub-const (format "undefine constant type for: ~a " exp)))
      )
    ))


(define filter-duplicate-const
  (lambda (lst)
    (if (null? lst)
        '()
        (let* ((c (car lst))
               (filter-first (remove c lst)))
          (cons c (filter-duplicate-const filter-first))
          ))
    ))


;return the memory representation of the constant and replace primitives by their value
(define get-c-elements
  (lambda (exp)
    (cond
      ((void? exp) `(,t_void))
      ((null? exp) `(,t_nill))
      ((boolean? exp) `(,t_bool ,(if (equal? #t exp) 1 0)))
      ((number? exp) `(,t_integer ,exp))
      ((char? exp) `(,t_char ,(char->integer exp)))
      ((string? exp) `(,t_string ,(string-length exp) ,@(map char->integer (string->list exp)) 0))
      ((pair? exp) `(,t_pair ,(car exp) ,(cdr exp)))
      ((vector? exp) `(,t_vector ,(vector-length exp) . ,(vector->list exp)))
      ((symbol? exp) `(,t_symbol ,(symbol->string exp)))
      (else
       (error 'get-c-elements (format "undefine constant type for: ~a " exp))))
    ))

; return the address of the constant
(define find-const-addr
  (lambda (c c-lst)
    (if (null? c-lst)
        (error 'find-const-addr (format "the constant: ~a is not in constant-tbl" c))
        (if (equal? c (cadar c-lst))
            (caar c-lst)
            (find-const-addr c (cdr c-lst)))
        )
    ))

; add reference to the sub constants for non primitive constants
(define add-const-tbl-references
  (lambda (const-tbl)
    (let ((search-proc (lambda (c) (find-const-addr c const-tbl))))
      (map 
       (lambda (c-row)
         (let ((c (cadr c-row))
               (elements (cdaddr c-row)))
           `(,(car c-row) 
             ,c
             (,(caaddr c-row)
              . ,(cond
                   ((or (void? c) (null? c) (number? c) (boolean? c) (char? c)) elements)
                   ((string? c) elements)
                   ((pair? c) `(,(search-proc (car elements)) ,(search-proc (cadr elements))))
                   ((vector? c) `(,(car elements) . ,(map search-proc (cdr elements)))) ; vector has also length
                   ((symbol? c) (map search-proc elements))
                   (else
                    (error 'add-const-tbl-references (format "undefine constant type for: ~a " c))))))
           ))
       const-tbl))
    ))


; extrct all the constants from list of parsed expresions
; pe-lst: list of parsed expresions
; c-lst: list of constants (start with '())
(define make-const-lst
  (lambda (pe-lst c-lst)
    (if (null? pe-lst)
        c-lst
        (make-const-lst 
         (cdr pe-lst)
         (with (car pe-lst)
               (lambda (first . rest)
                 (cond ((or (equal? first 'pvar) (equal? first 'bvar) (equal? first 'fvar)) c-lst)
                       ((equal? first 'seq) (make-const-lst (car rest) c-lst))
                       ((equal? first 'or) (make-const-lst (car rest) c-lst))
                       ((equal? first 'lambda-simple) (make-const-lst (cdr rest) c-lst))
                       ((equal? first 'lambda-opt) (make-const-lst (cddr rest) c-lst))
                       ((equal? first 'lambda-variadic) (make-const-lst (cdr rest) c-lst))
                       ((or (equal? first 'applic) (equal? first 'tc-applic)) 
                        `(,@(make-const-lst `(,(car rest)) '()) . ,(make-const-lst (cadr rest) c-lst)))
                       ((equal? first 'const) `(,@rest . ,c-lst)) ; here it's happen
                       ((equal? first 'if3) (make-const-lst  rest c-lst))
                       ((equal? first 'define) (make-const-lst  (cdr rest) c-lst)) ; search only in the value of define
                       (else (error 'make-const-lst (format "unknown parsed expr: ~a" (car pe-lst))))
                       ))))
        )))

#|
(define code-gen-const-tbl2 ;;
  (lambda (const-tbl)
    (let* ((first-addr (caar const-tbl)) ; the address of the first constant cell
           (cell-lst (apply append (map caddr const-tbl)))) ; extract only the data cells lists from the table
      (letrec ((run
                (lambda(start-addr cell-lst counter)
                  (if (null? cell-lst)
                      ""
                      (string-append
                       (format "\tMOV(INDD(~a,IMM(~a)),~a);" start-addr counter (car cell-lst)) nl
                       (run start-addr (cdr cell-lst) (add1 counter))))
                  )))
        (string-append
         "\t /* add the basic constants at addreses " (number->string first-addr) " to 1000*/" nl
        (run first-addr cell-lst 0))))
    ))
|#

(define code-gen-const-tbl
  (lambda (const-tbl)
    (let* ((first-addr (caar const-tbl)) ; the address of the first constant cell
           (cell-lst (map (lambda (x) (if (number? x) (number->string x) x)) ; convert numbers to string
                          (apply append ; append all sub cell lists to one long list
                                 (map caddr const-tbl)))) ; extract only the data cells lists from the table
           (with-comma (cons (car cell-lst) ; the first cell do not need comma
                             (map (lambda (cell) (string-append " , " cell)) ; add comma to each cell
                                  (cdr cell-lst))))
           (string-cell-lst (apply string-append with-comma)) ; make a long string from it
           (size (length cell-lst)))
      (string-append       
       "\t /* add the basic constants at addreses " (number->string first-addr) " to 1000*/" nl
       
       "\tlong mem_init[] = { " string-cell-lst " };" nl
       ; copying code
       (format "\tint i = ~a , j = 0 ;\n" first-addr)
       (format "\tfor(i = ~a , j = 0 ; i < ~a ; i++ , j++ ){ \n" first-addr (+ first-addr size))
       "\t\tMOV(IND(i) , IMM(mem_init[j])); \n"
       "\t}\n\n"
       ))
    ))

; get: list of parsed expressions and first constant address
; return: the constant table
(define make-const-tbl
  (lambda (pe-lst addr)
    (letrec ((run (lambda (c-lst addr)
                    (if (null? c-lst)
                        '()
                        (let* ((c (car c-lst))
                               (c_elements (get-c-elements c))
                               (c_size (length c_elements)))
                          `(  
                            (,addr ,c ,c_elements) 
                            . ,(run (cdr c-lst) (+ addr c_size)))
                          )))
                  ))
      (add-const-tbl-references
       (run
        (filter-duplicate-const
         (append `(,(void) () #t #f) ; add by default the 4 basic constants
                 (apply append
                        (map find-all-sub-const
                             (make-const-lst pe-lst '())))))
        addr))
      )
    ))


;; generate code for const reference
(define code-gen-const
  (lambda (pe depth)
    (with pe
          (lambda (const val)
            (if (equal? const 'const)
                (let ((c-addr (find-const-addr val const-tbl)))
                  (format "\tMOV(R0,IMM( ~a ));\n" c-addr))
                (error 'code-gen-const (format "~a is not aconstant" pe)))
            ))
    ))





