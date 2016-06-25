;;; librery functions implemented in scheme,          ;;;
;;; will be append to each code, and compile with him ;;;

;; enclosing functions for cisc functions 
;; (overide the basic functions and add paramerest cheking )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;*** auxileries

; void
(define (void) (if #f #f))

;display
(define display (let ((display display)
                      (*void* (void)))
                  (lambda (obj)
                    (display obj)
                    *void*
                    )))
;length
(define length
  (lambda (lst)
    (if (null? lst) 0
        (+ 1 (length (cdr lst))))
    ))

;not
(define not
  (lambda (x)
    (if x #f #t)))

; error
(define error
  (lambda (func msg)
    ;TODO - will be replaced by format
    (display "Exception in ")
    (display func)
    (display ": ")
    (display msg)
    (display "\n")
    ;(exit) TODO - uncomment before submitting
    ))



;;*** predicates

(define boolean? 
  (let ((cisc-boolean? boolean?))
    (lambda (sob) (cisc-boolean? sob))))
(define symbol? 
  (let ((cisc-symbol? symbol?))
    (lambda (sob) (cisc-symbol? sob))))
(define procedure? 
  (let ((cisc-procedure? procedure?))
    (lambda (sob) (cisc-procedure? sob))))
(define integer? 
  (let ((cisc-integer? integer?))
    (lambda (sob) (cisc-integer? sob))))
(define char? 
  (let ((cisc-char? char?))
    (lambda (sob) (cisc-char? sob))))
(define vector? 
  (let ((cisc-vector? vector?))
    (lambda (sob) (cisc-vector? sob))))
(define string? 
  (let ((cisc-string? string?))
    (lambda (sob) (cisc-string? sob))))
(define pair? 
  (let ((cisc-pair? pair?))
    (lambda (sob) (cisc-pair? sob))))
(define void? 
  (let ((cisc-void? void?))
    (lambda (sob) (cisc-void? sob))))
(define null? 
  (let ((cisc-null? null?))
    (lambda (sob) (cisc-null? sob))))

;;apply , TODO - make it variadic function
(define apply 
  (let ((apply apply))
    (lambda (func  lst)
      (if (procedure? func)
          (if (list? lst)
              (apply func lst)
              (error "apply" "expected list as argument no.2"))
          (error "apply" "expected closure as argument no.1")))))

;;*** pairs :

; cons
(define cons 
  (let ((cisc-cons cons))
          (lambda (a b) (cisc-cons a b))))

; car
(define car (let ((cisc-car car))
              (lambda (pair) (if (pair? pair)
                                 (cisc-car pair)
                                 (error 'car "expected pair")))
              ))

; cdr
(define cdr (let ((cisc-cdr cdr))
              (lambda (pair) (if (pair? pair)
                                 (cisc-cdr pair)
                                 (error 'cdr "expected pair")))
              ))
;set-cdr!
(define set-cdr!
  (let ((cisc-set-cdr! set-cdr!))
    (lambda (pair the-cdr)
      (cond ((not (pair? pair)) (error 'set-cdr! "expected pair as argument no.1"))
            (else (cisc-set-cdr! pair the-cdr))))
    ))

;set-car!
(define set-car!
  (let ((cisc-set-car! set-car!))
    (lambda (pair the-car)
      (cond ((not (pair? pair)) (error 'set-car! "expected pair as argument no.1"))
            (else (cisc-set-car! pair the-car))))
    ))

;;*** list

;list
(define list
  (lambda lst lst))

;list?
(define (list? lst)
  (if (null? lst)
      #t
      (if (pair? lst)
          (list? (cdr lst))
          #f)
      ))

;append
(define (append l m)
 (if (null? l) m
  (cons (car l) (append (cdr l) m))))

;map
(define map 
  (lambda (func lst)
    (if (null? lst)
        '()
        (cons (func (car lst)) (map func (cdr lst))))
    ))

;andmap
(define andmap
  (lambda (func lst)
    (if (null? lst) #t
        (and (func (car lst)) (andmap func (cdr lst))))
    ))

;;*** arithmetics

;auxileries

(define number? integer?)

(define num-error
  (lambda (func) 
    (error func "expected all arguments to be numbers")))

;+ 
(define +
  (let ((cisc-+ +))
    (lambda num-lst
      (cond ((not (andmap number? num-lst)) (num-error '+))
            (else (apply cisc-+ num-lst))))
    ))

;-
(define -
  (let ((cisc-minus -))
    (lambda num-lst
      (cond ((not (andmap number? num-lst)) (num-error '-))
            ((< (length num-lst) 1) (error '- "expected at least 1 argument"))
            (else (apply cisc-minus num-lst))))
    ))

;/
(define /
  (let ((cisc-/ /))
    (lambda num-lst
      (cond ((not (andmap number? num-lst)) (num-error '/))            
            ((< (length num-lst) 1) (error '/ "expected at least 1 argument"))
            ((not (andmap (lambda (x) (not (zero? x))) (cdr num-lst))) (error '/ "division by zero"))
            ((and (= (length num-lst) 1) (zero? (car num-lst))) (error '/ "division by zero"))
            (else (apply cisc-/ num-lst))))
    ))

;*
(define *
  (let ((cisc-* *))
    (lambda num-lst
      (cond ((not (andmap number? num-lst)) (num-error '*))
            (else (apply cisc-* num-lst))))
    ))

;<
(define <
  (let ((cisc-< <))
    (lambda num-lst
      (cond ((not (andmap number? num-lst)) (num-error '<))
            (else (apply cisc-< num-lst))))
    ))

;>                      
(define >
  (let ((cisc-> >))
    (lambda num-lst
      (cond ((not (andmap number? num-lst)) (num-error '>))
            (else (apply cisc-> num-lst))))
    ))

;=
(define =
  (let ((cisc-= =))
    (lambda num-lst
      (cond ((not (andmap number? num-lst)) (num-error '=))
            (else (apply cisc-= num-lst))))
    ))

;zero?
(define zero?
  (lambda (x)
    (and (number? x) (= 0 x))
    ))

;remainder
(define remainder
  (lambda (mone mechane)
    (cond ((not (integer? mone)) (error 'remainder "expected integer as argument no.1"))
          ((not (integer? mechane)) (error 'remainder "expected integer as argument no.2"))
          (else
           (let* ((quotient (/ mone mechane))
                  (rem (- mone (* quotient mechane))))
             (if (> mone 0)
                 (if (> rem 0) rem (- rem))
                 (if (< rem 0) rem (- rem))))
           ))
    ))

;;*** string

;string-length
(define string-length
  (let ((cisc-sob-length sob-length))
    (lambda (str)
      (cond ((not (string? str)) (error 'string-length "expected string as argument no.1"))
            (else (cisc-sob-length str))))
    ))

;string-set!
(define string-set!
  (let ((cisc-string-set! string-set!))
    (lambda (str index char)
      (cond ((not (string? str)) (error 'string-set! "expected string as argument no.1"))
            ((not (integer? index)) (error 'string-set! "expected integer as argument no.2"))
            ((not (char? char)) (error 'string-set! "expected char as argument no.3"))
            ((or (< index 0) (>= index (string-length str))) (error 'string-set! "invalid index"))
            (else (cisc-string-set! str index char))))
    ))

;string-ref
(define string-ref
  (let ((cisc-string-ref string-ref))
    (lambda (str index)
      (cond ((not (string? str)) (error 'string-ref "expected string as argument no.1"))
            ((not (integer? index)) (error 'string-ref "expected integer as argument no.2"))
            ((or (< index 0) (>= index (string-length str))) (error 'string-ref "invalid index"))
            (else (cisc-string-ref str index))))
    ))

;make-string
(define make-string
  (lambda (num . chars)
    (cond ((not (integer? num)) (error 'make-string "expected integer as argument no.1"))
          ((< num 0) (error 'make-string "invalid multiplier"))
          ((> (length chars) 1) (error 'make-string (format "expected 2 arguments, given ~a" (+ 1 (length chars)))))
          (else
           (letrec ((make-list (lambda (char num)
                                 (if (= 0 num) '()
                                     (cons char (make-list char (- num 1)))))))
             (if (null? chars)
                 (list->string (make-list (integer->char 0) num))
                 (if (not (char? (car chars)))
                     (error 'make-string "expected char as argument no.2")
                     (list->string (make-list (car chars) num)))))))
    ))

;symbol->string
(define symbol->string
  (let ((cisc-symbol->string symbol->string))
    (lambda (sym)
      (cond ((not (symbol? sym)) (error 'symbol->string "expected symbol as argument no.1"))
            (else (cisc-symbol->string sym))))
    ))

;string->symbol
(define string->symbol
  (let ((cisc-string->symbol string->symbol))
    (lambda (str)
      (cond ((not (string? str)) (error 'string->symbol "expected string as argument no.1"))
            (else (cisc-string->symbol str))))
    ))

;list->string
(define list->string
  (let ((cisc-list->string list->string))
    (lambda (lst)
      (cond ((not (list? lst)) (error 'list->string "expected list as argument no.1"))
            ((not (map char? lst)) (error 'list->string "one of the elements is not propper char"))
            (else (cisc-list->string lst))))
    ))

#|
; TODO - try to move Ym to top and the we can apply the letrec befor the lambda
(define make-string
  (lambda (num char)
    (cond ((not (integer? num)) (error 'make-string "expected integer as argument no.1"))
          ((< num 0) (error 'make-string "invalid index"))
          ((not (char? char)) (error 'make-string "expected char as argument no.2"))
          (else (letrec ((make-list (lambda (n chr rest)
                                      (if (= n 0)
                                          rest
                                          (make-list (- n 1) chr (cons chr rest))))))
                  (list->string (make-list num char '()))))
          )))

|#

;;;;(integer->char 1114111)


;;*** vector

;vector-length
(define vector-length
  (let ((cisc-sob-length sob-length))
    (lambda (vec)
      (cond ((not (vector? vec)) (error 'vector-length "expected vector as argument no.1"))
            (else (cisc-sob-length vec))))
    ))

;vector-ref
(define vector-ref
  (let ((cisc-vector-ref vector-ref))
      (lambda (vec index)
        (cond ((not (vector? vec)) (error 'vector-ref "expected vector as argument no.1"))
              ((not (integer? index)) (error 'vector-ref "expected integer as argument no.2"))
              ((or (< index 0) (>= index (vector-length vec))) (error 'vector-ref "invalid index"))
              (else (cisc-vector-ref vec index))))
    ))

;vector-set!
(define vector-set!
  (let ((cisc-vector-set vector-set!))
    (lambda (vec index obj)
      (cond ((not (vector? vec)) (error 'vector-set! "expected vector as argument no.1"))
            ((not (integer? index)) (error 'vector-set! "expected integer as argument no.2"))
            ((or (< index 0) (>= index (vector-length vec))) (error 'vector-ref "invalid index"))
            (else (cisc-vector-set vec index obj))))
    ))



;;*** from mayer

; Ycombinator
(define Ym
  (lambda fs
    (let ((ms (map
		(lambda (fi)
		  (lambda ms
		    (apply fi (map (lambda (mi)
				     (lambda args
				       (apply (apply mi ms) args))) ms))))
		fs)))
      (apply (car ms) ms))))

;; from support-code.scm
(define foldr
  (lambda (binop final s)
    (letrec ((loop
	      (lambda (s)
		(if (null? s) final
		    (binop (car s) (loop (cdr s)))))))
      (loop s))))

(define compose
  (let ((binary-compose
	 (lambda (f g)
	   (lambda (x)
	     (f (g x))))))
    (lambda s
      (foldr binary-compose (lambda (x) x) s))))

(define caar (compose car car))
(define cadr (compose car cdr))
(define cdar (compose cdr car))
(define cddr (compose cdr cdr))
(define caaar (compose car caar))
(define caadr (compose car cadr))
(define cadar (compose car cdar))
(define caddr (compose car cddr))
(define cdaar (compose cdr caar))
(define cdadr (compose cdr cadr))
(define cddar (compose cdr cdar))
(define cdddr (compose cdr cddr))
(define caaaar (compose car caaar))
(define caaadr (compose car caadr))
(define caadar (compose car cadar))
(define caaddr (compose car caddr))
(define cadaar (compose car cdaar))
(define cadadr (compose car cdadr))
(define caddar (compose car cddar))
(define cadddr (compose car cdddr))
(define cdaaar (compose cdr caaar))
(define cdaadr (compose cdr caadr))
(define cdadar (compose cdr cadar))
(define cdaddr (compose cdr caddr))
(define cddaar (compose cdr cdaar))
(define cddadr (compose cdr cdadr))
(define cdddar (compose cdr cddar))
(define cddddr (compose cdr cdddr))

;;; You need to have the following binary boolean predicates:
;;; bin<?
;;; bin=?

(define bin<? <)
(define bin=? =)

(define bin>? (lambda (a b) (bin<? b a)))
(define bin<=? (lambda (a b) (not (bin>? a b))))
(define bin>=? (lambda (a b) (not (bin<? a b))))

(define order
  (lambda (<)
    (letrec ((loop
	      (lambda (a s)
		(or (null? s)
		    (and (< a (car s))
			 (loop (car s) (cdr s)))))))
      (lambda (a . s)
	(loop a s)))))

(define < (order bin<?))
(define <= (order bin<=?))
(define > (order bin>?))
(define >= (order bin>=?))








