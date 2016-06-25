(load "pc.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Padding functions ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define add-padding 
  (lambda (left var right)
    (format 
     (string-append "~" (number->string left) "a")
     (format (string-append "~" (number->string right) "@a")
             var))))

;; make the cut if overflow and add the overflow symbols
(define cut-padding 
  (lambda (var left right)
    (let* ((whith (string-length var))
           (ped-size (cond ((= right 0) (- whith left)) ; <- 
                           ((= left 0) (- whith right)) ; ->
                           (else (/ (- whith left) 2))))
           (var-lst (string->list var))
           (new-var-lst (split-list
                         (if (= left 0) var-lst 
                             (split-list var-lst 
                                         (- whith (ceiling ped-size) 1)
                                         (lambda (a b) (append a `(,(integer->char 9758))))))
                         (if (= right 0) 0 (+ (floor ped-size) 1)) 
                             (lambda (a b) (if (= right 0)
                                               b
                                               (append `(,(integer->char 9756)) b)))))
           )
      (list->string 
       (if (and (null? (cdr new-var-lst)) (> left 0) (> right 0))
          `(,(integer->char 9757))
          new-var-lst))
       )))

;; the main padding handle function
(define dir-padding
  (lambda (var left right) ; (s n n)
    (let* ((pad (max left right))
           (new-var (if (> (string-length var) pad)
                        (cut-padding var left right)
                        var))
           (whith (string-length new-var))
           (new-right (cond ((= right 0) 0) ; <- 
                            ((= left 0) right) ; ->
                            (else (+ whith (floor (/ (- right whith) 2)))))) ; <->
           )
      (add-padding left new-var new-right)
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parsing functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


;;meta-chars;;

(define <meta-char>
  (new
   (*parser (char #\~))
   (*parser (char #\{))
   (*parser (char #\}))
   (*parser (char #\\))
   (*disj 4)
;   (*pack
;    (lambda(lst) (list->string lst)))
   done))

(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))

(define <string-meta-char>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page))
       (*parser (^<meta-char> "\\{tab}" #\tab))
       (*parser (^<meta-char> "\\{newline}" #\newline))
       (*parser (^<meta-char> "\\{return}" #\return))
       (*parser (^<meta-char> "\\{page}" #\page))
       (*parser (^<meta-char> "\\{" #\{))
       (*parser (^<meta-char> "\\}" #\}))
       (*disj 11)
       done))


;;digits;;

(define <digit-1-9>
  (range #\1 #\9))

(define <digit-0-9>
  (range #\0 #\9))

(define <hex-digit>
  (let ((zero (char->integer #\0))
	(lc-a (char->integer #\a))
	(uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
	 (*pack
	  (lambda (ch)
	    (- (char->integer ch) zero)))

	 (*parser (range #\a #\f))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) lc-a))))

	 (*parser (range #\A #\F))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) uc-a))))

	 (*disj 3)
	 done)))

(define <XX>
  (new (*parser <hex-digit>)
       (*parser <hex-digit>)
       (*caten 2)
       (*pack-with
	(lambda (h l)
	  (+ l (* h 16))))
       done))

(define <XXXX>
  (new (*parser <XX>)
       (*parser <XX>)
       (*caten 2)
       (*pack-with
	(lambda (h l)
	  (+ l (* 256 h))))
       done))

(define <hex-char>
  (new (*parser (word-ci "\\"))
       (*parser (one-of-ci "hux"))
       (*parser (word "16#"))
       (*disj 2)
       (*caten 2)
       
       (*parser <XXXX>)
       (*parser <XX>)
       (*disj 2)
       (*pack integer->char)

       (*caten 2)
       (*pack-with (lambda (_< ch) ch))
       done))

(define <whitespace>
  (const
   (lambda(ch)
     (char<=? ch #\space))))

(define <natural-number>
  (new (*parser (char #\0))
       
       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 2)
       (*pack-with
        (lambda (fst lst)
          (list->string (cons fst lst))))
       
       (*disj 2)
       done))


;; real parsers ;;

(define <string>
  (new (*parser <any-char>)
       (*parser <meta-char>)
       *diff
       
       (*parser <string-meta-char>)
       (*parser <hex-char>)
       (*disj 3)
       
       *plus
       (*pack (lambda(lst) (list->string lst)))
       done))

(define ^<wrap>
  (lambda(<left> <right>)
    (lambda(<p>)
      (new
       (*parser <left>)
       (*parser <p>)
       (*parser <right>)
       (*caten 3)
       (*pack-with
        (lambda (_ e __) e))
       done))
    ))

(define <symbol-str>
  (new (*parser (range-ci #\a #\z))
       (*parser (range #\0 #\9))
       (*parser (one-of "!$^*-_=+<>?/"))
       (*disj 3) *plus
       (*pack
        (lambda (s)
          (string->symbol
           (list->string s))))
       (*parser <natural-number>)
       *diff
       done))

(define ^<sym>
  (lambda (vars)
    (new
     (*parser ((^<wrap> (char #\{) (char #\}))
               ((^<wrap> (star <whitespace>) (star <whitespace>))
                <symbol-str>)))
     (*pack
      (lambda (sym)
        (let ((val (cadr (or
                          (assoc sym vars)
                          (error 'varible (format "~a not found\n" sym))))))
          (format "~a" val))))
     done)
    ))
#|
(define <comment-str>
  (new
   (*delayed (lambda () (^<parser> '((undef "undef")))))
   (*pack
    (lambda (_) ""))
   done))
|#

(define <comment-str>
  (new
   
   (*parser (word "~"))
   (*delayed (lambda () <comment>))
   (*caten 2)
   
   (*parser <any-char>)
   (*parser (word "~{{"))
   (*parser (word "}}"))
   (*disj 2)
   *diff
   (*disj 2)
   *star
   (*pack
    (lambda (_) ""))
   done))

(define <comment>
  (new
   (*parser ((^<wrap> (word "{{") (word "}}")) <comment-str>))
   done))

(define <dashes>
  (new
   (*parser (char #\-)) *plus
   done))

(define ^<width-val>
  (lambda (vars)
    (let ((<sym> (^<sym> vars)))
      (new
       (*parser <natural-number>)
       (*parser <sym>)
       (*disj 2)
       done))
    ))

(define ^<width-no-arrows>
  (lambda(vars)
    (new
     (*parser ((^<wrap> <dashes> <dashes>) (^<width-val> vars)))
     done)
    ))

(define ^<width>
  (lambda(vars)
    (let ((<width-no-arrows> (^<width-no-arrows> vars))
          (<sym> (^<sym> vars)))
      (new
       
       (*parser <width-no-arrows>)
       (*parser (char #\>))
       (*parser <sym>)
       (*caten 3)
       (*pack-with
        (lambda (amount _> var-value)
          (dir-padding var-value 0 (string->number amount))
          ))
       
       (*parser (char #\<))
       (*parser (^<width-no-arrows> vars))
       (*parser (char #\>))
       (*parser <sym>)
       (*caten 4)
       (*pack-with
        (lambda (_< amount _> var-value) 
          (dir-padding var-value (string->number amount) (string->number amount))
          ))
       
       (*parser (char #\<))
       (*parser <width-no-arrows>)
       (*parser <sym>)
       (*caten 3)
       (*pack-with
        (lambda (_< amount var-value)
          (dir-padding var-value (string->number amount) 0)
          ))
       
       (*disj 3)
       done))
    ))

(define ^<tilda>
  (lambda (vars)
    (let* ((<sym> (^<sym> vars))
           (<width> (^<width> vars)))
      (new (*parser (char #\~))
           
           (*parser <comment>)
           (*parser <sym>)
           (*parser <width>)
           (*parser (char #\~))
           (*pack 
            (lambda (ch) (format "~a" ch)))
           (*disj 4)
           
           (*caten 2)
           (*pack-with
            (lambda (_ e) e))
           
           done)
      )))

(define ^<parser>
  (lambda (vars)
    (let ((<tilda> (^<tilda> vars)))
      (new (*parser <tilda>)
           (*parser <string>)
           (*disj 2) *star
           (*pack 
            (lambda (lst)
              (apply string-append lst)))
           done))
    ))

#|
(define formatter
  (lambda (str . vars)
    (let* ((var-l (apply append vars))
           (<parser> (^<parser> var-l))
           (res (test-string <parser> str)))
      (if (equal? (cadadr res) "")
      (cadar res)
      (error 'formatter (format "invalid string format: ~a \n remaining string: ~a" str (cadadr res)))))
    ))
|#

(define formatter
  (lambda (str . vars)
    (let* ((var-l (apply append vars))
           (<parser> (^<parser> var-l)))
      (<parser> (string->list str)
                (lambda (res  remaining-chars)
                  (if (null? remaining-chars)
                      res
                      (error 'formatter (format "invalid string format: ~a \n remaining string: ~a" str remaining-chars))))
                (lambda (a) 
                  (error 'formatter (format "invalid string format: ~a \n no mach found" str))))
      )))




