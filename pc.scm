;;; pc.scm
;;; A simple implementation of parsing combinators
;;;
;;; Programmer: Mayer Goldberg, 2013

(define *marker-length* 8)

(define with (lambda (s f) (apply f s)))

(define list-head
  (lambda (s n)
    (cond ((null? s) '())
	  ((zero? n) '(etc))
	  (else (cons (car s)
		  (list-head (cdr s) (- n 1)))))))

(define <end-of-input>
  (lambda (s ret-match ret-none)
    (if (null? s)
	(ret-match #t '())
	(ret-none '()))))

(define const
  (lambda (pred?)
    (lambda (s ret-match ret-none)
      (cond ((null? s) (ret-none '()))
	    ((pred? (car s)) (ret-match (car s) (cdr s)))
	    (else (ret-none '()))))))

(define <epsilon>
  (lambda (s ret-match ret-none)
    (ret-match '() s)))

(define caten
  (lambda ps
    (letrec ((binary-caten
	      (lambda (p1 p2)
		(lambda (s ret-match ret-none)
		  (p1 s
		      (lambda (e1 s)
			(p2 s
			    (lambda (e2 s)
			      (ret-match (cons e1 e2) s))
			    ret-none))
		      ret-none))))
	     (loop
	      (lambda (ps)
		(if (null? ps)
		    <epsilon>
		    (binary-caten (car ps)
		     (loop (cdr ps)))))))
      (loop ps))))

(define <fail>
  (lambda (s ret-match ret-none)
    (ret-none '())))

(define disj
  (lambda ps
    (letrec ((binary-disj
	      (lambda (p1 p2)
		(lambda (s ret-match ret-none)
		  (p1 s ret-match
		      (lambda (w1)
			(p2 s ret-match
			    (lambda (w2)
			      (ret-none `(,@w1 ,@w2)))))))))
	     (loop
	      (lambda (ps)
		(if (null? ps)
		    <fail>
		    (binary-disj (car ps)
		     (loop (cdr ps)))))))
      (loop ps))))

(define delay
  (lambda (thunk)
    (lambda (s ret-match ret-none)
      ((thunk) s ret-match ret-none))))

(define star
  (lambda (p)
    (disj (pack-with (caten p (delay (lambda () (star p))))
		     cons)
	  <epsilon>)))

(define plus
  (lambda (p)
    (pack-with (caten p (star p))
	       cons)))

(define pack
  (lambda (p f)
    (lambda (s ret-match ret-none)
      (p s (lambda (e s) (ret-match (f e) s)) ret-none))))

(define pack-with
  (lambda (p f)
    (lambda (s ret-match ret-none)
      (p s (lambda (e s) (ret-match (apply f e) s)) ret-none))))

(define diff
  (lambda (p1 p2)
    (lambda (s ret-match ret-none)
      (p1 s
	  (lambda (e w)
	    (p2 s (lambda _ (ret-none '()))
		(lambda (w1) (ret-match e w))))
	  ret-none))))

(define maybe
  (lambda (p)
    (lambda (s ret-match ret-none)
      (p s
	 (lambda (e s) (ret-match `(#t ,e) s))
	 (lambda (w) (ret-match `(#f #f) s))))))

(define guard
  (lambda (p pred?)
    (lambda (s ret-match ret-none)
      (p s
	 (lambda (e s)
	   (if (pred? e)
	       (ret-match e s)
	       (ret-none '())))
	 ret-none))))

(define otherwise
  (lambda (p message)
    (lambda (s ret-match ret-none)
      (p s
	 ret-match
	 (let ((marker
		(format "[-->~s]"
		  (list-head s *marker-length*))))
	   (lambda (w) (ret-none `(,@w ,message ,marker))))))))

;;;

(define ^char
  (lambda (char=?)
    (lambda (character)
      (const
       (lambda (ch)
	 (char=? ch character))))))

(define char (^char char=?))

(define char-ci (^char char-ci=?))

(define ^word
  (lambda (char)
    (lambda (word)
      (apply caten (map char (string->list word))))))

(define word (^word char))

(define word-ci (^word char-ci))

(define ^word-suffixes
  (lambda ()
    (letrec ((loop
	      (lambda (s)
		(if (null? s)
		    <epsilon>
		    (maybe
		     (caten (char (car s))
			    (loop (cdr s))))))))
      (lambda (suffix)
	(loop (string->list suffix))))))

(define word-suffixes (^word-suffixes))

(define word-suffixes-ci (^word-suffixes))

(define ^word+suffixes
  (lambda (word-suffixes)
    (lambda (prefix suffix)
      (caten (word prefix)
	     (word-suffixes suffix)))))

(define word+suffixes (^word+suffixes word-suffixes))

(define word+suffixes-ci (^word+suffixes word-suffixes-ci))

(define ^one-of
  (lambda (char)
    (lambda (word)
      (apply disj (map char (string->list word))))))

(define one-of (^one-of char))

(define one-of-ci (^one-of char-ci))

(define ^range
  (lambda (char<=?)
    (lambda (char-from char-to)
      (const
       (lambda (ch)
	 (and (char<=? char-from ch)
	      (char<=? ch char-to)))))))

(define range (^range char<=?))

(define range-ci (^range char-ci<=?))

(define <any-char> (const (lambda (ch) #t)))

;;;


(define continue
  (lambda (ds cs)
    (with cs
      (lambda (c . cs)
	(c ds cs)))))

(define new
  (lambda cs
    (continue '() cs)))

(define done
  (lambda (ds cs)
    (with ds
      (lambda (parser . ds)
	(if (null? ds)
	    parser
	    (error 'done
		   (format "The parser stack still contains ~a parsers!"
		     (length ds))))))))

(define *parser
  (lambda (p)
    (lambda (ds cs)
      (continue `(,p . ,ds) cs))))

(define unary
  (lambda (f-unary)
    (lambda (ds cs)
      (with ds
	(lambda (d . ds)
	  (continue `(,(f-unary d) . ,ds) cs))))))

(define *delayed
  (lambda (thunk)
    (lambda (ds cs)
      (continue `(,(delay thunk) . ,ds) cs))))

(define binary
  (lambda (f-binary)
    (lambda (ds cs)
      (with ds
	(lambda (d2 d1 . ds)
	  (continue `(,(f-binary d1 d2) . ,ds) cs))))))

(define *star (unary star))

(define *plus (unary plus))

(define *diff (binary diff))
  
(define *pack (lambda (f) (unary (lambda (p) (pack p f)))))

(define *pack-with (lambda (f) (unary (lambda (p) (pack-with p f)))))

(define *guard (lambda (pred?) (unary (lambda (p) (guard p pred?)))))

(define split-list
  (lambda (s n ret-s1+s2)
    (if (zero? n)
	(ret-s1+s2 '() s)
	(split-list (cdr s) (- n 1)
		    (lambda (s1 s2)
		      (ret-s1+s2 (cons (car s) s1) s2))))))

(define nary
  (lambda (f-n-ary n)
    (lambda (ds cs)
      (split-list ds n
       (lambda (sgra ds)
	 (continue
	  `(,(apply f-n-ary (reverse sgra)) . ,ds) cs))))))

(define *caten (lambda (n) (nary caten n)))

(define *disj (lambda (n) (nary disj n)))

(define *maybe (unary maybe))

(define *otherwise
  (lambda (string)
    (unary
     (lambda (p)
       (otherwise p string)))))

;;; 

(define test-string
  (lambda (parser string)
    (parser (string->list string)
	    (lambda (e s)
	      `((match ,e)
		(remaining ,(list->string s))))
	    (lambda (w) `(failed with report: ,@w)))))

(define test
  (lambda (parser s)
    (parser s
	    (lambda (e s)
	      `((match ,e)
		(remaining ,s)))
	    (lambda (w) `(failed with report: ,@w)))))

