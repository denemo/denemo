(use-modules (srfi srfi-1)) ; List library

;; In this file are only functions that enhance the traditional Guile functions. Nothing depends on any Denemo functions nor on anything which is not in this file. There might be dependencies between each other though. Conclusion: This file can be run from guile without any errors about functions not found.

; Create a seed for (random int) one time Denemo starts. The seed is altered by random itself afterwards.
(let ((time (gettimeofday)))
    (set! *random-state*
        (seed->random-state (+ (car time)
                 (cdr time)))))
                 
;Blank clears the console output. Don't use in released scripts, only for debugging.
(define (Blank)
    (system "clear"))

;disp is an advanced display. Just give anything you want to print, it appends strings automatically and does a line break in the end. Don't use in released scripts, only for debugging.
(define disp (lambda args
   (letrec ((disp-in (lambda (arg) 
              (if (null? arg) 
                  #f 
                  (begin 
                     (display (car arg)) 
                     (disp-in (cdr arg))))))) 
             (disp-in args)
             (newline))))

;Doublequote constant to avoid backslashing
(define DBLQ "\"")
;Linefeed constant to avoid backslashing
(define LFEED "\n")
;Stop constant to avoid backslashing
(define stop "\0")

; A function that returns #f for cases where commands work with chunks of code. this prevents the spamming of (lambda () #f) for a function that returns #f.
(define (False) 
    #f)
    
; A function that returns #t. See (False)
(define (True) 
    #t)

;repeat executes a proc n times
(define (Repeat proc n)
    (let loop ((counter 0))
        (if (> n counter)
            (begin
                (proc)
                (loop (1+ counter))))))

;Repeat a command until it returns #f
;Warning: Functions that do not return #f create infinity loops!
(define (RepeatUntilFail proc)
    (let loop ()
        (if (proc)
            (loop)
            #f)))
            
;Repeat a function while another (a test) returns #t. The return value of proc does NOT matter
;;Warning: From all Repeat functions this one has the highest probability to be stuck in a loop forever. Always use tests that MUST return #f in the end. Do NOT use the Denemo tests like (None?) or (Music?) for example, they know nothing about a staffs end.
(define (RepeatProcWhileTest proc test)
    (RepeatUntilFail        
        (lambda () 
            (if (test)
                (begin (proc) #t); this is a dumb script. It will try to execute proc again even if proc itself returned #f.    
                #f )))) ; test failed, let RepeatUntilFail fail.
                

    
; String Escaper
;; Escapes Strings
;; from brlewis http://srfi.schemers.org/srfi-13/mail-archive/msg00025.html
;;; esc should be a list of pairs, each pair is a character and a substitute string.
(define (string-escaper esc)
  (let ((spec (char-escape-spec esc)))
    (lambda (str) (string-escape str spec))))

(define (string-needs-escape? str esc)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (= i len)
      #f
      (let ((c (string-ref str i)))
        (if (and (char>=? c (car esc))
             (char<=? c (cadr esc)))
        #t
        (loop (+ 1 i))))))))

(define (string-escape str esc)
  (if (string-needs-escape? str esc)
      (list->string
       (reverse
    (let ((len (string-length str)))
      (let loop ((i 0)
             (li '()))
        (if (= i len)
        li
        (loop (+ 1 i)
              (let ((c (string-ref str i)))
            (if (and (char>=? c (car esc))
                 (char<=? c (cadr esc)))
                (let ((li2 (vector-ref
                    (caddr esc)
                    (- (char->integer c)
                       (char->integer (car esc))))))
                  (if li2
                  (append li2 li)
                  (cons c li)))
                (cons c li)))))))))
      str))

(define (char-escape-spec speclist)
  (let ((minchar (caar speclist))
    (maxchar (caar speclist)))
    (let loop ((li (cdr speclist)))
      (if (not (null? li))
      (begin
        (let ((testchar (caar li)))
          (if (char<? testchar minchar)
          (set! minchar testchar))
          (if (char>? testchar maxchar)
          (set! maxchar testchar)))
        (loop (cdr li)))))
    (list
     minchar
     maxchar
     (let ((specv (make-vector (+ 1 (- (char->integer maxchar)
                       (char->integer minchar))) #f)))
      (map (lambda (specpair)
         (vector-set! specv
              (- (char->integer (car specpair))
                 (char->integer minchar))
              (reverse (string->list (cdr specpair)))))
       speclist)
      specv))))
      
;; examples of use

(define html-escape (string-escaper '((#\< . "&lt;")
                      (#\> . "&gt;")
                      (#\& . "&amp;"))))

(define scheme-escape (string-escaper '((#\\ . "\\\\")
                    (#\" . "\\\""))))
                    
                    
(define lilypond-markup-escape (string-escaper '((#\\ . "\\\\") (#\# . "\"#\"")
                    (#\" . "\\\""))))

#! (define latex-escape (string-escaper '((#\\ . "\\\\")
                       (#\~ . "\\~")
                       (#\# . "\\#")
                       (#\$ . "\\$")
                       (#\% . "\\%")
                       (#\^ . "\\^")
                       (#\& . "\\&")
                       (#\{ . "\\{")
                       (#\} . "\\}")
                       (#\_ . "\\_")))) !#


;;;;;;;;;; Parse strings for json values
(define (ParseJson target key)
  (let ((theregexp #f) (thematch #f))

 ;;; this is the regexp to find "key":"something" with the something being the match, ie in ()
(set! theregexp (string-append "\"" key "\":\"([^\"]*)\""))

;;;;; this gets a match structure for target, so if there is a match
(set! thematch (string-match theregexp target))

(if (regexp-match? thematch) ;;; if there was a match return it
   (match:substring thematch 1)
    ;;;if there was no match return #f
    #f)))

; Shuffling Sequences
;;; http://mumble.net/~campbell/scheme/shuffle.scm
;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.
;;; This uses SRFIs 1 (list-lib) and 8 (receive).

(use-modules (srfi srfi-1)) ; List library
(use-modules (srfi srfi-8)) ; Returning and Accepting Multiple Values

;;;; Merge Shuffle
;;; Partition the list into two equal halves; shuffle the two halves,
;;; and then merge them by randomly choosing which half to select the
;;; next element from.

(define (Flip-coin) 
    (if (= 1 (random 2))
        #t
        #f))

(define (Merge-shuffle-list list)

  (define (merge a b)
    (cond ((not (pair? a)) b)
          ((not (pair? b)) a)
          (else
           (if (Flip-coin)
               (cons (car a) (merge (cdr a) b))
               (cons (car b) (merge a (cdr b)))))))

  (define (partition list a b)
    (let ((next (cdr list))
          (a b)
          (b (cons (car list) a)))
      (if (null-list? next)
          (values a b)
          (partition next a b))))

  (if (null-list? list)
      '()
      (let shuffle ((list list))
        (if (null-list? (cdr list))
            list
            (receive (a b) (partition list '() '())
              (merge (shuffle a) (shuffle b)))))))

;;; This has *far* too many SET-CDR!s.

(define (Merge-shuffle-list! list)

  (define (merge! a b)
    (cond ((null-list? a)       b)
          ((null-list? b)       a)
          ((Flip-coin)          (%merge! a b) a)
          (else                 (%merge! b a) b)))

  (define (%merge! a b)
    (cond ((null-list? (cdr a))
           (set-cdr! a b))
          ((Flip-coin)
           (%merge! (cdr a) b))
          (else
           (%merge! b (let ((next (cdr a)))
                        (set-cdr! a b)
                        next)))))

  (define (partition! list a b)
    (let ((next (cdr list)))
      (set-cdr! list a)
      (if (null-list? next)
          (values list b)
          (partition! next b list))))

  (if (null-list? list)
      '()
      (let shuffle! ((list list))
        (if (null-list? (cdr list))
            list
            (receive (a b) (partition! list '() '())
              (merge! (shuffle! a) (shuffle! b)))))))
;; End Shuffle

    
; from http://icem.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/t-y-scheme/t-y-scheme-Z-H-7.html
;; needed for define-macro defstruct
(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (if (null? l) #f
          (if (eqv? (car l) o) i
              (loop (+ i 1) (cdr l)))))))

; from http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-11.html#node_sec_9.2
(define-macro (defstruct s . ff)
    (let ((s-s (symbol->string s)) (n (length ff)))
      (let* ((n+1 (+ n 1))
             (vv (make-vector n+1)))
        (let loop ((i 1) (ff ff))
          (if (<= i n)
            (let ((f (car ff)))
              (vector-set! vv i 
                (if (pair? f) (cadr f) '(if #f #f)))
              (loop (+ i 1) (cdr ff)))))
        (let ((ff (map (lambda (f) (if (pair? f) (car f) f))
                       ff)))
          `(begin
             (define ,(string->symbol 
                       (string-append "make-" s-s))
               (lambda fvfv
                 (let ((st (make-vector ,n+1)) (ff ',ff))
                   (vector-set! st 0 ',s)
                   ,@(let loop ((i 1) (r '()))
                       (if (>= i n+1) r
                           (loop (+ i 1)
                                 (cons `(vector-set! st ,i 
                                          ,(vector-ref vv i))
                                       r))))
                   (let loop ((fvfv fvfv))
                     (if (not (null? fvfv))
                         (begin
                           (vector-set! st 
                               (+ (list-position (car fvfv) ff)
                                  1)
                             (cadr fvfv))
                           (loop (cddr fvfv)))))
                   st)))
             ,@(let loop ((i 1) (procs '()))
                 (if (>= i n+1) procs
                     (loop (+ i 1)
                           (let ((f (symbol->string
                                     (list-ref ff (- i 1)))))
                             (cons
                              `(define ,(string->symbol 
                                         (string-append
                                          s-s "." f))
                                 (lambda (x) (vector-ref x ,i)))
                              (cons
                               `(define ,(string->symbol
                                          (string-append 
                                           "set!" s-s "." f))
                                  (lambda (x v) 
                                    (vector-set! x ,i v)))
                               procs))))))
             (define ,(string->symbol (string-append s-s "?"))
               (lambda (x)
                 (and (vector? x)
                      (eqv? (vector-ref x 0) ',s)))))))))
                      
;InvertedMap takes a list of functions and applies each to a single value and returns a list of returnvalues.
(define (InvertedMap val . funcs)
    (concatenate
        (map (lambda (proc) (call-with-values (lambda () (proc val)) list))
         funcs)))
         
#! (define (InvertedMap2 value . functions)
    (define returnlist (list #f))
    (for-each (lambda (x)
        (define return (x value))
        (if return
            (append! returnlist (list return))))
        functions)  
    (list-tail returnlist 1)) !#
    
(define (Confirm title msg)
  (let ((ch (d-GetUserInput title msg (_ "y"))))
        (and ch (equal? ch (_ "y")))))
