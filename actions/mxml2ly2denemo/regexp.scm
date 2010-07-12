(use-modules (ice-9 regex))
(use-modules (srfi srfi-13))
(use-modules (sxml ssax input-parse))
(load "look-for-str.scm")

(system "clear")

(set-current-input-port (open-input-file "test.txt"))

;;;Strip Comments and blank lines, collaps tabs and whitespaces to a single one
(define (lyparse::Strip stringy) 
	;(set! stringy (regexp-substitute/global #f "%\\{[^%\\{]*%\\}" stringy 'pre " " 'post))
	;(set! stringy (regexp-substitute/global #f "hallo.*\\\n" stringy 'pre "test" 'post))
	;(set! stringy (regexp-substitute/global #f "[^\\%]%.*\n" stringy 'pre "" 'post))
	;(set! stringy (regexp-substitute/global #f "[ \t]+" stringy 'pre " " 'post))
	stringy
)

(define (makelist port)
     (let loop ()
       (cons (next-token '() '(#\space #\newline *eof*) "" port)
     	(if (eof-object? (read-char port)) '() (loop))))
)

(define listy (makelist (current-input-port)))


(define note-regex (make-regexp "^[a-z]+('*|,*)[0-9]*\\.*$" regexp/icase)) ; TODO: % comments could begin just after a note name or even within a note name to comment out duration

(if (regexp-exec note-regex "a,3..")
(display "match")
)(newline)

(newline)
(close 0)
