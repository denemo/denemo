(let ()
(define (stringnr stringy)
	(if (Note?)
		;force chordizise. Ugly lilypond output, but guaranteed to work for all instruments
		(begin (d-Chordize) (AttachDirective "note" "postfix" (cons "StringNumber" (string-append "s" stringy)) (string-append "\\" stringy " ")))
		#f)) ; not a note. Abort.
(Doublestroke
	(lambda () (stringnr (string-trim-both (d-GetUserInput "String Number" "Please enter a string number"  "1")))) ;Ask the user which string
	(cons "1st string" (lambda () (stringnr "1")))
	(cons "2nd string" (lambda () (stringnr "2")))
	(cons "3rd string" (lambda () (stringnr "3")))
	(cons "4th string" (lambda () (stringnr "4")))
	(cons "5th string" (lambda () (stringnr "5")))
	(cons "6th string" (lambda () (stringnr "6")))
	(cons "7th string" (lambda () (stringnr "7")))
	(cons "8th string" (lambda () (stringnr "8")))
	(cons "9th string" (lambda () (stringnr "9")))
	(cons "10th string" (lambda () (stringnr "10")))));; d-StringNum