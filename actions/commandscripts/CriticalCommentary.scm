;;CriticalCommentary
(let ((intro (d-DirectiveGet-score-display "CriticalCommentaryIntro")))
  (define Title "Critical Commentary")
  (define Prolog "Key: m = Movement, v = voice, b = bar")
  (if intro
    (begin
      (set! Title (substring  intro 0 (string-index intro #\nl)))
      (set! Prolog (substring  intro (string-index intro #\nl)))))
  (d-PushPosition)
  (while (d-PreviousMovement))
  (let ()
   (define (format-commentary thelist)
    (define ret "")
    (let loop ((count 0))
      (if (< count (length thelist))
	(begin
	  (set! ret (string-append ret "\n\\markup { \\wordwrap-string #\"" (number->string (+ 1 count)) ". \""
	    (list-ref thelist count) "}\n"))
	  (loop (+ 1 count)))))
   ret)
   (define thecomments '())
   (define ok #f)
   (define tag "CriticalComment")
 
   (let movement ()
    (define movement-number (number->string (d-GetMovement)))
    (define voice 1)
    (while (d-MoveToStaffUp))
    (d-MoveToBeginning)

   (let measure ()
      (define measure-number (number->string (d-GetMeasure)))
      (define thecomment #f)
	(let loop ()
	  (if (d-Directive-standalone? tag) 
	    (begin
	      (set! ok #t)
	      (set! thecomment (d-DirectiveGet-standalone-postfix tag))
	      (set! thecomments (cons* (string-append "\\wordwrap-string #\"At m" movement-number " v" (number->string voice) " b" measure-number ": \"" thecomment) thecomments))))
	    (if (NextDirectiveOfTagInMeasure tag)
	      (loop)))	     
	(if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
	  (begin
	    (set! voice (+ 1 voice))
	    (measure))
	  (begin
	    (if (d-MoveToMeasureRight)
		(begin
		  (while  (d-MoveToStaffUp))
		  (set! voice 1)
		  (measure))))))
			    
    (if (d-NextMovement)
	(movement)
	(begin
	    (if ok
	      (begin
		(d-SetSaved #f) 
		(d-LilyPondInclude "book-titling.ily")
		(d-LilyPondInclude "simplified-book-titling.ily")
		(d-DirectivePut-score-override "CriticalCommentary" DENEMO_OVERRIDE_AFFIX)
		(d-DirectivePut-score-postfix "CriticalCommentary" (string-append "\\pageBreak\n\\titledPiece \\markup \"" (scheme-escape Title) "\"\n\\markup { \\fill-line {\\postscript #\"-12 3 moveto 24 0 rlineto stroke\"}}\n\\markup {\\italic \\wordwrap-string #\"" (scheme-escape Prolog) "\"}\n"  (format-commentary (reverse thecomments)))))
	      (begin
		(d-SetSaved #f) 
		(d-LilyPondInclude "book-titling.ily")
		(d-LilyPondInclude "simplified-book-titling.ily")
		(d-DirectivePut-score-override "CriticalCommentary" DENEMO_OVERRIDE_AFFIX)
		(d-DirectivePut-score-postfix "CriticalCommentary" (string-append "\\pageBreak\n\\titledPiece \\markup \"" (scheme-escape Title) "\"\n\\markup { \\fill-line {\\postscript #\"-12 3 moveto 24 0 rlineto stroke\"}}\n\\markup {\\italic \\wordwrap-string #\"" (scheme-escape Prolog) "\"}\n" )))
	      ))))) 				
   (d-PopPosition))
 