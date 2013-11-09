;;;StandAloneFootnote
(let ((tag "StandaloneFootnote"))
   (define (set-footnote mark text)
	(d-Directive-standalone tag)
	(d-DirectivePut-standalone-postfix tag (string-append
		"\\override Score.FootnoteItem #'annotation-line = ##f \\footnote \""
		mark 
		"\" #'(0 . 6) #'NonMusicalPaperColumn \\markup { \\super \"" mark "\" \\teeny \""
		text 
		"\"} \\default  "))
	(d-DirectivePut-standalone-graphic tag (string-append "\n" mark "\nDenemo\n16"))
	(d-DirectivePut-standalone-display tag (string-append text "\n" mark "\n" text))
	(d-DirectivePut-standalone-gy tag -40)
	(d-DirectivePut-standalone-minpixels tag 20)
	(d-SetSaved #f)
	(d-RefreshDisplay))
  (define (choose-footnote mark text)	
			(set! text (d-GetUserInput  (_ "Footnote") (_ "Give footnote text") text))
			(set! mark (d-GetUserInput (_ "Footnote") (_ "Give footnote marker") mark))
			(if (and mark text)
			(set-footnote mark text)))
  (if (d-Directive-standalone? tag)
			(let ((choice #f) (current (d-DirectiveGet-standalone-display tag)))
				(define mark (GetNthLine current 1))
				(define text (GetNthLine current 2))
				(disp "mark " mark " and text " text "ok")
				
				(disp "About to ask for choice " choice " at start")
				(set! choice (GetEditOption))
				(disp "Now got choice " choice " ok\n")
				(case choice
					((edit) (choose-footnote mark text))
					((delete) (d-DirectiveDelete-standalone tag))
					((advanced) (d-DirectiveTextEdit-standalone tag))
					(else (disp "A problem with eqv ..."))))
	(begin
		(choose-footnote "*"(_ "Orig. ")))))
	