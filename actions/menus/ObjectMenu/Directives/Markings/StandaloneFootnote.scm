;;;StandAloneFootnote
(let ((tag "StandaloneFootnote"))
   (define (set-footnote mark text)
	(d-Directive-standalone tag)
	(d-DirectivePut-standalone-postfix tag (string-append
		"\\override Score.FootnoteItem.annotation-line = ##f \\footnote \""
		mark 
		"\" #'(0 . 2) \\markup { \\super \"" mark "\" \\teeny \""
		text 
		"\"}  "))
	(d-DirectivePut-standalone-graphic tag (string-append "\n" mark "\nDenemo\n16"))
	(d-DirectivePut-standalone-data tag (string-append "(cons \"" text "\" \""  mark "\")"))
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
			(let ((mark "*") (text (_ "Orig. ")) (choice #f) (current  (eval-string (d-DirectiveGet-standalone-data tag) )))
				(if current 
					(begin
						(set! mark (cdr current))
						(set! text (car  current))))

				(set! choice (GetEditOption))
				(case choice
					((edit) (choose-footnote mark text))
					((delete) (d-DirectiveDelete-standalone tag))
					((advanced) (d-DirectiveTextEdit-standalone tag))
					(else (disp "A problem with eqv ..."))))
	(begin
		(choose-footnote "*"(_ "Orig. ")))))
	