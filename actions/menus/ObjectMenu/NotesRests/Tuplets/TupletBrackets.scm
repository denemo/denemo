;;TupletBrackets
(let ((tag "TupletBrackets")(choice (RadioBoxMenu
	 (cons (_"No Brackets") "#f")
	 (cons (_ "Always Brackets") "#t")
	 (cons (_ "Bracket if not beamed") "'if-no-beam") )))
(if choice
   (begin
		 (d-Directive-standalone tag)
		(StandAloneDirectiveProto (cons tag 
		(string-append "\\override TupletBracket #'bracket-visibility = #" choice "")) #f "\n[\nDenemo\n24")
		(d-DirectivePut-standalone-gy tag -44)
		(d-DirectivePut-standalone-grob tag tag)
		(d-MoveCursorRight)
		(d-RefreshDisplay)
		(d-SetSaved #f))))
