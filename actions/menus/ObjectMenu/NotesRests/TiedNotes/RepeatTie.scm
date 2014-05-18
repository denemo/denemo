;;RepeatTie
(let ((tag "RepeatTie"))
   (if (Appending?)
   	(d-MoveCursorLeft))
    (if (Note?)
       (begin
		(if (d-Directive-chord? tag)
		    (d-DirectiveDelete-chord tag)   
		    (begin
		        (d-DirectivePut-chord-postfix tag "\\repeatTie")
		        (d-DirectivePut-chord-graphic tag "\n⌣\nDenemo\n30")))
		(d-RefreshDisplay)
		(d-SetSaved #f))
        (d-InfoDialog (_ "This command only applies to notes or chords."))))

