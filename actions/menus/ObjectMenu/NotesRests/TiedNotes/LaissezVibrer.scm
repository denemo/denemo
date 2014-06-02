;;LaissezVibrer
(let ((tag "LaissezVibrer"))
   (if (Appending?)
   	(d-MoveCursorLeft))
    (if (Note?)
       (begin
		(if (d-Directive-chord? tag)
		    (d-DirectiveDelete-chord tag)   
		    (begin
		        (d-DirectivePut-chord-postfix tag "\\laissezVibrer")
		        (d-DirectivePut-chord-gy tag -30)
		        (d-DirectivePut-chord-graphic tag "\n𝅘𝅥⌣\nDenemo\n18")))
		(d-RefreshDisplay)
		(d-SetSaved #f))
        (d-InfoDialog (_ "This command only applies to notes or chords."))))
