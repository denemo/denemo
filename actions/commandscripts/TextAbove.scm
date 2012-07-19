;;TextAbove
(let ((text #f))
	(set! text (d-GetUserInput "Text" "Give text to appear above music: " "\\bold \\italic \"Tutti\""))
	(if text (begin
		(d-DirectivePut-chord-display "Text"  text )
		(d-DirectivePut-chord-postfix "Text"  (string-append "^\\markup " text  " "))
		(d-DirectivePut-chord-minpixels  "Text" 20)))
		(d-SetSaved #f))