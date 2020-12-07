;;ShiftDot
(let ((tag "ShiftDot") (X (d-GetUserInput (_ "Dot Position") (_ "Give horizontal shift required") "-0.2"))
	(Y  (d-GetUserInput (_ "Dot Position") (_ "Give vertical shift required") "0.0")))
		(if (and X Y)
			(begin
				(d-DirectivePut-note-prefix tag (string-append "\\tweak Dots.extra-offset #'(" X " . " Y ")"))
				(d-DirectivePut-note-display tag "<d>")
				(d-SetSaved #f))
			(d-InfoDialog (_ "Cancelled"))))
