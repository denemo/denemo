;;;TweakFingeringPosition
(let ((tag "Fingering")(x #f) (y #f) (current #f)(finger #f)(data #f))
	(if (d-Directive-note? tag)
		(begin
			(set! data (d-DirectiveGet-note-data tag))
			(if data
				(set! finger (car (eval-string data)))
				(set! finger (d-GetUserInput  (_ "Fingering") (_  "Give finger number: ") "1")))
			(if data
				(set! current (cadr (eval-string data)))
				(set! current "1"))
			(set! x (d-GetUserInput (_ "Fingering Position") (_  "Give horizontal shift required: ") current))
			(if x
				(begin
					(if data
						(set! current (caddr (eval-string data)))
						(set! current "1"))
					(set! y (d-GetUserInput (_ "Fingering Position") (_  "Give vertical shift required: ") current))
					(if y
						(begin 
							(d-DirectivePut-note-postfix tag (string-append "- \\tweak  extra-offset #'(" x " . " y ")-" finger " "))
							(d-DirectivePut-note-display tag finger)
							(d-DirectivePut-note-data tag (string-append  "(list \"" finger "\" \"" x "\" \"" y "\")"))))))))
(d-SetSaved #f))
