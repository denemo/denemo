;;;LilyPondDefinition
(let ((name #f)(def #f))
	(if (and (defined? 'LilyPondDefinition::params) (pair? LilyPondDefinition::params))
		(begin
			(set! name (car LilyPondDefinition::params))
			(set! def (cdr LilyPondDefinition::params)))
		(begin
			(set! name (d-GetUserInput (_ "Creating LilyPond Definition") (_ "Give name (alphabetical only): ") "Barline"))
			(if name 
					(set! def (d-GetUserInput (_ "Creating LilyPond Definition") (_ "Give valid LilyPond syntax for this definition: ") "\\bar \"||\"")))))
	(if def
		(let ( (tag (string-append "Allow\n" name)))
		(set! def (string-trim-both def char-set:whitespace))
		(if (string-null? def)
			(set! def "{}"))
		(d-DirectivePut-score-prefix tag (string-append name " = " def "\n"))
		(d-DirectivePut-score-display tag name)
		(d-DirectivePut-score-override tag DENEMO_OVERRIDE_AFFIX)
		(d-SetSaved #f))))