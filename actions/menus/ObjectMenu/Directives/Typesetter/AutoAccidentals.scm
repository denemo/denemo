;;AutoAccidentals
(let ((tag "AutoAccidentals"))
	(if AutoAccidentals::params 
		(d-InfoDialog (_ "This directive sets the style of typesetting for accidentals"))
		(let ((choice (RadioBoxMenu
				(cons (_ "Default") "default")
				(cons (_ "Modern") "modern")
				(cons (_ "Modern Cautionary") "modern-cautionary")
				(cons (_ "Neo-Modern") "neo-modern")	
				(cons (_ "Teaching") "teaching")	
				(cons (_ "Forget") "forget"))))
				(if choice
					(begin
						(d-SetSaved #f)
        					(d-DirectivePut-layout-postfix tag (string-append "\\accidentalStyle Score." choice "\n")))))))