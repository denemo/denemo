;;;AllowVoltaHook
(let ((tag "AllowVoltaHook") (choice (RadioBoxMenu (cons "End at double barline ||" "||") (cons "End at barline |" "|") (cons "End at custom barline" 'custom))))
            (if (eq? choice 'custom)
                (set! choice (d-GetUserInput (_ "Ending a Repeat Alternative") (_ "Give barline to end at: ") "[|:")))
            (if choice
                    (begin
		            (d-DirectivePut-scoreheader-postfix tag (string-append "\n\\allowVoltaHook \"" choice "\"\n"))))
		            (d-SetSaved #f))
        
