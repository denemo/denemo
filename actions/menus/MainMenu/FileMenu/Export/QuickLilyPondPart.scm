;;;QuickLilyPondPart
(d-PushPosition)
(d-GoToBeginning)
(let ((name  (d-Open "query=filename")))
	(if name
		(let* ((filename  (string-append (substring name 0 (- (string-length name) 7)) "-denemo-" (number->string (d-GetStaff)) ".ly"))   
				(port (open-file filename "w")))
			(let loop ()
				(if port
					(begin 
						(format port "~A " (d-GetLilyPond))
						(if (d-MoveCursorRight)
							(loop)
							(close-port port))))))
		(d-WarningDialog (_ "The score has not been saved, so no name or location for the part is defined"))))
(d-PopPosition)	
