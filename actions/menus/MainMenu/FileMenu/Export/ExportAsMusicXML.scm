;ExportAsMusicXML
(define-once ExportAsMusicXML::warning #t)
(if (d-GetSaved)
	(let* ((num-movements (d-GetMovementsInScore)) (single-movement (= 1 num-movements))(filename (d-Open "query=filename")))
		(if (and filename (> (string-length filename) 7)) ;7 = length of .denemo suffix
						(set! filename (string-drop-right filename 7)))
		(if  ExportAsMusicXML::warning
			(begin
				(set!  ExportAsMusicXML::warning #f)
				(if single-movement
					(d-WarningDialog (_ "Your score will be exported to MusicXML music interchange format. Note this does not preserve all the information - you must save your score as well"))
					(d-WarningDialog (_ (string-append
					"Your score will be saved one movement to a file with the n'th movement named "
					filename "-n.xml"))))))
		(if single-movement
			(d-ExportMusicXML)
			(let ()
				(while (d-PreviousMovement))
				(let loop ((num 1))
					(d-ExportMusicXML (string-append filename "-" (number->string num)))
					(if (<= num num-movements)
						(begin
							(d-NextMovement)
							(loop (1+ num))))))))
	(d-WarningDialog (_ "Score not saved.")))
