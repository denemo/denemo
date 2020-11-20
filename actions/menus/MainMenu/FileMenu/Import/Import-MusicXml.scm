(if (not (MovementEmpty?))
	(begin
		(d-NewMovement)
		(while (d-StaffUp))
		(while (d-StaffDown)
			(d-DeleteStaff))
			(d-DeleteStaff)))
(if (not (or (d-Directive-movementcontrol? "TitledPiece") (d-Directive-scoreheader? "BookTitle")))
	(let ((choice (RadioBoxMenu (cons (_ "Simple Titles") 'simple) (cons (_ "Book Titles") 'book))))
		(case choice
			 ((simple) (d-DirectivePut-movementcontrol-display "TitledPiece" (_ "Untitled")))
			 ((book) (d-BookTitle (_ "Untitled"))))))
(d-ImportMusicXml)
