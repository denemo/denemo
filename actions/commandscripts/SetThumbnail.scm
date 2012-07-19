;;;ThumbnailSelection
(if (d-GetSaved)
	(begin
	(if (d-PreviousMovement)
		(d-WarningDialog "Returning to the first Movement for thumbnail selection"))
	(while (d-PreviousMovement)
		(display "Seeking first movement"))
 	(if (d-GoToSelectionStart)
 		(let ((response "y"))
 			(d-SetThumbnailSelection)
 			(d-Save)
 			(set! response (d-GetUserInput "Thumbnail Selection Complete" "Create thumbnail now?" response))
 			(if (and response (equal? response "y"))
 				(d-CreateThumbnail)))	
 		(d-WarningDialog "No Selection to set the thumbnail to\nCreate a selection and re-run the command.")))
 	(d-WarningDialog "Save the score before trying to set the thumbnail"))
        