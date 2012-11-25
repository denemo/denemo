;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;ScoreTitle
	(if (d-PreviousMovement)
		(d-WarningDialog (_ "Returning to the first Movement to set this title")))
	(while (d-PreviousMovement)
		(display "Seeking first movement"))
        (SetHeaderField "title")
        (DenemoPrintAllHeaders)
        (d-RefreshDisplay)
        