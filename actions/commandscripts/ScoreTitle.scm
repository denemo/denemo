;;;ScoreTitle
	(if (d-PreviousMovement)
		(d-WarningDialog "Returning to the first Movement to set this title"))
	(while (d-PreviousMovement)
		(display "Seeking first movement"))
        (SetHeaderField "title")
        (DenemoPrintAllHeaders)
        (d-RefreshDisplay)
        