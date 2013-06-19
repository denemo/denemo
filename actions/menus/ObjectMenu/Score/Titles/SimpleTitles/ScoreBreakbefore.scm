;;;ScoreBreakbefore
	(if (d-PreviousMovement)
		(d-WarningDialog "Returning to the first Movement to set this."))
	(while (d-PreviousMovement)
		(display "Seeking first movement"))
        (SetHeaderField "opus")
        (DenemoPrintAllHeaders)
        (d-RefreshDisplay)
        