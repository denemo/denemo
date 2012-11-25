;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;ScorePiece
	(if (d-PreviousMovement)
		(d-WarningDialog "Returning to the first Movement to set this."))
	(while (d-PreviousMovement)
		(display "Seeking first movement"))
        (SetHeaderField "piece")
        (DenemoPrintAllHeaders)
        (d-RefreshDisplay)
        