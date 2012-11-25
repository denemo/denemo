;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
	(if (d-PreviousMovement)
		(d-WarningDialog "Returning to the first Movement to set the arranger"))
	(while (d-PreviousMovement)
		(display "Seeking first movement"))
        (SetHeaderField "arranger")
        (DenemoPrintAllHeaders)
        (d-RefreshDisplay)        
