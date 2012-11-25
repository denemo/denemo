;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
	(if (d-PreviousMovement)
		(d-WarningDialog "Returning to the first Movement to set the Poet"))
	(while (d-PreviousMovement)
		(display "Seeking first movement"))
        (SetHeaderField "poet")
        (DenemoPrintAllHeaders)
        (d-RefreshDisplay)        
