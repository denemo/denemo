;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;;StagedDelete Deletes a chord one note at a time, finally ending at non-printing rest and then deleting that
(if (Music?)
    (if (d-GetNonprinting)
	;;; non-printing
	(if (d-GetNotes)
	    (d-RemoveNoteFromChord)
	    (d-DeleteObject))
	
	     ;;; an ordinary black note or rest
	(if (d-GetNotes)
	    (d-RemoveNoteFromChord)
	    (d-SetNonprinting)))
	;;; not a music object
    (d-DeleteObject))	

(d-RefreshDisplay)