;;;;StagedDelete
(if (Appending?)
	(d-MoveCursorLeft))
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
(d-SetSaved #f)
(d-RefreshDisplay)