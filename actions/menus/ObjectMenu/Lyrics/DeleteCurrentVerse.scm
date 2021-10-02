;DeleteCurrentVerse
(let ((delete DeleteCurrentVerse::params))
	(if (not delete)
		(set! delete (RadioBoxMenu (cons (_"Cancel") #f) (cons (_ "Delete Current Verse (no undo!)") 'delete))))
	(if delete
		(d-DeleteVerse)
		(if (not DeleteCurrentVerse::params)
			(d-InfoDialog (_ "Cancelled")))))
