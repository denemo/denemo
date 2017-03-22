;;DeleteCurrentVerse
(let ((delete (RadioBoxMenu (cons (_"Cancel") #f) (cons (_ "Delete Current Verse (no undo!)") 'delete))))
	(if delete
		(d-DeleteVerse)
		(d-InfoDialog (_ "Cancelled"))))
