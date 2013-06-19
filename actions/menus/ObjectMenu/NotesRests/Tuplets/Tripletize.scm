;;;Tripletize
(if (Appending?)
	(d-MoveCursorLeft))
(if (Note?)
	(begin
		(d-StartTriplet)
		(d-Diminish)
		(d-SetMark)
		(d-Copy)
		(d-Paste)
		(d-Paste)
		(d-MoveCursorRight)
		(d-EndTuplet))
	(d-WarningDialog (_ "This command needs a note or chord to turn into a triplet")))
