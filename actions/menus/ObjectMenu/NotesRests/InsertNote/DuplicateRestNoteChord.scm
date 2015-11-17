;;;Duplicate Note, Chord or Rest
(let ((appending (Appending?)))
  (if appending
	(d-MoveCursorLeft))
	(if (Music?) 
		(begin
		
		(d-PushClipboard) 
		(d-SetMark)
		(d-Copy)
		(d-Paste)
		(d-PopClipboard)
		(d-RefreshDisplay)))
  (if appending
       (d-MoveCursorRight)))	