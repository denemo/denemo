 ;;;Duplicate Note or Chord, not for rests
(if (Note?) 
	(begin
	(d-PushClipboard) 
	(d-SetMark)
	(d-Copy)
	(d-Paste)
	(d-PopClipboard)))
	
(d-RefreshDisplay)