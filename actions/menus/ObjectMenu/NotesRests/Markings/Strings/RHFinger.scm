;;;;;; RHFinger
(let ((tag "RHFinger") (num "0"))
	(set! num (d-GetUserInput "Right Hand Fingering" "Give finger number" "1"))
	(if (and num (string->number num))
		(begin 
			(d-DirectivePut-note-display tag num)
			(d-DirectivePut-note-postfix tag  (string-append "-\\rightHandFinger #" num " "))
			(d-DirectivePut-note-minpixels tag 20)
			(d-DirectivePut-note-tx tag 10)
			(d-Chordize #t))
		(begin
			(d-DirectiveDelete-note tag)))
(d-SetSaved #f)
(d-RefreshDisplay))
