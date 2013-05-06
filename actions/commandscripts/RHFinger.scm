;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;;;; RHFinger
(let ((tag "RHFinger") (num "0"))
	(set! num (d-GetKeypress))
	(if (string->number num) 
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
