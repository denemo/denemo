;;ChordChangesOnly
(let ((tag "ChordChangesOnly"))
	(if (d-Directive-standalone? tag)
		(d-InfoDialog (_ "This Directive turns off repeations of the chord except at the start of a new line"))
		(begin
			(d-Directive-standalone tag)
			(d-DirectivePut-standalone-postfix tag "\\set chordChanges = ##t  ")
			(d-DirectivePut-standalone-display tag (_ "Chord Changes Only"))
			(d-DirectivePut-standalone-minpixels tag 30)
			(d-SetSaved #f)(d-RefreshDisplay))))
