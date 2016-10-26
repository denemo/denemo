;;;AddDuplicateMeasure
(d-PushClipboard)
(d-SelectMeasure)
(d-Copy)
(if (d-MoveToMeasureRight)
	(if (None?)
		#t ; Empty measure
		(d-InsertMeasureBefore)); create empty measure
	(begin ; End of staff
		(d-AppendMeasureAllStaffs)
		(d-MoveToMeasureRight)))
(d-Paste)
(d-PopClipboard)
(if AddDuplicateMeasure::params
	(begin
		(d-PushPosition)
		(while (d-PrevChordInMeasure))
		(let ((channel (d-StaffProperties "query=midi_channel")) (ticks #f) (time 0)(key #f)
		      (prognum (d-StaffProperties "query=midi_prognum")))
		      (define command (number->string (logior (string->number channel) #xC0)))
		    (d-OutputMidiBytes (string-append command  " " prognum))
		    (let loop ()
			(set! ticks (d-GetDurationInTicks))
			(if (d-GetNonprinting)
			    (begin
				(set! channel "9")
				(set! key 60))
			    (set! key (d-GetNoteAsMidi)))
			(if (and ticks key)
			    (begin
				(d-OneShotTimer time (string-append "(d-PlayMidiNote  " (number->string key) " 127 " channel " " (number->string ticks) ")"))
				(set! time (+ time ticks))))
			(if (d-NextChordInMeasure)
			    (loop))))
		(d-PopPosition)))