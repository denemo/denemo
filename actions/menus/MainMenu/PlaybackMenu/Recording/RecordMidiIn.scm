;RecordMidiIn
(let ()
	(define num (d-GetMeasuresInStaff))
	(if (< num 8)
		(set! num 8))
	(d-MidiRecord)
	(if (d-RecordingMidi)
		(begin
			(d-PushPosition)
			(while (d-MoveToStaffUp))
			(if (not (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name")))
					(begin
						(d-CreateClickStaffForMidi num)
						(d-MuteStaff)))
			(d-PopPosition))))
