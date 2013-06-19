;;;;;;;;;;;;PlayAtCursor
(let ((channel (d-StaffProperties "query=midi_channel"))
 	  (prognum (d-StaffProperties "query=midi_prognum")))
 	  (define command (number->string (logior (string->number channel) #xC0)))
		(d-OutputMidiBytes (string-append command  " " prognum))
		(let loop ((count 1))
			(define key (d-GetNoteFromTopAsMidi count))
				(if key
					(begin
						(d-PlayMidiNote key 127 (string->number channel)  1000)
						(loop (+ count 1))))))