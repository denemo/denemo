;AdjustTempoToMidiRecording
(define-once AdjustTempoToMidiRecording::start #f)
(define-once AdjustTempoToMidiRecording::markstart #f)
(let ()
	(define (get-earliest-time)
		(define thetime (d-GetMidiOnTime))
		(if (not thetime)
			(set! thetime (d-GetTimeAtCursor)))
		thetime)
	(if (not AdjustTempoToMidiRecording::start)
		(begin
			(d-SyncRecordingToCursor)
			(set! AdjustTempoToMidiRecording::markstart (d-GetMarkedMidiNoteSeconds))
			(set! AdjustTempoToMidiRecording::start (get-earliest-time))
			(d-InfoDialog (string-append (_ "Time at Denemo Cursor ") (format #f "~1,1f" AdjustTempoToMidiRecording::start) "\n"
					 (_ "Time at Marked MIDI note ") (format #f "~1,1f" AdjustTempoToMidiRecording::markstart) " seconds.\n"	
					 (_ "Move the Denemo Cursor to another position and click on the MIDI note that should appear at that position\nAnd then re-execute this command."))))
		(let ((message "Re-scaling Done")(time (get-earliest-time))(marktime (d-GetMarkedMidiNoteSeconds)))
			
			(if (and time marktime (> time AdjustTempoToMidiRecording::start) (> marktime AdjustTempoToMidiRecording::markstart))
				(let ( (factor (/ (- time AdjustTempoToMidiRecording::start) (- marktime AdjustTempoToMidiRecording::markstart))))
					(if (and (< factor 10) (> factor 0.1))
							(let ((leadin (d-MidiRecordingOffset)))
								(d-MovementTempo (* factor (d-MovementTempo)))
								;set leadin scaled...
								(d-MidiRecordingOffset (/ leadin factor)))
							(set! message (_ "Re-scaling out of range"))))
				(set! message (_ "Wrongly ordered adjustement points")))
			(d-InfoDialog message)	
			(set! AdjustTempoToMidiRecording::start #f))))