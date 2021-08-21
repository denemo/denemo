;;;DeleteRecordedMidi
(let ((recording (d-RecordingMidi)))
	(d-DeleteImportedMidi)
	(d-MoveToEarliestEmptyMeasure)
	(d-MoveCursorLeft)
	(if recording
		(d-MidiRecord)))