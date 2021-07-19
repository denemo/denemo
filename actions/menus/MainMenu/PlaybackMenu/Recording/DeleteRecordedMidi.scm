;;;DeleteRecordedMidi
(let ((recording (d-RecordingMidi)))
	(d-DeleteImportedMidi)
	(if recording
		(d-MidiRecord)))
