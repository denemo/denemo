;;DeleteLastRecordedMidi
(let ((pos (d-GetMarkedMidiNote)))
	(d-SetMarkedMidiNote -1)
	(d-PlayMarkedMidi)
	(d-DeleteLastRecordedNote)
	(d-SetMarkedMidiNote pos))