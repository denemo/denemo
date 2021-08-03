;SyncRecordingToCursor
(if (d-GetMarkedMidiNote)
	(d-SynchronizeRecording)
	(d-WarningDialog (_ "No Recorded MIDI note is marked.")))