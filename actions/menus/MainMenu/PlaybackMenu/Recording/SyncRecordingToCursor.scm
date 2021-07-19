;;SyncRecordingToCursor
(if (d-GetMarkedMidiNote)
	(begin
		(d-ExtendClickTrack)
		(d-SynchronizeRecording))
	(d-WarningDialog (_ "No Recorded MIDI note is marked.")))