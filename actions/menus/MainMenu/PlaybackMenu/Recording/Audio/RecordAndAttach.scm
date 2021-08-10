;RecordAndAttach - Record (Start/Stop) and Attach at Cursor
(define-once RecordAndAttach::position '(1 1 1 1))
(define-once RecordAndAttach::explain #t)
(define-once RecordAndAttach::deleteClickTrack #f)
(let ((filename (string-append DENEMO_LOCAL_ACTIONS_DIR "audio-recording.wav")))
	(d-ToggleRecordingAudio)
	(if (d-RecordingAudio)
		(begin
			(set! RecordAndAttach::position (GetPosition))
			(d-MidiInListening)
			(while (d-MoveToStaffUp))
			(if (not (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name")))
				(begin
					(d-CreateClickStaffForMidi (max 8 (d-GetMeasuresInStaff)))
					(d-GoToPosition #f (1+ (list-ref RecordAndAttach::position 1))(list-ref RecordAndAttach::position 2)(list-ref RecordAndAttach::position 3))
					(set! RecordAndAttach::position (GetPosition))))
			(d-GoToPosition #f 1 (list-ref RecordAndAttach::position 2) (list-ref RecordAndAttach::position 3))
			(if RecordAndAttach::explain
				(d-InfoDialog (_ "Play music you want to insert next\nThen re-issue this command."))))
		(begin
			(d-ExportRecordedAudio filename)
			(if (EmptyMeasure?)
				(d-2))
			(d-OpenSourceAudioFile filename)
			
			(if RecordAndAttach::explain
				(let ((confirm #f))
					(set! RecordAndAttach::explain #f)
					(d-InfoDialog (_ "Now the music is attached to the score starting at the cursor position.
The timing of the notes is shown at the top of the score.
You can drag the timing of start of the music by left-clicking above the blue line.
You can insert the rhythm in your staff and check by invoking Play.
When that's correct, press the Shift key to switch to Inserting/Appending Pitches and play in the music again."))))
			(if RecordAndAttach::deleteClickTrack
				(begin
					(d-GoToPosition #f 1 1 1)
					(d-DeleteStaff)))
			(apply d-GoToPosition RecordAndAttach::position))))
 
