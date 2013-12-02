;;;RecordAndConvert
(define DenemoClickTrack (_ "Click"))
(let ((position (GetPosition)))
(while (d-MoveToStaffUp))
(if  (not (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name")))
	(begin
		(d-InitialTimeSig)
		(d-CreateClickStaffForMidi)
		(d-GoToPosition #f  (+ 1 (list-ref position 1)) (list-ref position 2)  (list-ref position 3)))
	(begin
		(d-GoToPosition #f (list-ref position 1) (list-ref position 2)  (list-ref position 3))))	
(d-MidiRecord))

