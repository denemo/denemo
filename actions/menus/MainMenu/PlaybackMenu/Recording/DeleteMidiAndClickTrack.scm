;DeleteMidiAndClickTrack
(let ((pos (GetPosition)))
	(d-DeleteImportedMidi)
	(while (d-MoveToStaffUp))
	(if (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name"))
		(begin
			(d-DeleteStaff)
			(d-StaffSetSpaceAbove 20)))
			(d-GoToPosition #f (- (list-ref pos 1) 1) (list-ref pos 2) (list-ref pos 3)))