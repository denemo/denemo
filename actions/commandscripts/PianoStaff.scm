;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;PianoStaff
(let ((name #f) (del (and (None?)
	(equal? (d-StaffProperties "query=denemo_name") "Unnamed"))))
	(d-AddAfter)
	(d-StaffProperties (_ "RH"))
	(d-PianoStaffStart)
	(d-PianoStaffName)
	(d-AddAfter)
	(d-InitialClef "Bass")
	(d-StaffProperties (_ "LH"))
	(d-PianoStaffEnd)
	(if del
		(begin
			(d-MoveToStaffUp)
			(d-MoveToStaffUp)
			(d-DeleteStaff))))

