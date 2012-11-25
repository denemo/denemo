;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;NewStaffAfter
(let ((name #f))
	(d-AddAfter)
	(d-InstrumentName)
	(set! name (d-DirectiveGet-staff-display "InstrumentName"))
	(if name	
			(d-StaffProperties (string-append "denemo_name=" name))))