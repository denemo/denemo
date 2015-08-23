;;NewVoice
(let ((name (d-StaffProperties "query=denemo_name")))
	(d-NewStructuredStaff)
	(d-StaffProperties (string-append "denemo_name=" name) )
	(d-SetCurrentStaffAsVoice))