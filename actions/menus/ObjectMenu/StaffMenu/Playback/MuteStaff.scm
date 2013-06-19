;;;MuteStaff
(let ((tag "MuteStaff")) 
	(if (equal? MuteStaff::params "delete")
		(begin
			(d-LockDirective #f)
			(set! MuteStaff::params #f)
			(d-DirectiveDelete-standalone tag)
			(d-StaffMasterVolume 1))		
		(begin
			(d-PushPosition)
			(d-MoveToBeginning)
			(if (not (d-Directive-standalone? tag))
				(begin
					(d-Directive-standalone tag)
					(d-LockDirective #t)))
			(d-DirectivePut-standalone-minpixels tag 50)
			(d-DirectivePut-standalone-gx tag 20)
			(if (> (d-StaffMasterVolume) 0)
				(begin
					(d-DirectivePut-standalone-graphic tag "Speaker_Icon_Mute")
					(d-StaffMasterVolume 0))
				(begin
					(d-StaffMasterVolume 1)
					(d-DirectivePut-standalone-graphic tag "Speaker_Icon")))
			(d-SetSaved #f)
			(d-PopPosition)))
   (d-RefreshDisplay))