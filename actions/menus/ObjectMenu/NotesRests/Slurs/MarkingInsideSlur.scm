;;MarkingInsideSlur
(let ((tag "MarkingInsideSlur"))
(if (equal? MarkingInsideSlur::params "edit")
	(begin
		(d-InfoDialog (_ "This makes the next marking on a note etc move inside the slur.")))
	(begin 
		(d-DirectivePut-standalone tag)
		(d-DirectivePut-standalone-postfix tag "\\once \\override Script #'avoid-slur = #'inside
			\\once \\override Script #'outside-staff-priority = ##f ")
		(d-DirectivePut-standalone-display tag (_ "Inside Slur "))
		(d-DirectivePut-standalone-minpixels tag 30)
		(d-SetSaved #f)
		(d-RefreshDisplay))))
