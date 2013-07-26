;;TextInsideSlur
(let ((tag "TextInsideSlur"))
(if (equal? TextInsideSlur::params "edit")
	(begin
		(d-InfoDialog (_ "This makes the next text marking on a note etc move inside the slur.")))
	(begin 
		(d-DirectivePut-standalone tag)
		(d-DirectivePut-standalone-postfix tag "\\once \\override TextScript #'avoid-slur = #'inside
			\\once \\override TextScript #'outside-staff-priority = ##f ")
		(d-DirectivePut-standalone-display tag (_ "Inside Slur "))
		(d-DirectivePut-standalone-minpixels tag 30)
		(d-SetSaved #f)
		(d-RefreshDisplay))))
