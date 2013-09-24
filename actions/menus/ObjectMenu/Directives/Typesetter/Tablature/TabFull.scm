;;;TabFull
(let ((tag "TabFull"))
(if (d-Directive-standalone? tag)
	(begin
		(d-InfoDialog (_ "This directive causes the tab to be notated fully with beaming and other notation added\nDelete the directive to undo the effect.")))
	(begin
	(if (d-Directive-staff? "TabStaff")
	   (begin
		(d-DirectivePut-standalone tag)
		(d-DirectivePut-standalone-postfix tag "\\tabFullNotation")
		(d-DirectivePut-standalone-display tag tag)
		(d-DirectivePut-standalone-minpixels tag 30)
		(d-SetSaved #t)
		(d-RefreshDisplay))
	  (begin
	  	(d-WarningDialog (_ "This Directive can only be added in a Tab Staff. See Staff menu.")))))))
