;;;"Void"
(let ((tag "Void"))
	(d-Directive-standalone tag)
	(d-DirectivePut-standalone-postfix tag "\\void ")
	(d-DirectivePut-standalone-display tag (_ "Hide"))
	(d-DirectivePut-standalone-minpixels tag 30)
	(d-SetSaved #f)
	(d-RefreshDisplay))