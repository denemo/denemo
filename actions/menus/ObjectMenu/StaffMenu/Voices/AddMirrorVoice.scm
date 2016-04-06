;;AddMirrorVoice
(let ((tag "help") (name (d-StaffProperties "query=denemo_name")))
	(d-NewStructuredStaff)
	(d-StaffProperties (string-append "denemo_name=" name) )
	(d-SetCurrentStaffAsVoice)
	(d-SubstituteMusic)
	(d-Directive-standalone tag)
	(d-DirectivePut-standalone-graphic tag (string-append "\n"
	(_ "Music here is mirrored from ") (d-DirectiveGet-voice-display "SubstituteMusic") "\nDenemo\n20"))
	(d-RefreshDisplay))