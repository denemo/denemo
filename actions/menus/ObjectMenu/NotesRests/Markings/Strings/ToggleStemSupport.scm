;;;ToggleStemSupport
(let ((tag "StemSupport"))
(if (d-Directive-voice? tag)
	(begin
		(d-InfoDialog "Collisions of fingerings etc with stems allowed")
		(d-DirectiveDelete-voice tag))
	(begin
	(d-DirectivePut-voice-postfix tag
	"\\override Fingering #'add-stem-support = ##t
 	 \\override StringNumber #'add-stem-support = ##t
 	 \\override StrokeFinger #'add-stem-support = ##t
	"))))
(d-SetSaved #f)

