;;;ShowStems
(let ((tag "ShowStems"))
    (if (d-Directive-standalone? tag)
      	(d-InfoDialog (_ "This directive turns on typesetting of stems on notes."))
	(begin
		(d-DirectivePut-standalone tag)
		(d-DirectivePut-standalone-postfix tag  "\\undo \\hide Staff.Stem ")
		(d-DirectivePut-standalone-graphic tag "\n|\nDenemo\n20")
		(d-DirectivePut-standalone-minpixels tag 20)
		(d-SetSaved #f)
		(d-RefreshDisplay))))
