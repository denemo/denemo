;;;HideStems
(let ((tag "HideStems"))
    (if (d-Directive-standalone? tag)
           (d-InfoDialog (_ "This directive turns off stems"))
	   (begin
		(d-DirectivePut-standalone tag)
		(d-DirectivePut-standalone-postfix tag  "\\hide Staff.Stem ")
		(d-DirectivePut-standalone-graphic tag "\n|x\nDenemo\n20")
		(d-DirectivePut-standalone-minpixels tag 20)
		(d-SetSaved #f)
		(d-RefreshDisplay))))
