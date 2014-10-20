;;;InsertGraphic
(let ((tag "InsertGraphic")(params InsertGraphic::params)(file #f))
	(if (equal? params "edit")
		(set! params #f))
	(if  params
		(set! file params))
	(if (not file)
		(set! file (d-GetUserInput "Graphic File" "Give File Name (eps)" "")))
	(if file
		(begin
			(d-Directive-standalone tag)
			(d-DirectivePut-standalone-prefix tag "<>-\\tweak #'extra-offset #'(-0.88 . -2.94)")
			(d-DirectivePut-standalone-postfix tag (string-append  "^\\markup\\epsfile #X #2 #\"" file "\" "))
			(d-DirectivePut-standalone-graphic tag "\nG\nDenemo\n30")
			(d-DirectivePut-standalone-minpixels tag 30)
			(d-SetSaved #f)
			(d-RefreshDisplay))))
