;;;;;;; NotForLayout
 (let ((tag (d-DirectiveGetTag-standalone)) ( id (d-GetLayoutId)))
  (if tag
  	(let ()
  		(define text (string-append (_ "Not for ") (d-GetLayoutName)))
  		(d-DirectivePut-standalone-display tag text)
  		(d-DirectivePut-standalone-ty tag -50)
  		(d-DirectivePut-standalone-tx tag -30)
	(d-DirectivePut-standalone-x tag id))))
	(d-SetSaved #f)
	(d-RefreshDisplay)