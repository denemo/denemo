;;;;CloseLilyBlock
(let ((tag  "CloseLilyBlock")(params "CloseLilyBlock::params"))
(if (equal? params "edit")
	(d-InfoDialog (_ "This Denemo Directive closes a block of music with a close curly brace }. It must be preceded earlier in the save voice by a corresponding open {, otherwise the music will not typeset"))
	(begin
 		(d-Directive-standalone tag)
 		(d-DirectivePut-standalone-postfix tag "}")
		(d-DirectivePut-standalone-graphic tag "
}
denemo
40")
		(d-DirectivePut-standalone-minpixels tag 30)
		(d-DirectivePut-standalone-gy tag 10)
		(d-RefreshDisplay)
		(d-SetSaved #f))))