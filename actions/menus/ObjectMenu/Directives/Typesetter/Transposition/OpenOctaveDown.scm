;;;;OpenOctaveDown
(let ((tag  "OpenOctaveDown")(params "OpenOctaveDown::params"))
(if (equal? params "edit")
	(d-InfoDialog (_ "This Denemo Directive starts block of music that will be transposed up one octave. The block of music should be closed with a curly brace }.t"))
	(begin
 (d-Directive-standalone tag)
 (d-DirectivePut-standalone-postfix tag "\\transpose c c,{")
(d-DirectivePut-standalone-display tag (_ "Down8va"))
		(d-DirectivePut-standalone-graphic tag "
{
denemo
40")
(d-DirectivePut-standalone-minpixels tag 30)
(d-RefreshDisplay)
		(d-DirectivePut-standalone-gy tag 10)
		(d-RefreshDisplay)
		(d-SetSaved #f))))