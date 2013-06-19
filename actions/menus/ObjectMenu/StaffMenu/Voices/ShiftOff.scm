;;;ShiftOff
(if (d-Directive-standalone?  "ShiftForVoices")
	(d-DirectiveDelete-standalone  "ShiftForVoices"))
(d-DirectivePut-standalone "ShiftForVoices")
(d-DirectivePut-standalone-postfix "ShiftForVoices" "\\shiftOff")
(d-DirectivePut-standalone-display "ShiftForVoices" (_ "No Shift"))
(d-DirectivePut-standalone-minpixels "ShiftForVoices" 30)
(d-RefreshDisplay)
