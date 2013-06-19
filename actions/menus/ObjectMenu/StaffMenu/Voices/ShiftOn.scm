;;;ShiftOn
(if (d-Directive-standalone?  "ShiftForVoices")
	(d-DirectiveDelete-standalone  "ShiftForVoices"))
(d-DirectivePut-standalone "ShiftForVoices")
(d-DirectivePut-standalone-postfix "ShiftForVoices" "\\shiftOn")
(d-DirectivePut-standalone-display "ShiftForVoices" (_ "Shift"))
(d-DirectivePut-standalone-minpixels "ShiftForVoices" 30)
(d-RefreshDisplay)
