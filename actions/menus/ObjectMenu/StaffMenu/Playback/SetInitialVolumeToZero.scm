;;;SetInitialVolumeToZero
(d-PushPosition)
(d-MoveToBeginning)
(if (and (d-Directive-standalone? "Mute") (> (d-DirectiveGet-standalone-override "Mute") 0))
	(begin
	(d-SetSaved #f)
	(d-DirectivePut-standalone-midibytes "Mute" "")
	(d-DirectivePut-standalone-override "Mute" 0)
	(d-DirectivePut-standalone-graphic "Mute" "Speaker_Icon"))
	(begin
	(d-DirectivePut-standalone "Mute")
	(d-DirectivePut-standalone-graphic "Mute" "Speaker_Icon_Mute")
	(d-DirectivePut-standalone-midibytes "Mute" "0")
	(d-DirectivePut-standalone-minpixels "Mute" 50)
	(d-DirectivePut-standalone-gx "Mute" 20)
	(d-DirectivePut-standalone-override "Mute" (logior DENEMO_OVERRIDE_VOLUME DENEMO_OVERRIDE_STEP))	
	(d-SetSaved #f)))
(d-PopPosition)
(d-RefreshDisplay)