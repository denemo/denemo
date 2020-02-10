;;;ChangeSoundingDuration
    (let ((tag "Duration")  (big #f))
    	(if (Music?)
    		(begin
                (d-SetDurationInTicks #f)
                (set! big (d-GetUserInput (_ "Change Sounding Duration") 
				        (_ "Choose a duration in \"ticks\" (𝅘𝅥 = 384 ticks): ") (number->string (d-GetDurationInTicks))))   
                (if (and big (string->number big) (> (string->number big) 0))
                    (begin
                    (d-DirectivePut-chord-override "Duration" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
                    (d-DirectivePut-chord-prefix "Duration" (d-GetNoteDuration))
                    (d-SetDurationInTicks (string->number big))
                    (d-SetSaved #f))))))   