;;AllowAmbitus
(let ((tag "Ambitus")(criterion #f))
    (define (doAmbitus choice)
      (while (d-PreviousMovement))
      (let movement ()
		(while (d-MoveToStaffUp))
		(let staff () (disp "Position " (GetPosition) "\n\n")
		  ;;skip voices? mirrored staffs?
			(if (eq? choice 'delete)
				(d-DirectiveDelete-staff tag)
				(begin
					 (d-DirectivePut-staff-prefix tag "\\consists \"Ambitus_engraver\"\n")
					 (d-DirectivePut-staff-override tag  (logior DENEMO_ALT_OVERRIDE  DENEMO_OVERRIDE_AFFIX  DENEMO_OVERRIDE_GRAPHIC))
					 (if criterion
						(d-DirectivePut-staff-ignore tag (cdr (d-GetIncludeCriterion))))))
			 (if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
				(staff)))
	    (if (d-NextMovement)
			(movement))))
			
     (d-PushPosition)
	 (let ((choice (RadioBoxMenu (cons (_ "Set Conditional Ambitus") 'conditional) 
			(cons (_ "Set Unconditional Ambitus") 'set)
			(cons (_ "Delete Ambitus") 'delete)
			(cons (_ "Cancel") #f))))
		(if choice
			(begin
				(if (eq? choice 'conditional)
					(begin
						(set! criterion (_ "Ambitus"))
						(d-CreateIncludeCriterion criterion)
						(d-SetIncludeCriterion criterion)))
				(doAmbitus choice))
			(d-WarningDialog (_ "Cancelled"))))
    (d-PopPosition))
    