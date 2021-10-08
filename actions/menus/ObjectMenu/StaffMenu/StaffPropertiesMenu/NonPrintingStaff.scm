;;;NonPrintingStaff
(let ((tag "NonPrintingStaff")(params NonPrintingStaff::params));(disp "params " params " so eq set? " (eq? params 'set) (eq? params 'unset) "\n")
     (if (eq? params 'unset)
     	(begin ;(disp "deleting for " params "\n") 
     		(d-DirectiveDelete-staff tag))
     	(begin
     		(if (eq? params 'set)
     			(d-DirectiveDelete-staff tag))
		(if (d-DirectiveGetForTag-staff tag)
			(begin
				(if (not params)
					(d-InfoDialog (_ "Staff will be printed")))
				(d-DirectiveDelete-staff tag))
			(begin 
				(d-DirectivePut-staff-prefix tag " \\void { ")
				(d-DirectivePut-staff-postfix tag " }\n ")
				(d-DirectivePut-staff-override tag  (logior DENEMO_ALT_OVERRIDE DENEMO_OVERRIDE_GRAPHIC))
				(d-DirectivePut-staff-display tag "Hidden Staff")
				(if (not params)
					(SetDirectiveConditional "staff"  tag))))
				(d-SetSaved #f))))
