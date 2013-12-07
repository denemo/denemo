;;;NonPrintingStaff
(let ((tag "DropMusic") (staff-tag "NonPrintingStaff"))
		(if (d-DirectiveGetForTag-staff staff-tag)
			(begin
				(d-DirectiveDelete-staff staff-tag))
			(begin 
				(d-DirectivePut-staff-prefix staff-tag " \\void ")
				(d-DirectivePut-staff-override staff-tag  (logior DENEMO_ALT_OVERRIDE DENEMO_OVERRIDE_GRAPHIC))
				(d-DirectivePut-staff-display staff-tag "Hidden Staff")))
				(d-SetSaved #f))