;;;;; TransposeStaffPrint
(define-once Transpose::Interval "c ees")
(if (d-HasFigures)
	(d-TransposeVoices)
	(let ((tag "TransposeStaffPrint")(lily #f) (text #f)(params TransposeStaffPrint::params))
	    (if (and params  (not (equal? params "edit")))
		(set!  Transpose::Interval params)
		(set! Transpose::Interval  (d-GetNoteNamesFromUser 2 Transpose::Interval (_ "--> Transpose to -->") )))
	    (if Transpose::Interval
		(begin
		    (set! lily (string-append  "\\transpose " Transpose::Interval " "))
		    (set! text (string-append  (_ "Print transposed:  ") Transpose::Interval " "))
		    (d-DirectivePut-staff-override tag  DENEMO_OVERRIDE_GRAPHIC)
		    (d-DirectivePut-staff-display  tag text)
		    (d-DirectivePut-staff-prefix  tag lily)
		    (d-SetSaved #f))
		(d-WarningDialog (_ "Cancelled")))))
