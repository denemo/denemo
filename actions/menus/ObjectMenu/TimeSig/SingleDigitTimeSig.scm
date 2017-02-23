;;;SingleDigitTimeSig
  (if (d-Directive-timesig? "SingleDigitTimeSig")
	(let ((choice (RadioBoxMenu 
                (cons (_ "Inspect/Edit Single Digit Directive") 'help) 
                (cons (_ "Delete") 'delete))))
            (case choice
                ((help)
                  (if (Appending?) (d-MoveCursorLeft))
                   (if (Timesignature?)
                   	(d-DisplayCurrentObject)
                   	(d-EditStaffProperties)))
                  ((delete)
                    (d-DirectiveDelete-timesig "SingleDigitTimeSig"))))
	(begin
	  (d-DirectivePut-timesig-prefix "SingleDigitTimeSig" "\\once \\override Staff.TimeSignature  #'style = #'single-digit\n")
	  (d-DirectivePut-timesig-graphic "SingleDigitTimeSig" "\n1-Digit\nDenemo\n12")
	  (d-DirectivePut-timesig-gy "SingleDigitTimeSig" 0)
	  (d-DirectivePut-timesig-minpixels "SingleDigitTimeSig" 30)))
(d-SetSaved #f)
(d-RefreshDisplay)