;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;SingleDigitTimeSig

  (if (d-Directive-timesig? "SingleDigitTimeSig")
	(d-DirectiveDelete-timesig "SingleDigitTimeSig")
	(begin
	  (d-DirectivePut-timesig-prefix "SingleDigitTimeSig" "\\once \\override Staff.TimeSignature  #'style = #'single-digit\n")
	  (d-DirectivePut-timesig-graphic "SingleDigitTimeSig" "\n1-Digit\nDenemo\n12")
	  (d-DirectivePut-timesig-gy "SingleDigitTimeSig" 0)
	  (d-DirectivePut-timesig-minpixels "SingleDigitTimeSig" 30)))
  
(d-SetSaved #f)
(d-RefreshDisplay)