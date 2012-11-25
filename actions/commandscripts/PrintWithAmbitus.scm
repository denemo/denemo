;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;;;;;;;PrintWithAmbitus
(define (PrintWithAmbitus::ambitize)
  (while (d-PreviousMovement))
  (let movement ()
    (while (d-MoveToStaffUp))
    (let staff ()
   	(ToggleDirective "staff" "prefix" "Ambitus" "\\with { \\consists \"Ambitus_engraver\" }")	
 	 (if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
		(staff)))
		(if (d-NextMovement)
		    (movement))))

(d-PushPosition)
(PrintWithAmbitus::ambitize)
(d-TypesetForScript "(d-PrintWithAmbitus)")
(PrintWithAmbitus::ambitize)
(d-PopPosition)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
