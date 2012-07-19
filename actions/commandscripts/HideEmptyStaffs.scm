  ;;;;;;;;;;;;;;;Toggle HideEmptyStaffs
(let ((current "0.0"))
  (set! current (d-DirectiveGet-score-prefix "HideEmptyStaffs"))
  (if (boolean? current)
      (begin
	(d-DirectiveDelete-score "HideEmptyStaffs")
	(d-DirectivePut-score-prefix "HideEmptyStaffs" 
				     "\\layout {
                                     \\context { \\RemoveEmptyStaffContext }
                                     }"
				     )
         (d-DirectivePut-score-override  "HideEmptyStaffs" DENEMO_OVERRIDE_GRAPHIC)
	(d-DirectivePut-score-display  "HideEmptyStaffs" "Hide Empty Staffs"))
      (d-DirectiveDelete-score "HideEmptyStaffs"))
  (d-RefreshDisplay))

