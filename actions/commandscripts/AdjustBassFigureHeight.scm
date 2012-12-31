;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;AdjustBassFigureHeight
(let ((tag "AdjustBassFigureHeight"))
	(define offset #f)
	(if (defined? 'AdjustBassFigureHeight::params)
		(set! offset AdjustBassFigureHeight::params))
	(if (not offset)
		(set! offset (d-GetUserInput (_ "Figured Bass Height") (_ "Give height adjustment (unit = staff space): ") "1.0")))
	(if (and (string? offset) (string->number offset))
		(begin
			(d-DirectivePut-note-prefix tag (string-append "\\once \\override Staff.BassFigureAlignmentPositioning #'Y-offset = #'" offset " "))
			(d-DirectivePut-note-override tag DENEMO_ALT_OVERRIDE)
			(d-DirectivePut-note-display tag "^")
			(d-DirectivePut-note-ty tag -10)
			(d-RefreshDisplay)
			(d-SetSaved #f))))