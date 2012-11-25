;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;IncipitFromSelection
(let ((lily ""))
  (define (accum)
    (let ((this (d-GetLilyPond)))
      (if this
	(set! lily (string-append lily this)))))
;;if no selection select the first two measures of the top staff of the first movement
  (if (not (d-GoToSelectionStart))
      (begin
	(while (d-PreviousMovement))
	(while (d-MoveToStaffUp))
	(d-MoveToBeginning)
	(d-SetMark)
	(d-MeasureRight)
	(RepeatProcWhileTest d-CursorRight (lambda () (not (Appending?))))))

  (SingleAndSelectionSwitcher accum)
  (set! lily (string-append (d-GetPrevailingClefAsLilyPond) " "  (d-GetPrevailingTimesigAsLilyPond) " "  (d-GetPrevailingKeysigAsLilyPond) " " lily))
  (set! lily (string-append "\n\\markup \\score {\n{" lily "\n}\n\\layout {indent = 0.0\\cm }\n}"))

  (SetScoreHeaderField "incipit" lily #f))
  
