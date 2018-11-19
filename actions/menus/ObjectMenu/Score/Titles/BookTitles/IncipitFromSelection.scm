;;;IncipitFromSelection
(let ((lily ""))
  (define (accum)
    (let ((this (d-GetLilyPond)))
      (if this
    (set! lily (string-append lily this)))))
;;if no selection select the first two measures of the top staff of the first movement
  (d-DirectiveDelete-scoreheader "ScoreIncipit")
  (if (not (d-GoToSelectionStart))
      (begin
        (while (d-PreviousMovement))
        (while (d-MoveToStaffUp))
        (d-MoveToBeginning)
        (accum)
        (while (d-NextObject)
          (if (< (d-GetMeasure) 3)
            (accum))))
      (begin
          (SingleAndSelectionSwitcher accum)))
  ;;;avoid mis-matched curly braces in lily
  (let ((open (string-count lily #\{))(close (string-count lily #\})))
    (let loop ((count (- open close)))
      (if (> count 0)
        (begin
          (set! lily (string-append lily "}"))
          (loop (1- count))))))
  (set! lily (string-append (d-GetPrevailingClefAsLilyPond) " "  (d-GetPrevailingTimesigAsLilyPond) " "  (d-GetPrevailingKeysigAsLilyPond) " " lily))
  (set! lily (string-append "\n\\markup \\score {\\DenemoGlobalTranspose\n{" lily "\n}\n\\layout {\nindent = 0.0\\cm\n #(layout-set-staff-size 18)\n}}"))
  (d-SetSaved #f)
  (SetScoreHeaderField "incipit" (_ "Incipit") #f lily))