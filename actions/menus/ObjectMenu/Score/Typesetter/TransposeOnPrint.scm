;;;TransposeOnPrint
(define-once Transpose::Interval #f)
(let ((action (RadioBoxMenu (cons (_ "Transpose Score") d-TransposeScorePrint) (cons (_ "Transpose Staff") d-TransposeStaffPrint))))
    (if action
        (let ((interval (d-GetNoteNamesFromUser 2 Transpose::Interval (_ "--- Transpose to -->") )))
            (if interval
                (action interval)))))
