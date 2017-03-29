;;;;; TransposeStaffPrint
(define-once Transpose::Interval "c ees")
(let ((lily #f) (text #f)(params TransposeStaffPrint::params))
    (if (and params  (not (equal? params "edit")))
        (set!  Transpose::Interval params)
        (set! Transpose::Interval  (d-GetNoteNamesFromUser 2 Transpose::Interval (_ "<-- Transpose to -->") )))
    (if Transpose::Interval
        (begin
            (set! lily (string-append  "\\transpose " Transpose::Interval " "))
            (set! text (string-append  (_ "Print transposed:  ") Transpose::Interval " "))
            (d-PushPosition)
            (while (d-MoveToVoiceUp))
            (let loop ()
                (d-DirectivePut-voice-override  "TransposeStaffPrint" DENEMO_OVERRIDE_GRAPHIC)
                (d-DirectivePut-voice-display  "TransposeStaffPrint" text)
                (d-DirectivePut-voice-prefix  "TransposeStaffPrint" lily)
                 (if (d-MoveToVoiceDown)
                     (loop)))
            (d-PopPosition)
            (d-SetSaved #f))
        (d-WarningDialog (_ "Cancelled"))))