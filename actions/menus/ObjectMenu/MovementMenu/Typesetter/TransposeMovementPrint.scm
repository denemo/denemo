;;;; TransposeMovementPrint
(define-once Transpose::Interval "c ees")
(let ((text #f) (tag "TransposeMovementPrint"))
    (if (and TransposeScorePrint::params (not (equal?  TransposeScorePrint::params "edit")))
        (set! Transpose::Interval TransposeScorePrint::params)
        (set! Transpose::Interval (d-GetUserInput (_ "Set Transpose Interval") (_ "Give Interval to transpose by as two note names, 
         for example \"c g\" means transpose 5th up.
        Note names are in Dutch!!! a,b,c ... are the same but
        \"es\" = flat, so e.g. bes means b-flat
        \"is\" = sharp so e.g fis means f-sharp
        Use commas for octave(s) down, 
        single-quotes for octave(s) up
        e.g. c c' means octave up.
        You do not have to start with c
        e.g. d e means a tone higher.
        ") Transpose::Interval)))
    (if Transpose::Interval
        (begin
             (d-DirectivePut-movementcontrol-prefix tag (string-append  "\\transpose " Transpose::Interval " "))
          (set! text (string-append  "Print transposed:  " Transpose::Interval " ")) 
          (d-DirectivePut-movementcontrol-display tag text)
          (d-DirectivePut-movementcontrol-override tag DENEMO_OVERRIDE_AFFIX)
          (d-SetSaved #f))))

