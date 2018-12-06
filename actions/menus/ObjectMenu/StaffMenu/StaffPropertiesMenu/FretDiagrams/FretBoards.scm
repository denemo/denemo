;;;FretBoards
(let ((tag "FretBoards")(choice #f))
     (if (not (d-Directive-staff? tag)) 
        (set! choice (RadioBoxMenu
                    (cons (_ "Guitar")   #f)   
                    (cons (_ "Mandolin") "mandolin")
                    (cons (_ "Ukulele") "ukulele"))))
    (ToggleDirective "voice" "postfix" tag "\n" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
    (if choice
        (begin
            (d-LilyPondInclude (string-append "predefined-" choice "-fretboards.ly"))
            (ToggleDirective "staff" "postfix" tag 
                (string-append " \\new FretBoards <<\n\\set Staff.stringTunings = #" choice "-tuning\n")  DENEMO_OVERRIDE_LILYPOND))
        (ToggleDirective "staff" "postfix" tag " \\new FretBoards <<\n"  DENEMO_OVERRIDE_LILYPOND))
    (if (and (= 1 (d-GetStaffsInMovement)) (not (GetMeasureTicks)))
        (d-WarningDialog (_ "This score cannot be typeset until you have entered some chords in this staff (which will then be typeset as fret diagrams)"))))
        

