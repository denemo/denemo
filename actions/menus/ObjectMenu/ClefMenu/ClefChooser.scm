;;;;ClefChooser
(let ((choice (RadioBoxMenu (cons (_ "Treble") "treble")
                          (cons (_ "Bass") "bass")
                          (cons (_ "Alto") "alto")                     
                          (cons (_ "Tenor") "tenor")                   
                          (cons (_ "Treble Octava Bassa") "Treble Octava bassa")
                          (cons (_ "Bass Octava Bassa") "Bass Octava bassa")
                          (cons (_ "Soprano") "Soprano")
                          (cons (_ "Baritone") "Baritone")                     
                          (cons (_ "French") "French")
                          (cons (_ "Drum") 'drum)
                          (cons (_ "LilyPond") 'lilypond))))

    (if choice
        (begin
        (case choice
            ((lilypond)
                (let ((clef (d-GetUserInput (_ "Custom Clef") (_ "Give LilyPond syntax for clef name") "treble^8"))  (tag "ClefChooser"))
                    (if clef
                        (begin
                            (d-Directive-standalone tag)
                            (d-DirectivePut-standalone-postfix tag (string-append "\\clef \"" clef "\" "))
                            (d-DirectivePut-standalone-graphic tag "\nC\nDenemo\n24")
                            (d-DirectivePut-standalone-minpixels tag 50)
                            (d-InfoDialog (_ "N.B.The Denemo display will show notes using prevailing Denemo clef,\nbut they will be typeset in the clef given"))))))
            ((drum)
                (d-InitialClef "Bass")
                (d-DirectivePut-clef-override "DrumClef" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
                (d-DirectivePut-clef-postfix "DrumClef" "\\clef percussion\n ")
                (d-DirectivePut-clef-graphic "DrumClef" "DrumClef")
                (d-DirectivePut-clef-gy "DrumClef" -5)
                (d-StaffProperties "midi_channel=9")
                (d-DirectivePut-voice-display "DrumClef" (_ "Drum Clef"))
                (d-DirectivePut-voice-override "DrumClef" DENEMO_OVERRIDE_GRAPHIC)
                (d-DirectivePut-voice-prefix "DrumClef" "\\new Voice \\with { middleCPosition = #6 }"))
            (else
                (d-DirectiveDelete-voice "DrumClef")
                (d-DirectiveDelete-clef "DrumClef")
                (if (d-MoveCursorLeft)
                    (begin
                    (d-MoveCursorRight)
                    (d-InsertClef choice))
                    (begin
                        (if (equal? (d-DirectiveGetTag-clef ) "DrumClef" ) (d-DirectiveDelete-clef "DrumClef") )
                        (d-InitialClef choice)))))
                    (d-SetSaved #f)           
                    (d-RefreshDisplay))))
