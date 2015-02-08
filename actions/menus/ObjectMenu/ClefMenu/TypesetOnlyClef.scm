;;;;TypesetOnlyClef
(let ((tag "TypesetOnlyClef")
        (choice (RadioBoxMenu (cons (_ "Treble") "treble")
                          (cons (_ "Bass") "bass")
                          (cons (_ "Alto") "alto")                     
                          (cons (_ "Tenor") "tenor")                   
                          (cons (_ "Treble Octava Bassa") "treble_8")
                          (cons (_ "Bass Octava Bassa") "bass_8")
                          (cons (_ "Soprano") "soprano")
                          (cons (_ "French") "french")
                          (cons (_ "LilyPond") 'lilypond))))

    (if (eq? choice 'lilypond)
                (set! choice (d-GetUserInput (_ "Custom Clef") (_ "Give LilyPond syntax for clef name") "treble^8")))
    
    (if choice
        (begin
            (d-Directive-standalone tag)
            (d-DirectivePut-standalone-postfix tag (string-append "\\clef \"" choice "\" "))
            (d-DirectivePut-standalone-graphic tag "\nD\nDenemo\n24")
            (d-DirectivePut-standalone-minpixels tag 50)
            (d-InfoDialog (_ "N.B.The Denemo display will show notes using prevailing Denemo clef,\nbut they will be typeset in the clef given"))
            (d-SetSaved #f)         
            (d-RefreshDisplay))
        (d-InfoDialog (_ "Cancelled"))))


