;;;AddBassInversion
(let ((tag "AddBassInversion")(note AddBassInversion::params))
  (if (Appending?)
    (d-MoveCursorLeft))
  (if (Note?)
    (begin
    (if (not note)
        (begin
            (set! note (d-DirectiveGet-note-data tag))
            (if (not note)
                (set! note "Bes"))
            (set! note (d-GetUserInput (_ "Add Bass Note") (_ "Give bass note to add below root\nUse \"es\" for flat, \"is\" for sharp") note))))
    (if (string? note)
        (if (d-Directive-score? "CompactChordChart")
            (begin
                (set! note (string-downcase note))
                (d-DirectivePut-standalone tag)
                (d-DirectivePut-standalone-minpixels tag 20)
                (d-DirectivePut-standalone-prefix tag "<>-\\tweak #'extra-offset #'(2 . -6.5)")
                (d-DirectivePut-standalone-postfix tag (string-append "^\\markup\\scale #'(2.5 . 2)\\column{\\line\\large{ \"/\" \\hspace #-0.5 \\score{
    \\DenemoGlobalTranspose {
    \\new ChordNames { " note "} }  \\layout{ \\context {\\Score
            chordRootNamer = #denemo-chord-inv-name->pop-markup }  indent=-2 }    
    }}
    }"))
                (d-DirectivePut-standalone-graphic tag (string-append "\n/" note "\nDenemo\n24"))
                (d-DirectivePut-standalone-gx tag 15)
                (d-DirectivePut-standalone-gy tag 60)
                (d-DirectivePut-standalone-minpixels tag 30)
                (d-SetSaved #f))
                (begin
                    (d-DirectivePut-note-data tag note)
                    (set! note (string-downcase note))
                    (d-DirectivePut-note-postfix tag (string-append "\\withMusicProperty bass ##t " note " "))
                    (d-DirectivePut-note-display tag (string-append "/" (string-upcase note)))
                    (d-DirectivePut-note-tx tag -7)
                    (d-DirectivePut-note-ty tag 30)
                    (d-Chordize #t)
                    (d-SetSaved #f)))))))
