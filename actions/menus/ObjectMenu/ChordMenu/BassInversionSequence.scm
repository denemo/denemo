(let ((tag "BassInversionSequence")(notes "")(notenames "")(good #f))
    (if (d-Directive-standalone? tag)
        (begin
            (d-InfoDialog (_ "This directive places a sequence of bass note names after the next chord. You can delete it with the usual backspace and del commands.")))
        (begin 
           (if (Appending?)
            (if (d-MoveCursorLeft)
                (if (Note?)
                    (set! good #t)))
            (if (Note?)
                (set! good #t)))
            (if good
                (begin
                    (d-MoveCursorLeft)
                    (d-LilyPondInclude "note-name-markup.ily")
                    (let loop ()
                        (define note (d-GetUserInput (_ "Note Name") (_ "Give Next Note or Blank to finish") "c"))
                        (if (and note (not (string-null? note)))
                            (begin
                                (set! notenames (string-append notenames "/" note))
                                (set! notes (string-append notes (string-append " #(denemo-bass-inversion (ly:pitch-transpose (ly:make-pitch -1 "
                                                                        (number->string (GetStep note)) " " (number->string (GetAccidental note)) 
                                                                        " ) (ly:make-pitch -1 DenemoTransposeStep DenemoTransposeAccidental))) ")))
                            (loop))))
                    (if (and notes (not (string-null? notes)))
                        (begin
                            (d-SetSaved #t)
                            (StandAloneDirectiveProto (cons tag  (string-append "-\\tweak word-space 0.1 _\\markup\\with-dimensions #'(-2 . 2) #'(-2 . 2) \\line {" notes "}")) #f (string-append "\n" notenames "\nDenemo 8") "\\BASS INV")
                            (d-DirectivePut-standalone-prefix tag "<>-\\tweak #'extra-offset #'(4 . -2)")
                            (d-DirectivePut-standalone-data tag "'((x-offset . \"4\") (y-offset . \"-2\"))")
                            (d-DirectivePut-standalone-gx tag 20)
                            (d-DirectivePut-standalone-gy tag 60)
                            (d-MoveCursorRight)
                            (d-MoveCursorRight))))
                (d-WarningDialog (_ "Place the cursor on chord to which bass sequence belongs"))))))
