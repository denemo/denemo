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
                (set! note "C"))
            (set! note (d-GetUserInput (_ "Add Bass Note") (_ "Give bass note to add below root") note))))
    (if (string? note)
        (begin
            (d-DirectivePut-note-data "tag" note)
            (set! note (string-downcase note))
            (d-DirectivePut-note-postfix "tag" (string-append "\\withMusicProperty bass ##t " note " "))
            (d-DirectivePut-note-display "tag" (string-append "/" (string-upcase note)))
            (d-DirectivePut-note-tx "tag" -7)
            (d-DirectivePut-note-ty "tag" 30)
            (d-Chordize #t)
            (d-SetSaved #f)
            (d-RefreshDisplay))))))
