;;;AddBassInversion
(let ((tag "AddBassInversion")(note AddBassInversion::params))
(if (equal? note "edit")
	(set! note #f))
  (if (Appending?)
    (d-MoveCursorLeft))
  (if (Note?)
    (begin
    (d-CursorToNthNoteHeight 1) ;;attach to lowest note only
    (if (not note)
        (begin
            (set! note (d-DirectiveGet-note-data tag))
            (if (not note)
                (set! note "Bes"))
            (set! note (d-GetUserInput (_ "Add Bass Note") (_ "Give bass note to add below root\nUse \"es\" for flat, \"is\" for sharp") note))))
    (if (string? note)
                (begin
                	(if (string-null? note)
                	 	(d-DirectiveDelete-note tag)
		        	 (begin
					    (d-DirectivePut-note-data tag note)
					    (set! note (string-downcase note))
					    (d-DirectivePut-note-postfix tag (string-append "\\withMusicProperty bass ##t " note " "))
					    (d-DirectivePut-note-display tag (string-append "/" (string-upcase note)))
					    (d-DirectivePut-note-tx tag -7)
					    (d-DirectivePut-note-ty tag 30)
					    (d-Chordize #t)))
                    		(d-SetSaved #f))))))
