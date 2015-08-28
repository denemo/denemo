;;;InsertChordDirective
(let ((params InsertChordDirective::params)
    (replace #f)(choice 'new)
    (currentpost (d-DirectiveGet-chord-postfix (d-DirectiveGetTag-chord)))
    (currentpre (d-DirectiveGet-chord-prefix (d-DirectiveGetTag-chord))))
    (if(or  currentpre currentpost)
      (begin
        (if (not currentpre) (set! currentpre ""))
        (if (not currentpost) (set! currentpost ""))
        (set! choice (RadioBoxMenu
                     (cons (_ "New LilyPond Directive")   'new)   
                     (cons (string-append (_ "Edit ") currentpre " | " currentpost) 'edit)
                     (cons (string-append (_ "Delete ") currentpre " | " currentpost) 'delete)
                     (cons (_ "Advanced") 'advanced)))               
        (case choice
            ((advanced) 
                (d-DirectiveTextEdit-chord (d-DirectiveGetTag-chord))
                (set! choice #f))
             ((delete)
                (set! choice #f)
                (d-DirectiveDelete-chord     (d-DirectiveGetTag-chord)))
            ((edit) (set! replace choice)))))
     (if choice       
        (let ((answer1 (d-GetUserInput "Insert Lilypond" "Give Lilypond text to insert before chord" currentpre))
        (answer2 (d-GetUserInput "Insert Lilypond" "Give Lilypond text to insert after chord" currentpost)))
            (if (or answer1 answer2)
                  (let ((tag  (d-GetChecksum (string-append answer1 answer2))))
                    (if replace 
                        (set! tag (d-DirectiveGetTag-chord)))
                     (if answer1 (d-DirectivePut-chord-prefix tag answer1))
                     (if answer2 (d-DirectivePut-chord-postfix tag answer2))
                     (d-DirectivePut-chord-display tag (string-append answer1 "|" answer2))
                     (d-SetSaved #f))))))
                                        
