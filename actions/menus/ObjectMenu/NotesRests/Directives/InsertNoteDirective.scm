;;;InsertNoteDirective
(let ((params InsertNoteDirective::params)
	(replace #f)(choice 'new)
	(currentpost (d-DirectiveGet-note-postfix (d-DirectiveGetTag-note)))
	(currentpre (d-DirectiveGet-note-prefix (d-DirectiveGetTag-note))))
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
                (d-DirectiveTextEdit-note (d-DirectiveGetTag-note))
                (set! choice #f))
             ((delete)
                (set! choice #f)
             	(d-DirectiveDelete-note     (d-DirectiveGetTag-note)))
            ((edit) (set! replace choice)))))
     (if choice       
        (let ((answer1 (d-GetUserInput "Insert Lilypond" "Give Lilypond text to insert before note" currentpre))
        (answer2 (d-GetUserInput "Insert Lilypond" "Give Lilypond text to insert after note" currentpost)))
            (if (or answer1 answer2)
                  (let ((tag  (d-GetChecksum (string-append answer1 answer2))))
                    (if replace 
                    	(set! tag (d-DirectiveGetTag-note)))
                     (if answer1 (d-DirectivePut-note-prefix tag answer1))
                     (if answer2 (d-DirectivePut-note-postfix tag answer2))
                     (d-DirectivePut-note-display tag (string-append answer1 "|" answer2))
                     (d-SetSaved #f))))))
                      	          		
