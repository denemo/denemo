;;;InsertStandaloneDirective
(let ((replace #f)(choice 'new)(current (d-DirectiveGet-standalone-postfix (d-DirectiveGetTag-standalone))))
    (if current
      (begin
        (set! choice (RadioBoxMenu
                     (cons (_ "New LilyPond Directive")   'new)   
                     (cons (string-append (_ "Edit ") current) 'edit)
                     (cons (_ "Advanced") 'advanced)))               
        (case choice
            ((advanced) 
                (d-DirectiveTextEdit-standalone (d-DirectiveGetTag-standalone))
                (set! choice #f))
            ((edit) (set! replace current)))))
     (if choice       
        (let ((answer (d-GetUserInput "Insert Lilypond" "Give Lilypond text to insert" (if current current ""))))
            (if (and answer (not (string=? answer "")))
                (begin
                    (if replace 
                        (d-DirectivePut-standalone-postfix (d-DirectiveGetTag-standalone) (string-append answer " "))
                        (StandAloneDirectiveProto (cons (d-GetChecksum answer) (string-append answer " ")))))))))
