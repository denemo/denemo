;;DisplaceHorizontally
(let*    ((tag "DisplaceHorizontally") (X (d-DirectiveGet-standalone-data tag)))
    (if (not X)
     (set! X "0.5"))
    (set! X (d-GetUserInput (_ "Horizontal shift") (_ "Give horizontal shift required") X))
    (if  X 
       (begin
        (StandAloneDirectiveProto (cons tag (string-append  "\\once \\override NoteColumn.X-offset = #" X " "  )) #f "\n⬌\nDenemo\n24" X #f X )
        (d-DirectivePut-standalone-ty tag -20)
        (d-MoveCursorRight)
        (d-SetSaved #f)
        (d-RefreshDisplay))))
